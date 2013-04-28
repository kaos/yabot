%%%
%% Copyright 2013 Andreas Stenius
%%
%%  Licensed under the Apache License, Version 2.0 (the "License");
%%  you may not use this file except in compliance with the License.
%%  You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%%  Unless required by applicable law or agreed to in writing, software
%%  distributed under the License is distributed on an "AS IS" BASIS,
%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%  See the License for the specific language governing permissions and
%%  limitations under the License.
%%
-module(yabot_xmpp).

-behaviour(gen_server).
-behaviour(yabot_client).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% yabot_client callbacks
-export([
         add_bridge/2,
         send_message/2,
         recv_message/2
]).

%% xmpp specific api
-export([join_room/3, leave_room/2]).

-include("yabot.hrl").
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-define(SERVER, ?MODULE). 

-record(state, {
          session,
          server,
          port,
          room,
          nick,
          bridge=[]
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).


%%%===================================================================
%%% yabot_client callbacks
%%%===================================================================

add_bridge(Ref, Peer) ->
    gen_server:call(Ref, {add_bridge, Peer}).

send_message(Ref, Message) ->
    gen_server:call(Ref, {send_message, Message}).

recv_message(_Ref, _Message) ->
    ok. %gen_server:call(Ref, {recv_message, Message}).


%%%===================================================================
%%% xmpp api
%%%===================================================================

join_room(Ref, Room, Nick) when is_pid(Ref) ->
    gen_server:call(Ref, {join_room, Room, Nick});
join_room(Id, Room, Nick) ->
    yabot_sup:client_req(Id, join_room, [Room, Nick]).

leave_room(Ref, Room) when is_pid(Ref) ->
    gen_server:call(Ref, {leave_room, Room});
leave_room(Id, Room) ->
    yabot_sup:client_req(Id, leave_room, [Room]).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Options) ->
    application:start(exmpp),
    Session = exmpp_session:start_link(),
    [User, Server] = string:tokens(
                       proplists:get_value(jid, Options),
                       "@"),
    Jid = exmpp_jid:make(User, Server, random),
    exmpp_session:auth(
      Session, Jid, 
      proplists:get_value(pass, Options),
      proplists:get_value(method, Options, digest)
     ),
    {ok, #state{ 
            session=Session, 
            server=Server, 
            port=proplists:get_value(port, Options, default),
            room=proplists:get_value(room, Options),
            nick=exmpp_utils:any_to_binary(proplists:get_value(nick, Options, "yabot")),
            bridge=proplists:get_value(bridge, Options, [])
           },
     0
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({send_message, Message}, _From, State) ->
    {
      reply, ok,
      send( 
        exmpp_message:groupchat(Message),
        State#state.room,
        State)
    };
handle_call({join_room, Room, Nick}, _From, #state{ room=Room, nick=Nick }=State) ->
    {reply, ok, State};
handle_call({join_room, Room, Nick}, _From, #state{ room=Room }=State) ->
    {
      reply, ok, 
      send(
        exmpp_presence:presence(available, "Ready"), 
        muc_room(Room, Nick), 
        State#state{ nick=exmpp_utils:any_to_binary(Nick)})
    };
handle_call({join_room, Room, Nick}, _From, #state{ room=OldRoom }=State) ->
    {reply, ok, join(Room, Nick, "Ready", leave(OldRoom, State))};
handle_call({leave_room, Room}, _From, #state{ room=Room }=State) ->
    {reply, ok, leave(Room, State)};
handle_call({add_bridge, Peer}, _From, #state{ bridge=Peers }=State) ->
    {reply, ok, State#state{ bridge=[Peer|Peers]}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    {ok, _StreamID} = 
        case State#state.port of
            default ->
                exmpp_session:connect_TCP(
                  State#state.session,
                  State#state.server
                 );
            Port ->
                exmpp_session:connect_TCP(
                  State#state.session,
                  State#state.server,
                  Port
                 )
        end,
    try
        login(State),       
        {noreply, State}
    catch 
        throw:{auth_error, _}=Error ->
            error_logger:warning_report([{module, ?MODULE}, Error]),
            {stop, normal, State}
    end;
handle_info(#received_packet{ 
               packet_type=Type, 
               type_attr=Attr,
               from=From,
               raw_packet=Packet }, 
            #state{ bridge=Bridge }=State) ->
    Jid = exmpp_jid:make(From),
    io:format("~p received ~p ~s from ~s~n", 
              [?MODULE, Type, Attr, 
               exmpp_jid:to_list(Jid)]),
    %% aaww, this is ugly...
    case Type of
        message ->
            Message=exmpp_message:get_body(Packet),
            io:format("~s~n", [Message]),
            case Message of
                undefined ->
                    io:format("~s~n", [exmpp_xml:document_to_list(Packet)]);
                _ ->
                    Sender = exmpp_jid:resource(Jid),
                    case State#state.nick of
                        Sender -> nop;
                        _ ->
                            yabot_bridge:message(Sender, Message, Bridge)
                    end
            end;
        iq ->
            io:format("~s~n", [exmpp_xml:document_to_list(Packet)]);
        _ ->
            nop
    end,
    {noreply, State};
handle_info(_Info, State) ->
    io:format("~p got: ~p~n", [?MODULE, _Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    exmpp_session:stop(State#state.session).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


login(State) ->
    exmpp_session:login(State#state.session),
    send(exmpp_presence:presence(available, "Ready"), State),
    join(State#state.room, State#state.nick, "Ready", State).

send(Packet, #state{ session=Session }=State) ->
    io:format("~p send stanza~n~s~n", [?MODULE, exmpp_xml:document_to_list(Packet)]),
    exmpp_session:send_packet(
      Session, Packet),
    State.

send(Packet, To, State) ->
    send(
      exmpp_xml:set_attribute(
        Packet, <<"to">>, To),
      State).

join(undefined, _, _, State) -> State;
join(Room, Nick, Status, State) ->
    send(
      exmpp_xml:append_child(
        exmpp_presence:presence(available, Status),
        ?XMLEL4(
           ?NS_MUC, 'x', [],
           [?XMLEL4(
               ?NS_MUC, 'history', 
               [?XMLATTR(<<"maxstanzas">>, 0)], [])])),
      muc_room(Room, Nick),
      State
     ),
    State#state{ room=Room, nick=exmpp_utils:any_to_binary(Nick) }.

leave(undefined, State) -> State;
leave(Room, State) ->
    send(
      exmpp_presence:unavailable(),
      Room, State#state{ room=undefined }).

muc_room(Room, Nick) ->
    Room ++ "/" ++ Nick.
