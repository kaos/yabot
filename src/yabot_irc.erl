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
-module(yabot_irc).

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
         handle_message/2
        ]).


%% irc specific api
-export([join_channel/2, leave_channel/2]).

-include("yabot.hrl").
-include_lib("eircc/include/eircc.hrl").

-define(SERVER, ?MODULE). 

-record(state, {
          irc,
          bridges=[]
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

handle_message(Ref, Message) ->
    gen_server:call(Ref, {handle_message, Message}).


%%%===================================================================
%%% irc api
%%%===================================================================

join_channel(Ref, Channel) when is_pid(Ref) ->
    gen_server:call(Ref, {join_channel, Channel});
join_channel(Id, Channel) ->
    yabot_sup:client_req(Id, join_channel, [Channel]).

leave_channel(Ref, Channel) when is_pid(Ref) ->
    gen_server:call(Ref, {leave_channel, Channel});
leave_channel(Id, Channel) ->
    yabot_sup:client_req(Id, leave_channel, [Channel]).


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
    application:start(eircc),
    {ok, Irc} = eircc_sup:start_client(Options),
    {ok, #state{ 
            irc=Irc,
            bridges=yabot:list_opt(bridges, Options)
           }
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
handle_call({handle_message, Message}, _From, State) ->
    State1 = case Message#yabot_msg.channel of
                 undefined -> State;
                 _ -> send(yabot:message_to_list(Message), State)
             end,
    {reply, [], State1};
handle_call({join_channel, _Channel}, _From, State) ->
    {reply, ok, State};
handle_call({leave_channel, _Channel}, _From, State) ->
    {reply, ok, State};
handle_call({add_bridge, Peer}, _From, #state{ bridges=Peers }=State) ->
    {reply, ok, State#state{ bridges=[Peer|Peers]}};
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
handle_info(#eircc{ channel=undefined, from=Sender, message=Message}, #state{ bridges=Bridges }=State) ->
    io:format("~p got private message from ~p: ~p~n", [?MODULE, Sender, Message]),
    Replies = yabot:bridge_message(#yabot_msg{ from=Sender, message=Message }, Bridges),
    send_replies(Replies, Sender, State),
    {noreply, State};
handle_info(#eircc{ channel=Channel, from=Sender, message=Message}, #state{ bridges=Bridges }=State) ->
    M = #yabot_msg{ channel=Channel, from=Sender, message=normalize_msg(Message) },
    io:format("~p got message from ~s/~s: ~p~n", [?MODULE, Channel, Sender, M#yabot_msg.message]),
    Replies = yabot:bridge_message(M, Bridges),
    send_replies(Replies, undefined, State),
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
    eircc_sup:stop_client(State#state.irc).

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

send(Message, State) ->
    eircc_sup:send_message(State#state.irc, Message),
    State.

normalize_msg("ACTION " ++ Action) ->
    "/me " ++ Action -- "$";
normalize_msg(Msg) ->
    Msg.

send_replies([], _To, _State) -> nop;
send_replies(Replies, undefined, State) ->
    [send(yabot:message_to_list(Reply), State) || Reply <- Replies];
send_replies(Replies, To, State) ->
    [send({say, To, yabot:message_to_list(Reply)}, State) || Reply <- Replies].
