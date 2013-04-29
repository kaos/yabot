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
-module(yabot_bot).

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

-include("yabot.hrl").

-define(SERVER, ?MODULE). 

-record(state, {
          nick,
          bridges=[]
         }).


%%%===================================================================
%%% API
%%%===================================================================

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
%%% 
%%%===================================================================


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Options) ->
    {ok, #state{
            nick=proplists:get_value(nick, Options, "yabot"),
            bridges=yabot:list_opt(bridges, Options)
           }
    }.

handle_call({handle_message, Message}, _From, State) ->
    Replies = yabot:bridge_message(Message, State#state.bridges),
    {Reply, State1} = process_message(Message, State),
    {reply, [Reply|Replies], State1};
handle_call({add_bridge, Peer}, _From, #state{ bridges=Peers }=State) ->
    {reply, ok, State#state{ bridges=[Peer|Peers]}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
    
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    io:format("~p got: ~p~n", [?MODULE, _Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

process_message(#yabot_msg{ channel=undefined, message=Message }, State) ->
    process_command(parse(Message, undefined), State);
process_message(#yabot_msg{ from=Sender, message=Message }, #state{ nick=Me }=State) 
  when is_list(Message) ->
    case string:str(Message, Me) of
        1 ->
            process_command(
              parse(
                string:substr(Message, 1 + string:len(Me)), 
                Sender),
              State
             );
        _ ->
            {[], State}
    end;
process_message(#yabot_msg{ message=Message }=Msg, State)
  when is_binary(Message) ->
    process_message(Msg#yabot_msg{ message=binary_to_list(Message) }, State);
process_message(_, State) ->
    {[], State}.

adress(undefined) -> [];
adress(Who) -> 
    io_lib:format(", ~s", [Who]).

parse(Message, Sender) when is_binary(Message) ->
    parse(binary_to_list(Message), Sender);
parse([$,|Message], Sender) ->
    parse(Message, Sender);
parse([$:|Message], Sender) ->
    parse(Message, Sender);
parse(Message, Sender) ->
    case string:tokens(Message, " ") of
        [Command|Args] ->
            try
                {list_to_existing_atom(Command), Args}
            catch 
                error:badarg ->
                    io_lib:format(
                      "Huh?! I don't know what you are talking about~s. (~s)",
                      [adress(Sender), Command])
            end;
        _ -> []
    end.

reply(Message, State) ->    
    {#yabot_msg{ message=Message }, State}.
    
process_command(Message, State) when is_list(Message) ->
    %% failed to parse, so we got the reply already
    reply(Message, State);
process_command({echo, Args}, State) ->
    reply(string:join(Args, " "), State);
process_command({help, Args}, State) ->
    Response = case Args of
                   [] -> "Currently, I don't do much, but you can try 'echo some message...' and see what you get ;-P\r\n"
                             ++ "- Altough, 'help nick' will tell you what I listen to in the chat rooms.\r\n"
                             ++ "- (in case it is different from my \"real\" nick,\r\n"
                             ++ "-  or maybe there's more of me under different nicks).";
                   ["nick"|_] -> io_lib:format("I listen to the name ~s.", [State#state.nick]);
                   ["echo"|_] -> "Ok, ok. Nothing much to say about echo, really.";
                   [Cmd|_] -> io_lib:format("Sorry, I don't know anything about ~s.", [Cmd])
               end,
    reply(Response, State);
process_command({Cmd, _Args}, State) ->
    reply(
      io_lib:format(
        "Uh-oh, I don't know what to do with '~s'.",
       [Cmd]),
     State).
