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
            bridges=yabot:list_opt(bridges, Options)
           }
    }.

handle_call({handle_message, Message}, _From, State) ->
    yabot:bridge_message(Message, State#state.bridges),
    {reply, ok, State};
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
