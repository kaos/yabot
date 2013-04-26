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
-module(yabot_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         start_client/2,
         start_client/3,
         add_bridge/2,
         send_message/2,
         client_req/3
        ]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_client(Mod, Options) ->
    start_client(Mod, Mod, Options).

start_client(Id, Mod, Options) ->
    supervisor:start_child(
      ?MODULE,
      {Id, {Mod, start_link, [Options]}, transient, 5000, worker, [Mod]}
     ).

add_bridge(Src, Dst) ->
    client_req(Src, add_bridge, [Dst]).

send_message(Id, Message) ->
    client_req(Id, send_message, [Message]).

client_req(Id, Fun, Args) ->
    case client(Id) of
        {Id, Pid, _, [Mod|_]} ->
            erlang:apply(Mod, Fun, [Pid|Args])
    end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 1, 10}, []} }.

client(Id) ->
    case lists:keyfind(
           Id, 1, 
           supervisor:which_children(?MODULE)) of
        false ->
            {client_not_found, Id};
        Client ->
            Client
    end.
