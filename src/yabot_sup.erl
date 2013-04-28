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
         client_req/3
        ]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    Clients = case application:get_env(yabot, clients) of
                  {ok, C} when is_list(C) -> C;
                  _ -> []
              end,
    supervisor:start_link({local, ?MODULE}, ?MODULE, Clients).

start_client(Mod, Options) ->
    start_client(Mod, Mod, Options).

start_client(Id, Mod, Options) ->
    supervisor:start_child(
      ?MODULE, child_spec(Id, Mod, Options)
     ).

client_req(Id, Fun, Args) ->
    case client(Id) of
        {Id, Pid, _, [Mod|_]} ->
            erlang:apply(Mod, Fun, [Pid|Args]);
        Error -> {error, Error}
    end.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Clients) ->
    {ok, { 
       {one_for_one, 1, 10}, 
       [child_spec(Id, Mod, Options) || {Id, Mod, Options} <- Clients]
      } 
    }.


%% ===================================================================
%% Internal functions
%% ===================================================================

client(Id) ->
    case lists:keyfind(
           Id, 1, 
           supervisor:which_children(?MODULE)) of
        false ->
            {client_not_found, Id};
        Client ->
            Client
    end.

child_spec(Id, Mod, Options) ->
    {Id, {Mod, start_link, [Options]}, transient, 5000, worker, [Mod]}.
