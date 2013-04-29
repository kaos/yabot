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
-module(yabot).

-include("yabot.hrl").

%% API
-export([
         start/0, stop/0,
         list_opt/2,
         bridge_message/2,
         message_to_list/1
        ]).


%%%===================================================================
%%% API
%%%===================================================================

start() ->
    application:start(yabot).

stop() ->
    application:stop(yabot).

list_opt(Key, Opts) ->
    case proplists:get_value(Key, Opts, []) of
        List when is_list(List) ->
            List;
        Value ->
            [Value]
    end.

bridge_message(_, []) -> [];
bridge_message(#yabot_msg{}=Msg, Dsts) ->
    lists:flatten(
      [yabot_client:handle_message(Id, Msg) 
       || Id <- Dsts]
     ).
    
message_to_list(#yabot_msg{ from=undefined, message=Message }) ->
    Message;
message_to_list(#yabot_msg{ from=Nick, message=Message }) ->
    format_message(Nick, Message).

%%%===================================================================
%%% Internal functions
%%%===================================================================

format_message(Nick, "/me " ++ Message) ->
    io_lib:format("/me [~s] ~s", [Nick, Message]);
format_message(Nick, <<"/me ", Message/binary>>) ->
    io_lib:format("/me [~s] ~s", [Nick, Message]);
format_message(Nick, Message) ->
    io_lib:format("[~s] ~s", [Nick, Message]).
