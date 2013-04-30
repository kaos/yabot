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

-record(cmd, {
          name,
          exec,
          args,
          synopsis,
          help
         }).

-record(state, {
          nick,
          cmds=[],
          filters=[],
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
            cmds=parse_cmds(proplists:get_value(cmds, Options, [])),
            filters=yabot:list_opt(filters, Options),
            bridges=yabot:list_opt(bridges, Options)
           }
    }.

handle_call({handle_message, Message}, _From, State) ->
    Msg = filter_message(Message, State#state.filters),
    Replies = yabot:bridge_message(Msg, State#state.bridges),
    {Reply, State1} = process_message(Msg, State),
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

filter_message(Message, []) -> Message;
filter_message(#yabot_msg{ message=M }=Msg, Filters) 
  when is_binary(M) ->
    filter_message(Msg#yabot_msg{ message=binary_to_list(M) }, Filters);
filter_message(Message, Filters) ->
    lists:foldl(
      fun (Filter, #yabot_msg{ message=Msg }=M) ->
              if is_function(Filter) ->
                      M#yabot_msg{ message=Filter(Msg) };
                 is_list(Filter) ->
                      M#yabot_msg{ 
                        message=
                            os:cmd(
                              [Filter, " <<EOF-FILTER\n",
                               Msg, "\nEOF-FILTER\n"])}
              end
      end,
      Message,
      Filters).

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
                {cmd_to_atom(Command), Args}
            catch 
                error:badarg ->
                    io_lib:format(
                      "Huh?! I don't know what you are talking about~s. (~s)",
                      [adress(Sender), Command])
            end;
        _ -> []
    end.

cmd_to_atom(Command) ->
    list_to_existing_atom(
      lists:foldl(
        fun(C, S) ->
                string:strip(S, both, C)
        end,
        string:to_lower(Command),
        [$,,$:,$!,$-,$/]
       )).

list_synopsis(#state{ cmds=[] }) -> [];
list_synopsis(#state{ cmds=Cmds }) -> 
    "I also respond to these commands:\r\n"
        ++ string:join([S || #cmd{ synopsis=S } <- Cmds], ", ").

reply(Message, State) ->    
    {#yabot_msg{ message=Message }, State}.

process_command(Message, State) when is_list(Message) ->
    %% failed to parse, so we got the reply already
    reply(Message, State);
process_command({echo, Args}, State) ->
    reply(string:join(Args, " "), State);
process_command({hi, _}, State) ->
    reply("Hi!", State);
process_command({help, []}, State) ->
    reply(
      "Currently, I don't do much, but you can try 'echo some message...' and see what you get ;-P\r\n"
      ++ "- Altough, 'help nick' will tell you what I listen to in the chat rooms.\r\n"
      ++ "- (in case it is different from my \"real\" nick,\r\n"
      ++ "-  or maybe there's more of me under different nicks).\r\n"
      ++ list_synopsis(State),
      State);
process_command({help, [Cmd|_]}, State) ->
    Response = try
                   Command = cmd_to_atom(Cmd), 
                   case Command of
                       nick -> io_lib:format("I listen to the name ~s.", [State#state.nick]);
                       echo -> "Ok, ok. Nothing much to say about echo, really.";
                       _ -> 
                           case lists:keyfind(
                                  Command,
                                  #cmd.name, 
                                  State#state.cmds) 
                           of
                               false -> error(badarg);
                               #cmd{ help=Help } -> Help
                           end
                   end
               catch
                   error:badarg ->
                       io_lib:format("Sorry, I don't know anything about ~s.", [Cmd])
               end,
    reply(Response, State);
process_command({Cmd, Args}, #state{ cmds=Cmds }=State) ->
    case lists:keyfind(Cmd, #cmd.name, Cmds) of
        false ->
            reply(
              io_lib:format(
                "Uh-oh, I don't know what to do with '~s'.",
                [Cmd]),
              State);
        CmdDef ->
            run_cmd(CmdDef, Args, State)
    end.

parse_cmds(Cmds) ->
    [parse_cmd(Cmd) || Cmd <- Cmds].

parse_cmd({exec, Name, Cmd, Args, Synopsis, Help}) ->
    #cmd{
       name=Name,
       exec=Cmd,
       args=Args,
       synopsis=Synopsis,
       help=Help
      }.

run_cmd(#cmd{ exec=Exec, args=Spec }=Cmd, Args, State) ->
    reply(
      case args_ok(Spec, length(Args)) of
          ok ->
              os:cmd(cmd_string(Exec, Args));
          Oops ->
              io_lib:format("~s~nusage: ~s", [Oops, Cmd#cmd.synopsis])
      end,
      State).

args_ok({Min, Max}, Num) 
  when Min =< Num, Max >= Num ->
    ok;
args_ok(Count, Num) when Count == Num ->
    ok;
args_ok({Min, Max}, Num) ->
    io_lib:format("expected ~b-~b args, got ~b.", [Min, Max, Num]);
args_ok(Count, Num) ->
    io_lib:format("expected ~b arg~s, got ~b.", [Count, pl(Count), Num]).

pl(1) -> "";
pl(_) -> "s".

cmd_string(Exec, Args) ->
    string:join(
      [Exec|[os_escape(Arg) || Arg <- [Args]]],
      " "
     ).

os_escape([]) ->
    [];
os_escape(Arg) ->
    os_escape(Arg, [$"]).

os_escape([], Acc) ->
    lists:reverse([$"|Acc]);
os_escape([$"|Arg], Acc) ->
    os_escape(Arg, [$",$\\|Acc]);
os_escape([C|Arg], Acc) ->
    os_escape(Arg, [C|Acc]).
