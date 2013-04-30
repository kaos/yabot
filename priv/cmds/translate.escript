#!/usr/bin/env escript
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
%% -*- mode: erlang -*-

main(Args) ->
    Opts = parse_args(Args, []),
    Result = run(Opts),
    io:format("~s", [Result]),
    ok.

run(Opts) ->
    From = proplists:get_value(from, Opts, "auto"),
    To = proplists:get_value(to, Opts, "en"),
    Text = case proplists:get_value(text, Opts) of
               undefined ->
                   lists:flatten(
                     read_stdin()
                    );
               T -> T
           end,
    URL = url(From, To, Text),
    Body = request(URL),
    grab_result(Body).

parse_args([], Opts) -> Opts;
parse_args(["to", Lang|Args], Opts) ->
    parse_args(Args, [{to, Lang}|Opts]);
parse_args(["from", Lang|Args], Opts) -> 
    parse_args(Args, [{from, Lang}|Opts]);
parse_args(Words, Opts) -> 
    [{text, string:join(Words, " ")}|Opts].

read_stdin() -> 
    lists:reverse(
      read_stdin(io:get_line(""), [])
     ).
read_stdin({error, _}, Acc) -> Acc;
read_stdin(eof, Acc) -> Acc;
read_stdin(Data, Acc) -> read_stdin(io:get_line(""), [Data--"\r\n"|Acc]).

url(_, _, undefined) ->
    undefined;
url(From, To, Text) ->
    io_lib:format(
      "http://translate.google.com/translate_t?langpair=~s/~s&text=~s&hl=~s",
      [http_uri:encode(Arg) || Arg <- [From, To, Text, To]]).

request(undefined) -> "Bad args";
request(URL) ->
    ok = inets:start(),
    {ok, {_, _, Body}} = httpc:request(URL),
    Body.

grab_result(Body) ->
    case re:run(
           Body,
           "TRANSLATED_TEXT='([^']+)'",
           [{capture,[1],list}])
    of
        {match, Result} ->
            string:join(Result, " ");
        nomatch ->
            []
    end.
