%%%-------------------------------------------------------------------
%%% Copyright (c) 2009-2015 Hibari developers.  All rights reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% File    : gmt_eqc.erl
%%% Purpose : Wrapper for eqc.erl
%%%-------------------------------------------------------------------

-module(gmt_eqc).

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-define(GMTQC, proper).
-define(GMTQC_GEN, proper_gen).
-undef(EQC).
-endif. %% -ifdef(PROPER).

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-define(GMTQC, eqc).
-define(GMTQC_GEN, eqc_gen).
-undef(PROPER).
-endif. %% -ifdef(EQC).

-ifdef(GMTQC).
-include_lib("eunit/include/eunit.hrl").

-export([module/2]).
-export([start/0]).
-export([silent/1]).
-export([write_counterexamples/1, write_counterexamples/2, write_counterexamples/3]).
-export([write_counterexample/3, write_counterexample/4]).
-export([eunit_module/1, eunit_module/2, eunit_module/3, eunit_module/4]).

-ifdef(PROPER).
%% @doc PropER has a different API than EQC
module(Options, Module) ->
    proper:module(Module, Options).

%% @doc PropER doesn't have a server.  Always return true.
start() ->
    true.
-endif. %% -ifdef(PROPER).

-ifdef(EQC).
%% @doc Same API as EQC
module(Options, Module) ->
    eqc:module(Options, Module).

%% @doc Starts (and possibly restarts) the QuickCheck server. If
%% another instance is not running, start the server and return the
%% server's process id.  If another instance is already running,
%% return true.  Otherwise, forcefully restart the server.
start() ->
    try
        eqc:start(false)
    catch
        exit:already_running ->
            true;
        _X:_Y ->
            eqc:start(true)
    end.
-endif. %% -ifdef(EQC).

%% @doc Disable QuickCheck's test output (i.e. the "dots")
silent(Prop) ->
    on_output(silent_printer(), Prop).

silent_printer() ->
    Filter = fun($.) -> false; (_) -> true end,
    fun(Fmt, Args) -> io:format(lists:filter(Filter,Fmt), Args), ok end.

%% @doc Write failing counterexamples for specified Module
write_counterexamples(Module) ->
    write_counterexamples(Module, ?GMTQC:counterexamples()).

write_counterexamples(Module, CounterExamples) ->
    write_counterexamples(Module, CounterExamples, calendar:local_time()).

-ifdef(PROPER).
write_counterexamples(Module, CounterExamples, LocalTime) ->
    [ write_counterexample(Module, Prop, CE, LocalTime) || {{_Mod,Prop,_Arity}, CE} <- CounterExamples ].
-endif. %% -ifdef(PROPER).

-ifdef(EQC).
write_counterexamples(Module, CounterExamples, LocalTime) ->
    [ write_counterexample(Module, Prop, CE, LocalTime) || {Prop, CE} <- CounterExamples ].
-endif. %% -ifdef(EQC).

write_counterexample(Module, Prop, CounterExample) ->
    write_counterexample(Module, Prop, CounterExample, calendar:local_time()).

write_counterexample(Module, Prop, CounterExample, {{Year,Month,Day},{Hour,Minute,Second}}) ->
    Fmt = "~s:~s-~4..0B~2..0B~2..0B-~2..0B~2..0B~2..0B.erl",
    Args = [Module,Prop,Year,Month,Day,Hour,Minute,Second],
    FileName = lists:flatten(io_lib:format(Fmt, Args)),
    ok = file:write_file(FileName, io_lib:format("~p.", [CounterExample])),
    FileName.

%% @doc Wrap module as an EUnit test fixture
eunit_module(Module) ->
    eunit_module(Module, 3000).

eunit_module(Module, NumTests) ->
    eunit_module(Module, NumTests, 60).

eunit_module(Module, NumTests, Timeout) ->
    eunit_module(Module, NumTests, Timeout, fun() -> noop end).

eunit_module(Module, NumTests, Timeout, Teardown) ->
    {setup, local, fun() -> eunit_setup(Module) end
     , fun(Mod) -> Teardown(), eunit_teardown(Mod) end
     , {timeout, Timeout, [fun() -> eunit_run(Module, NumTests) end]}
    }.

eunit_setup(Module) ->
    start(),
    Module.

eunit_teardown(Module) ->
    ?assertEqual([], write_counterexamples(Module)).

-ifdef(PROPER).
eunit_run(Module, NumTests) ->
    erlang:group_leader(whereis(user), self()),
    module([{numtests,NumTests}, noshrink, {on_output,silent_printer()}], Module).
-endif. %% -ifdef(PROPER).

-ifdef(EQC).
eunit_run(Module, NumTests) ->
    erlang:group_leader(whereis(user), self()),
    module([{numtests,NumTests}, fun ?GMTQC_GEN:noshrink/1, {on_output,silent_printer()}], Module).
-endif. %% -ifdef(EQC).

-endif. %% -ifdef(GMTQC).
