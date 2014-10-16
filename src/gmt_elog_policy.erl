%%%----------------------------------------------------------------------
%%% Copyright (c) 2009-2014 Hibari developers.  All rights reserved.
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
%%% File    : gmt_elog_policy.erl
%%% Purpose : GMT event logging policy module
%%%----------------------------------------------------------------------

%%%
%%% @doc A lightweight event logging policy implementation.
%%%
%%% The emphasis of this module is <em>lightweight</em>.  The
%%% enabled() function may be called tens of thousands of times per
%%% second.
%%%
%%% The use of the 4 arguments to enabled can be free-form, since this
%%% entire module will be replaced whenever tracing is desired.
%%% However, it's strongly recommended that the following conventions
%%% be used:
%%%
%%% <ul>
%%% <li> <b>Level</b>: emergency | alert | critical | error | warning
%%%      | notice | info | debug | trace </li>
%%% <li> <b>Category</b>: term() </li>
%%% <li> <b>Module</b>: module() </li>
%%% <li> <b>Line</b>: integer() </li>
%%% </ul>
%%%

-module(gmt_elog_policy).

-compile({inline_size,100}). % 24 is the default

%% API
-export([dtrace/6, dtrace_support/0]).

%% Micro benchmarking
-export([test_c/6, test_e/6, test_e_setup/0]).

-define(DTRACE_SUPPORT, '**GMTUtil-DtraceSupport**').

%%%===================================================================
%%% API
%%%===================================================================

-type log_level() :: 'emergency' | 'alert' | 'critical' | 'error' | 'warning' | 'notice' | 'info' | 'debug' | 'trace'.

%%--------------------------------------------------------------------
%% @doc Fixture for event tracing. Return true if event was published
%% to dyntrace.
%%
%% args in D script:
%% - arg1: pid :: string
%% - arg2: Caterory :: int
%% - arg6: LogLevel :: string
%% - arg7: Module :: string
%% - arg3: Line :: int
%% - arg8: Message :: string
%%--------------------------------------------------------------------

-spec dtrace(log_level(), integer() | undefined, module(), integer(), string(), [term()]) ->
                    true | false | error | badarg.
dtrace(Level, Category, Module, Line, Fmt, Args) ->
    case dtrace_support() of
        disabled ->
            false;
        unsupported ->
            false;
        dyntrace ->
            Level0 = erlang:atom_to_list(Level),
            Category0 = if is_integer(Category) -> Category;
                           true                 -> 0
                        end,
            Module0 = erlang:atom_to_list(Module),
            Message = if Args =:= [] -> Fmt;
                         true        -> catch io_lib:format(Fmt, Args)
                      end,
            dyntrace:p(Category0, Line, Level0, Module0, Message)
    end.

-spec dtrace_support() -> dyntrace | disabled | unsupported.
dtrace_support() ->
    case get(?DTRACE_SUPPORT) of
        undefined ->
            case application:get_env(gmt_util, dtrace_support) of
                {ok, true} ->
                    case string:to_float(erlang:system_info(version)) of
                        {Num, _} when Num > 5.8 ->
                            %% R15B or higher
                            try dyntrace:available() of
                                true ->
                                    put(?DTRACE_SUPPORT, dyntrace);
                                false ->
                                    put(?DTRACE_SUPPORT, unsupported)
                            catch
                                _:_ ->
                                    put(?DTRACE_SUPPORT, unsupported)
                            end;
                        _ ->
                            put(?DTRACE_SUPPORT, unsupported)
                    end;
                _ ->
                    put(?DTRACE_SUPPORT, disabled)
            end,
            get(?DTRACE_SUPPORT);
        DTraceSupport ->
            DTraceSupport
    end.


%%
%% Functions for micro benchmarking
%%

%% (Result from before July 2010)
%% A laptop, CPU clock fixed at 1.33GHz, non-SMP VM, says:
%%
%% timer:tc(gmt_elog, test_iter, [0, 88999000])    -> {12925467,ok}
%% timer:tc(gmt_elog, test_iter_c, [0, 88999000])  -> {10012760,ok}
%% timer:tc(gmt_elog, test_iter_e, [0, 88999000])  -> {43373826,ok}
%%
%% The enabled() func is 3.4x faster than test_e() when test_e()'s
%% public table exists and contains the single tuple {test_e, 5}.
%%
%% If the public named table does not exist, test_e() is 25.6x slower
%% than enabled()!
%% If the public named table exists and is empty, test_e() is 2.2x slower
%% than enabled().

test_c(_Priority, _Category, _Module, _Line, _Fmt, _ArgList) ->
    false.

test_e(Priority, _Category, _Module, _Line, _Fmt, _ArgList) ->
    case (catch ets:lookup(goofus, test_e)) of
        [{test_e, Limit}] ->
            Priority =< Limit;
        _ ->
            false
    end.

test_e_setup() ->
    spawn(fun() ->
                  ets:new(goofus, [public, named_table]),
                  receive goofus -> goofus end
          end).
