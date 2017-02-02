%%%-------------------------------------------------------------------
%%% Copyright (c) 2010-2017 Hibari developers.  All rights reserved.
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
%%% File    : gmt_eqc_statem.erl
%%% Purpose : Wrapper for eqc_statem.erl
%%%-------------------------------------------------------------------

-module(gmt_eqc_statem).

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-define(GMTQC, proper).
-define(GMTQC_GEN, proper_gen).
-undef(EQC).
-define(ALWAYS(_N,PROP), PROP).
-endif. %% -ifdef(PROPER).

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-define(GMTQC, eqc).
-define(GMTQC_GEN, eqc_gen).
-undef(PROPER).
-endif. %% -ifdef(EQC).

-ifdef(GMTQC).

%% API
-export([gmt_sample_commands/1, gmt_sample_commands/2]).
-export([gmt_run_commands/1, gmt_run_commands/2]).
-export([gmt_gen_command/2]).

%% eqc_statem Callbacks
-export([command/1, initial_state/0, initial_state/1, next_state/3, precondition/2, postcondition/3]).

%% Interface Functions
-export([behaviour_info/1]).

%% Define the behaviour's required mods.
behaviour_info(callbacks) ->
    [{command_gen,2}
     , {initial_state,0}
     , {state_is_sane,1}
     , {next_state,3}
     , {precondition,2}
     , {postcondition,3}
     , {commands_setup,1}
     , {commands_teardown,1}
     , {commands_teardown,2}
    ].

%%%----------------------------------------------------------------------
%%% records
%%%----------------------------------------------------------------------

%%%%%%
%% state
-record(state, {mod, mod_state}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

gmt_sample_commands(Mod) ->
    gmt_sample_commands(Mod, []).

gmt_sample_commands(Mod, Options)
  when is_atom(Mod), is_list(Options) ->
    %% commands - sample
    Params = [{mod,Mod},{options,Options}],
    ?GMTQC_GEN:sample(with_parameters(Params,
                                      ?LET(InitialState,initial_state(Mod),
                                           command(InitialState)))).

gmt_run_commands(Mod) ->
    gmt_run_commands(Mod, []).

gmt_run_commands(Mod, Options)
  when is_atom(Mod), is_list(Options) ->
    %% commands - setup and teardown
    {ok,TestRefOnce} = Mod:commands_setup(true),
    ok = Mod:commands_teardown(TestRefOnce),

    %% commands - loop
    Parallel = proplists:get_bool(parallel, Options),
    Params = [{parallel,Parallel},{mod,Mod},{options,Options}],
    case Parallel of
        false ->
            ?FORALL(Cmds,with_parameters(Params,
                                         ?LET(InitialState,initial_state(Mod),
                                              more_commands(3,commands(?MODULE,InitialState)))),
                    begin
                        %% commands - setup
                        {ok,TestRef} = Mod:commands_setup(false),

                        %% commands - run
                        {H,S,Res} = run_commands(?MODULE,Cmds,Params),

                        %% whenfail
                        ?WHENFAIL(
                           begin
                               %% commands
                               FileName = write_commands(Cmds),
                               io:format("~nCOMMANDS:~n\t~p~n",[FileName]),
                               %% history
                               io:format("~nHISTORY:"),
                               if
                                   length(H) < 1 ->
                                       io:format(" none~n");
                                   true ->
                                       CmdsH = zip(tl(Cmds),H),
                                       [ begin
                                             {Cmd,{State,Reply}} = lists:nth(N,CmdsH),
                                             io:format("~n #~p:~n\tCmd: ~p~n\tReply: ~p~n\tState: ~p~n",
                                                       [N,Cmd,Reply,State])
                                         end
                                         || N <- lists:seq(1,length(CmdsH)) ]
                               end,
                               %% result
                               io:format("~nRESULT:~n\t~p~n",[Res]),
                               %% state
                               io:format("~nSTATE:~n\t~p~n",[S]),
                               %% state is sane
                               io:format("~nSTATE IS SANE:~n\t~p~n",[state_is_sane(Mod, S)])
                           end,
                           aggregate(command_names(Cmds),
                                     (ok =:= Res
                                      andalso state_is_sane(Mod, S)
                                      %% commands - teardown
                                      andalso ok =:= Mod:commands_teardown(TestRef,S#state.mod_state))))
                    end);
        true ->
            %% Number of attempts to make each test case fail. When
            %% searching for a failing example, we run each test
            %% once. When searching for a way to shrink a test case,
            %% we run each candidate shrinking 100 times.
            ?FORALL(_Attempts,?SHRINK(1,[100]),
                    ?FORALL(Cmds,with_parameters(Params,
                                                 ?LET(InitialState,initial_state(Mod),
                                                      parallel_commands(?MODULE,InitialState))),
                            ?ALWAYS(_Attempts,
                                    ?TIMEOUT(5000,
                                             begin
                                                 %% commands - setup
                                                 {ok,TestRef} = Mod:commands_setup(false),

                                                 %% commands - run
                                                 {H,HL,Res} = run_parallel_commands(?MODULE,Cmds,Params),

                                                 %% whenfail
                                                 ?WHENFAIL(
                                                    begin
                                                        %% commands
                                                        FileName = write_commands(Cmds),
                                                        io:format("~nCOMMANDS:~n\t~p~n",[FileName]),
                                                        %% history
                                                        io:format("~nHISTORY:~n\t~p~n",[H]),
                                                        %% history list
                                                        io:format("~nHISTORY LIST:~n\t~p~n",[HL]),
                                                        %% result
                                                        io:format("~nRESULT:~n\t~p~n",[Res])
                                                    end,
                                                    aggregate(command_names(Cmds),
                                                              (ok =:= Res
                                                               %% commands - teardown
                                                               andalso ok =:= Mod:commands_teardown(TestRef,undefined))))
                                             end))))
    end;
gmt_run_commands(_Mod, _Options) ->
    exit(badarg).

gmt_gen_command(Mod, ModState) ->
    Mod:command_gen(Mod, ModState).


%%%----------------------------------------------------------------------
%%% Callbacks - eqc_statem
%%%----------------------------------------------------------------------

%% initial state
initial_state() ->
    #state{}.

initial_state(Mod) ->
    #state{mod=Mod, mod_state=Mod:initial_state()}.

%% state is sane
state_is_sane(Mod, S) ->
    Mod:state_is_sane(S#state.mod_state).

%% command generator
command(S)
  when is_record(S,state) ->
    (S#state.mod):command_gen(S#state.mod, S#state.mod_state).

%% next state
next_state(S,R,C) ->
    NewModState = (S#state.mod):next_state(S#state.mod_state,R,C),
    S#state{mod_state=NewModState}.

%% precondition
precondition(S,C) ->
    (S#state.mod):precondition(S#state.mod_state,C).

%% postcondition
postcondition(S,C,R) ->
    (S#state.mod):postcondition(S#state.mod_state,C,R).


%%%----------------------------------------------------------------------
%%% Internal
%%%----------------------------------------------------------------------

write_commands(Cmds) ->
    Module = ?MODULE,
    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:local_time(),
    FileName = lists:flatten(io_lib:format("~s-~4..0B~2..0B~2..0B-~2..0B~2..0B~2..0B.erl",
                                           [Module,Year,Month,Day,Hour,Minute,Second])),
    write_commands(Cmds,FileName).

write_commands(Cmds,FileName) ->
    ok = file:write_file(FileName, io_lib:format("~p.", [Cmds])),
    FileName.

-endif. %% -ifdef(GMTQC).
