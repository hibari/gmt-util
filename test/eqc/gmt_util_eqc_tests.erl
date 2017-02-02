%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2017 Hibari developers.  All rights reserved.
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
%%% File    : gmt_util_eqc_tests.erl
%%% Purpose : GMT util QuickCheck tests
%%%-------------------------------------------------------------------

-module(gmt_util_eqc_tests).

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-define(GMTQC, proper).
-undef(EQC).
-endif. %% -ifdef(PROPER).

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-define(GMTQC, eqc).
-undef(PROPER).
-endif. %% -ifdef(EQC).

-ifdef(GMTQC).

-export([run/0]).
-compile(export_all).

%% run from eunit
eunit_test_() ->
    gmt_eqc:eunit_module(?MODULE, 3000).

run() ->
    run(3000).

run(Num) ->
    gmt_eqc:module({numtests,Num}, ?MODULE).

my_string() ->
    list(char()).

%% property to check gmt_util:left_pad() function
%% Checks length and correct padding/strings
prop_left_pad() ->
    ?FORALL({Str, Len, Char}, {my_string(), int(), char()},
            begin
                S = gmt_util:left_pad(Str, Len, Char),
                StrLen = length(Str),
                Res = case Len > StrLen of
                          true ->
                              PrefixLen = Len - StrLen,
                              {HL, TL} = lists:split(PrefixLen, S),
                              length(S) == Len andalso HL == lists:duplicate(PrefixLen, Char) andalso TL == Str;
                          false ->
                              S == Str
                      end,
                ?WHENFAIL(io:format("Str:~w S:~w~n",[Str, S]), Res)
            end).

%% property to check gmt_util:right_pad() function
%% Checks length and correct padding/strings
prop_right_pad() ->
    ?FORALL({Str, Len, Char}, {my_string(), int(), char()},
            begin
                S = gmt_util:right_pad(Str, Len, Char),
                StrLen = length(Str),
                Res = case Len > StrLen of
                          true ->
                              SuffixLen = Len - StrLen,
                              {HL, TL} = lists:split(StrLen, S),
                              length(S) == Len andalso HL == Str andalso TL == lists:duplicate(SuffixLen, Char);
                          false ->
                              S == Str
                      end,
                ?WHENFAIL(io:format("Str:~w S:~w~n",[Str, S]), Res)
            end).

prop_list_unique_u() ->
    ?FORALL(L, list(int()),
            lists:sort(gmt_util:list_unique_u(L)) == lists:usort(L)).

-endif. %% -ifdef(GMTQC).
