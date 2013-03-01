%%%----------------------------------------------------------------------
%%% Copyright (c) 2009-2013 Hibari developers.  All rights reserved.
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
%%% File    : gmt_elog.erl
%%% Purpose : GMT event log
%%%----------------------------------------------------------------------

-ifndef(gmt_elog_hrl).
-define(gmt_elog_hrl, true).

%%
%% Events with a message
%%

-define(ELOG_EMERGENCY(Msg),
        begin
            lager:emergency(Msg),
            gmt_elog_policy:enabled(emergency, undefined, ?MODULE, ?LINE, Msg, [])
        end).
-define(ELOG_ALERT(Msg),
        begin
            lager:alert(Msg),
            gmt_elog_policy:enabled(alert, undefined, ?MODULE, ?LINE, Msg, [])
        end).
-define(ELOG_CRITICAL(Msg),
        begin
            lager:critical(Msg),
            gmt_elog_policy:enabled(critical, undefined, ?MODULE, ?LINE, Msg, [])
        end).
-define(ELOG_ERROR(Msg),
        begin
            lager:error(Msg),
            gmt_elog_policy:enabled(error, undefined, ?MODULE, ?LINE, Msg, [])
        end).
-define(ELOG_WARNING(Msg),
        begin
            lager:warning(Msg),
            gmt_elog_policy:enabled(warning, undefined, ?MODULE, ?LINE, Msg, [])
        end).
-define(ELOG_NOTICE(Msg),
        begin
            lager:notice(Msg),
            gmt_elog_policy:enabled(notice, undefined, ?MODULE, ?LINE, Msg, [])
        end).
-define(ELOG_INFO(Msg),
        begin
            lager:info(Msg),
            gmt_elog_policy:enabled(info, undefined, ?MODULE, ?LINE, Msg, [])
        end).
-define(ELOG_DEBUG(Msg),
        begin
            lager:debug(Msg),
            gmt_elog_policy:enabled(debug, undefined, ?MODULE, ?LINE, Msg, [])
        end).
-define(ELOG_TRACE(Msg),
        gmt_elog_policy:enabled(trace, undefined, ?MODULE, ?LINE, Msg, [])).

%%
%% Events with a format and args
%%

-define(ELOG_EMERGENCY(Fmt, Args),
        begin
            lager:emergency(Fmt, Args),
            gmt_elog_policy:enabled(emergency, undefined, ?MODULE, ?LINE, Fmt, Args)
        end).
-define(ELOG_ALERT(Fmt, Args),
        begin
            lager:alert(Fmt, Args),
            gmt_elog_policy:enabled(alert, undefined, ?MODULE, ?LINE, Fmt, Args)
        end).
-define(ELOG_CRITICAL(Fmt, Args),
        begin
            lager:critical(Fmt, Args),
            gmt_elog_policy:enabled(critical, undefined, ?MODULE, ?LINE, Fmt, Args)
        end).
-define(ELOG_ERROR(Fmt, Args),
        begin
            lager:error(Fmt, Args),
            gmt_elog_policy:enabled(error, undefined, ?MODULE, ?LINE, Fmt, Args)
        end).
-define(ELOG_WARNING(Fmt, Args),
        begin
            lager:warning(Fmt, Args),
            gmt_elog_policy:enabled(warning, undefined, ?MODULE, ?LINE, Fmt, Args)
        end).
-define(ELOG_NOTICE(Fmt, Args),
        begin
            lager:notice(Fmt, Args),
            gmt_elog_policy:enabled(notice, undefined, ?MODULE, ?LINE, Fmt, Args)
        end).
-define(ELOG_INFO(Fmt, Args),
        begin
            lager:info(Fmt, Args),
            gmt_elog_policy:enabled(info, undefined, ?MODULE, ?LINE, Fmt, Args)
        end).
-define(ELOG_DEBUG(Fmt, Args),
        begin
            lager:debug(Fmt, Args),
            gmt_elog_policy:enabled(debug, undefined, ?MODULE, ?LINE, Fmt, Args)
        end).
-define(ELOG_TRACE(Fmt, Args),
        gmt_elog_policy:enabled(trace, undefined, ?MODULE, ?LINE, Fmt, Args)).

%%
%% Events with a category, a format, and args
%%

-define(ELOG_EMERGENCY(Cat, Fmt, Args),
        begin
            lager:emergency(Fmt, Args),
            gmt_elog_policy:enabled(emergency, Cat, ?MODULE, ?LINE, Fmt, Args)
        end).
-define(ELOG_ALERT(Cat, Fmt, Args),
        begin
            lager:alert(Fmt, Args),
            gmt_elog_policy:enabled(alert, Cat, ?MODULE, ?LINE, Fmt, Args)
        end).
-define(ELOG_CRITICAL(Cat, Fmt, Args),
        begin
            lager:critical(Fmt, Args),
            gmt_elog_policy:enabled(critical, Cat, ?MODULE, ?LINE, Fmt, Args)
        end).
-define(ELOG_ERROR(Cat, Fmt, Args),
        begin
            lager:error(Fmt, Args),
            gmt_elog_policy:enabled(error, Cat, ?MODULE, ?LINE, Fmt, Args)
        end).
-define(ELOG_WARNING(Cat, Fmt, Args),
        begin
            lager:warning(Fmt, Args),
            gmt_elog_policy:enabled(warning, Cat, ?MODULE, ?LINE, Fmt, Args)
        end).
-define(ELOG_NOTICE(Cat, Fmt, Args),
        begin
            lager:notice(Fmt, Args),
            gmt_elog_policy:enabled(notice, Cat, ?MODULE, ?LINE, Fmt, Args)
        end).
-define(ELOG_INFO(Cat, Fmt, Args),
        begin
            lager:info(Fmt, Args),
            gmt_elog_policy:enabled(info, Cat, ?MODULE, ?LINE, Fmt, Args)
        end).
-define(ELOG_DEBUG(Cat, Fmt, Args),
        begin
            lager:debug(Fmt, Args),
            gmt_elog_policy:enabled(debug, Cat, ?MODULE, ?LINE, Fmt, Args)
        end).
-define(ELOG_TRACE(Cat, Fmt, Args),
        gmt_elog_policy:enabled(trace, Cat, ?MODULE, ?LINE, Fmt, Args)).

-endif. % -ifndef(gmt_elog_hrl).
