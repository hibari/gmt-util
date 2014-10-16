%%%-------------------------------------------------------------------
%%% Copyright (c) 2007-2014 Hibari developers.  All rights reserved.
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
%%% File    : gmt_util_sup.erl
%%% Purpose : Top-level gmt util supervisor
%%%-------------------------------------------------------------------

-module(gmt_util_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link([]) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    %% Hint:
    %% Child_spec = [Name, {M, F, A},
    %%               Restart, Shutdown_time, Type, Modules_used]

    case gmt_elog_policy:dtrace_support() of
        unsupported ->
            lager:warning("dyntrace (DTrace/SystemTap) support is not available "
                          "in this Erlang VM. Please rebuild your Erlang/OTP with "
                          "'--with-dynamic-trace=' option to use this feature. "
                          "You can also disable this feature by setting "
                          "'dtrace_support' to 'false' in gmt_util section "
                          "of sys.config.");
        _ ->
            ok
    end,

    SysMonSrv =
        {gmt_sysmon_server, {gmt_sysmon_server, start_link, []},
         permanent, 2000, worker, [gmt_sysmon_server]},

    TLogSrv =
        {gmt_tlog_svr, {gmt_tlog_svr, start_link, []},
         permanent, 2000, worker, [gmt_tlog_svr]},

    Servers = [TLogSrv, SysMonSrv],

    {ok, {{one_for_one, 15, 60}, Servers}}.

%%====================================================================
%% Internal functions
%%====================================================================
