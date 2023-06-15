%%% Copyright 2023 Nomasystems, S.L. http://www.nomasystems.com
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License
-module(restcheck_runner).

%%% EXTERNAL EXPORTS
-export([
    run/3
]).

%%% MACROS
-define(CLIENT_NAME(APIName),
    (erlang:binary_to_atom(
        erf_util:to_snake_case(<<"restcheck_", (APIName)/binary, "_client">>),
        utf8
    ))
).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec run(Conf, API, Backend) -> Result when
    Conf :: restcheck:conf(),
    API :: erf:api(),
    Backend :: restcheck_pbt:backend(),
    Result :: ok | {error, Reason},
    Reason :: term().
run(Conf, API, Backend) ->
    ClientName = ?CLIENT_NAME(maps:get(name, API)),
    TestOpts = #{},
    Host = maps:get(host, Conf, <<"localhost">>),
    Port = maps:get(port, Conf, 8080),
    SSL = maps:get(ssl, Conf, false),
    ClientConf = #{
        host => Host,
        port => Port,
        ssl => SSL
    },
    {ok, _Pid} = restcheck_client:start_link(ClientName, ClientConf),
    DTOs = maps:get(schemas, API, []),
    load_dtos(maps:to_list(DTOs)),
    {SuiteName, Suite} = restcheck_suite:generate(API),
    ok = restcheck_suite:load(Suite),
    lists:foreach(
        fun(Endpoint) ->
            lists:foreach(
                fun(Operation) ->
                    OperationId = maps:get(id, Operation),
                    Function = erlang:binary_to_atom(
                        erf_util:to_snake_case(<<"prop_", OperationId/binary>>),
                        utf8
                    ),
                    % TODO: Add a way to configure the number of tests
                    ForAll = SuiteName:Function(Backend, ClientName, TestOpts),
                    % TODO: Improve the result reporting
                    true = restcheck_pbt:quickcheck(Backend, ForAll),
                    ok
                end,
                maps:get(operations, Endpoint, [])
            )
        end,
        maps:get(endpoints, API, [])
    ),
    restcheck_client:stop(ClientName),
    ok.

load_dtos([]) ->
    ok;
load_dtos([{RawName, Schema} | DTOs]) ->
    Name = erlang:binary_to_atom(RawName),
    DTO = ndto:generate(Name, Schema),
    ndto:load(DTO),
    load_dtos(DTOs).
