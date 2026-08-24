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
-module(restcheck_client_SUITE).

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        start_stop,
        request,
        base_path,
        auth
    ].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    Config = nct_util:setup_suite(Conf),
    load_server_mock(),
    {ok, ServerPid} = elli:start_link([
        {callback, restcheck_client_server},
        {port, 8080}
    ]),
    erlang:unlink(ServerPid),
    [{server_pid, ServerPid} | Config].

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    ServerPid = proplists:get_value(server_pid, Conf),
    elli:stop(ServerPid),
    unload_server_mock(),
    nct_util:teardown_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% INIT CASE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_testcase(Case, Conf) ->
    ct:print("Starting test case ~p", [Case]),
    nct_util:init_traces(Case),
    Conf.

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
    nct_util:end_traces(Case),
    ct:print("Test case ~p completed", [Case]),
    Conf.

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
start_stop(_Conf) ->
    {ok, ClientPid} = restcheck_client:start_link(
        restcheck_client,
        #{host => <<"localhost">>, port => 8080, ssl => false}
    ),
    ClientPid = erlang:whereis(restcheck_client),
    ok = restcheck_client:stop(restcheck_client),
    true = is_dead(ClientPid),
    ok.

request(_Conf) ->
    {ok, _ClientPid} = restcheck_client:start_link(
        restcheck_client,
        #{host => <<"localhost">>, port => 8080, ssl => false}
    ),
    meck:expect(
        restcheck_client_server,
        handle,
        fun
            ('GET', [<<"foo">>], _Req) ->
                {200, [{<<"Content-Type">>, <<"application/json">>}], <<"{\"foo\":\"bar\"}">>};
            ('GET', [<<"foo">>, <<"bar">>], Req) ->
                QueryString = elli_request:get_args(Req),
                case proplists:get_value(<<"limit">>, QueryString, undefined) of
                    <<"1">> ->
                        {200, [{<<"Content-Type">>, <<"application/json">>}],
                            <<"{\"bar\":[\"foo\"]}">>};
                    _Otherwise ->
                        {400, [], <<>>}
                end
        end
    ),

    {ok, #{
        status := 200,
        body := #{<<"foo">> := <<"bar">>}
    }} =
        restcheck_client:request(restcheck_client, #{path => <<"/foo">>}, #{}),

    {ok, #{
        status := 200,
        body := #{<<"bar">> := [<<"foo">>]}
    }} =
        restcheck_client:request(
            restcheck_client,
            #{
                path => <<"/foo/bar">>, query_parameters => [{<<"limit">>, 1}]
            },
            #{}
        ),
    {ok, #{status := 400, body := undefined}} =
        restcheck_client:request(
            restcheck_client,
            #{
                path => <<"/foo/bar">>, query_parameters => [{<<"limit">>, 2}]
            },
            #{}
        ),

    ok = restcheck_client:stop(restcheck_client),

    ok.

base_path(_Conf) ->
    meck:expect(
        restcheck_client_server,
        handle,
        fun('GET', _Path, Req) ->
            RawPath = elli_request:raw_path(Req),
            {200, [{<<"Content-Type">>, <<"application/json">>}],
                <<"{\"path\":\"", RawPath/binary, "\"}">>}
        end
    ),

    %% A trailing slash is dropped and a missing leading one is added, so the
    %% three of them request /api/v1/foo.
    lists:foreach(
        fun(RawBasePath) ->
            {ok, ClientPid} = restcheck_client:start_link(
                restcheck_client,
                #{
                    host => <<"localhost">>,
                    port => 8080,
                    ssl => false,
                    base_path => RawBasePath
                }
            ),
            {ok, #{status := 200, body := #{<<"path">> := <<"/api/v1/foo">>}}} =
                restcheck_client:request(restcheck_client, #{path => <<"/foo">>}, #{}),
            ok = restcheck_client:stop(restcheck_client),
            true = is_dead(ClientPid)
        end,
        [<<"/api/v1">>, <<"/api/v1/">>, <<"api/v1">>]
    ),

    ok.

auth(_Conf) ->
    {ok, _ClientPid} = restcheck_client:start_link(
        restcheck_client,
        #{host => <<"localhost">>, port => 8080, ssl => false}
    ),
    meck:expect(
        restcheck_client_server,
        handle,
        fun('GET', [<<"auth">>], Req) ->
            Headers = elli_request:headers(Req),
            Authorization =
                case proplists:get_value(<<"Authorization">>, Headers, undefined) of
                    undefined -> proplists:get_value(<<"authorization">>, Headers, <<>>);
                    Value -> Value
                end,
            {200, [{<<"Content-Type">>, <<"application/json">>}],
                element(2, njson:encode(#{<<"authorization">> => Authorization}))}
        end
    ),

    %% Basic auth
    {ok, #{
        status := 200,
        body := #{<<"authorization">> := <<"Basic YWRtaW46c2VjcmV0">>}
    }} =
        restcheck_client:request(
            restcheck_client,
            #{
                path => <<"/auth">>,
                auth => #{username => <<"admin">>, password => <<"secret">>}
            },
            #{}
        ),

    %% Bearer auth
    {ok, #{
        status := 200,
        body := #{<<"authorization">> := <<"Bearer admin">>}
    }} =
        restcheck_client:request(
            restcheck_client,
            #{path => <<"/auth">>, auth => #{token => <<"admin">>}},
            #{}
        ),

    ok = restcheck_client:stop(restcheck_client),

    ok.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
is_dead(Pid) ->
    is_dead(Pid, 5).

is_dead(_Pid, 0) ->
    false;
is_dead(Pid, N) ->
    case erlang:is_process_alive(Pid) of
        true ->
            timer:sleep(500),
            is_dead(Pid, N - 1);
        false ->
            true
    end.

load_server_mock() ->
    meck:new([restcheck_client_server], [non_strict, no_link]),
    meck:expect(
        restcheck_client_server,
        handle,
        fun(Req, _Args) ->
            restcheck_client_server:handle(
                elli_request:method(Req),
                elli_request:path(Req),
                Req
            )
        end
    ),
    meck:expect(restcheck_client_server, handle_event, fun(_Event, _Args, _Config) -> ok end),
    ok.

unload_server_mock() ->
    meck:unload(restcheck_client_server),
    ok.
