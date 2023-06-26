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
-module(restcheck_SUITE).

%%% INCLUDE FILES
-include_lib("stdlib/include/assert.hrl").

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        petstore
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
petstore(_Conf) ->
    ServerError = #{
        <<"code">> => 500,
        <<"message">> => <<"Internal Server Error">>
    },
    BadRequestError = #{
        <<"code">> => 400,
        <<"message">> => <<"Bad Request">>
    },
    meck:expect(
        restcheck_client_server,
        handle,
        fun
            ([<<"pets">>], 'GET', _Headers, _QueryParameters, _Body) ->
                Pets = [
                    #{<<"id">> => 1, <<"name">> => <<"Foo">>},
                    #{<<"id">> => 2, <<"name">> => <<"Bar">>}
                ],
                {Code, Headers, Response} =
                    {200, [{<<"x-next">>, <<"/pets?nextPage=2">>}], njson:encode(Pets)},
                {Code, [{<<"Content-Type">>, <<"application/json">>} | Headers], Response};
            ([<<"pets">>], 'POST', _Headers, _QueryParameters, _Body) ->
                {Code, Headers, Response} = {500, [], njson:encode(ServerError)},
                {Code, [{<<"Content-Type">>, <<"application/json">>} | Headers], Response};
            ([<<"pets">>, _PetId], 'GET', _Headers, _QueryParameters, _Body) ->
                {Code, Headers, Response} = {400, [], njson:encode(BadRequestError)},
                {Code, [{<<"Content-Type">>, <<"application/json">>} | Headers], Response}
        end
    ),

    Conf = #{
        spec_path => unicode:characters_to_binary(
            code:priv_dir(restcheck) ++ "/oas/3.0/examples/petstore.json"
        ),
        spec_format => erf_oas_3_0,
        pbt_backend => restcheck_triq,
        host => <<"localhost">>,
        port => 8080,
        ssl => false,
        timeout => 5000,
        num_requests => 10
    },

    meck:new([restcheck_client], [passthrough]),

    ?assertMatch(
        {ok, [
            {<<"list_pets">>, true},
            {<<"create_pets">>, {false, {server_error, _InternalServerError}}},
            {<<"show_pet_by_id">>, {false, {bad_request, _BadRequestError}}}
        ]},
        restcheck:run(Conf)
    ),

    ?assertEqual(
        10,
        meck:num_calls(restcheck_client, request, ['_', #{path => <<"/pets">>, method => get}, '_'])
    ),

    meck:unload(restcheck_client),

    ok.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
load_server_mock() ->
    meck:new([restcheck_client_server], [non_strict, no_link]),
    meck:expect(
        restcheck_client_server,
        handle,
        fun(Req, _Args) ->
            restcheck_client_server:handle(
                elli_request:path(Req),
                elli_request:method(Req),
                elli_request:headers(Req),
                elli_request:get_args(Req),
                elli_request:body(Req)
            )
        end
    ),
    meck:expect(restcheck_client_server, handle_event, fun(_Event, _Args, _Config) -> ok end),
    ok.

unload_server_mock() ->
    meck:unload(restcheck_client_server),
    ok.
