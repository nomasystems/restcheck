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

%% @doc <code>restcheck</code>'s main module.
-module(restcheck).

%%% REBAR3 EXPORTS
-export([
    init/1,
    do/1,
    format_error/1
]).

%%% LIB EXPORTS
-export([
    run/1
]).

%%% TYPES
-type conf() :: #{
    spec_path := binary(),
    spec_parser => module(),
    pbt_backend => restcheck_pbt:backend(),
    host => binary(),
    port => inet:port_number(),
    ssl => boolean(),
    auth => restcheck_client:auth(),
    timeout => non_neg_integer(),
    num_requests => pos_integer(),
    output_fun => restcheck_pbt:output_fun()
}.
-type test_result() :: {OperationId :: binary(), Result :: ok | {error, Reason :: term()}}.

%%% EXPORT TYPES
-export_type([
    conf/0,
    test_result/0
]).

%%% MACROS
-define(CLIENT_NAME(APIName),
    (erlang:binary_to_atom(
        erf_util:to_snake_case(<<"restcheck_", (APIName)/binary, "_client">>),
        utf8
    ))
).
-define(PROP_NAME(OperationId),
    (erlang:binary_to_atom(
        erf_util:to_snake_case(<<"prop_", (OperationId)/binary>>),
        utf8
    ))
).

%%%-----------------------------------------------------------------------------
%%% REBAR3 EXPORTS
%%%-----------------------------------------------------------------------------
-spec init(State) -> Result when
    State :: rebar_state:t(),
    Result :: {ok, NewState},
    NewState :: rebar_state:t().
init(State) ->
    Provider = providers:create([
        {name, restcheck},
        {module, ?MODULE},
        {bare, true},
        {deps, [app_discovery]},
        {example, "rebar3 restcheck"},
        {opts, []},
        {short_desc, "A rebar3 plugin to run restcheck tests."},
        {desc,
            "A rebar3 plugin to run restcheck tests.\n"
            "usage:\n"
            "{restcheck, [\n"
            "    {spec_path, string()}, % path to spec file\n"
            "    {spec_parser, module()}, % spec to API AST parser, defaults to erf_oas_3_0\n"
            "    {pbt_backend, module()}, % restcheck_pbt backend, defaults to restcheck_triq\n"
            "    {host, string()}, % server host, defaults to \"localhost\"\n"
            "    {port, inet:port_number()}, % server port number, defaults to 8080\n"
            "    {ssl, boolean()}, % enable/disable ssl, defaults to false\n"
            "    {auth, restcheck_client:auth()}, % includes auth headers in the request\n"
            "    {timeout, pos_integer()} % timeout per request in ms, defaults to 5000\n"
            "    {num_requests, pos_integer()} % number of requests per operation, defaults to 5000\n"
            "    {log_file, string()} % path to file where timestamped events are logged, disabled if no path is provided\n"
            "]}."}
    ]),
    NewState = rebar_state:add_provider(State, Provider),
    {ok, NewState}.

-spec do(State) -> Result when
    State :: rebar_state:t(),
    Result :: {ok, NewState} | {error, Reason},
    NewState :: rebar_state:t(),
    Reason :: string().
do(State) ->
    RawConf = rebar_state:get(State, restcheck, []),
    Conf = #{
        spec_path => unicode:characters_to_binary(proplists:get_value(spec_path, RawConf)),
        spec_parser => proplists:get_value(spec_parser, RawConf, erf_oas_3_0),
        pbt_backend => proplists:get_value(pbt_backend, RawConf, restcheck_triq),
        host => unicode:characters_to_binary(proplists:get_value(host, RawConf, "localhost")),
        port => proplists:get_value(port, RawConf, 8080),
        ssl => proplists:get_value(ssl, RawConf, false),
        timeout => proplists:get_value(timeout, RawConf, 5000),
        num_requests => proplists:get_value(num_requests, RawConf, 100)
    },
    LogFile = proplists:get_value(log_file, RawConf, undefined),
    LogEnabled =
        case LogFile of
            undefined ->
                false;
            _LogFile ->
                case filelib:ensure_dir(LogFile) of
                    ok ->
                        true;
                    {error, LogFileReason} ->
                        rebar_api:warning(
                            "Unable to create log file directory ~s. Log is disabled. Reason: ~s.",
                            [LogFile, LogFileReason]
                        ),
                        false
                end
        end,
    application:ensure_all_started(restcheck, permanent),
    case generate_and_load_suite(Conf) of
        {ok, {APIName, Tests}} ->
            ClientName = ?CLIENT_NAME(APIName),
            Host = maps:get(host, Conf, <<"localhost">>),
            Port = maps:get(port, Conf, 8080),
            SSL = maps:get(ssl, Conf, false),
            ClientConf = #{
                host => Host,
                port => Port,
                ssl => SSL
            },
            {ok, _Pid} = restcheck_client:start_link(ClientName, ClientConf),
            case LogEnabled of
                false ->
                    ok;
                true ->
                    file_log(LogFile, "Running restcheck for ~s", [APIName])
            end,
            rebar_api:info("Running restcheck for ~s", [APIName]),
            rebar_api:console("+~s+~s+~s+", [
                lists:duplicate(5, $-),
                lists:duplicate(32, $-),
                lists:duplicate(32, $-)
            ]),
            rebar_api:console("| ~3.ts | ~30.ts | ~30.ts |", [
                <<"OK?">>,
                <<"OperationId">>,
                <<"Description">>
            ]),
            rebar_api:console("+~s+~s+~s+", [
                lists:duplicate(5, $-),
                lists:duplicate(32, $-),
                lists:duplicate(32, $-)
            ]),
            Results =
                lists:map(
                    fun({OperationId, LazyTest}) ->
                        case LazyTest() of
                            true ->
                                case LogEnabled of
                                    false ->
                                        ok;
                                    true ->
                                        file_log(LogFile, "✅ ~30.ts", [OperationId])
                                end,
                                rebar_api:console("| ~2.ts | ~30.ts | ~30.ts |", [
                                    <<"✅"/utf8>>,
                                    OperationId,
                                    "Success"
                                ]),
                                true;
                            {false, Error} ->
                                case LogEnabled of
                                    false ->
                                        ok;
                                    true ->
                                        file_log(LogFile, "❌ ~30.ts: ~p", [OperationId, Error])
                                end,
                                rebar_api:console("| ~2.ts | ~30.ts | ~30.ts |", [
                                    <<"❌"/utf8>>,
                                    OperationId,
                                    format_error(Error)
                                ]),
                                false
                        end
                    end,
                    Tests
                ),
            rebar_api:console("+~s+~s+~s+", [
                lists:duplicate(5, $-),
                lists:duplicate(32, $-),
                lists:duplicate(32, $-)
            ]),
            restcheck_client:stop(ClientName),
            case
                lists:foldl(
                    fun
                        (true, {PassAcc, FailAcc}) ->
                            {PassAcc + 1, FailAcc};
                        (false, {PassAcc, FailAcc}) ->
                            {PassAcc, FailAcc + 1}
                    end,
                    {0, 0},
                    Results
                )
            of
                {Passed, 0} ->
                    case LogEnabled of
                        false ->
                            ok;
                        true ->
                            file_log(LogFile, "All restcheck tests passed for ~s. ~B/~B.", [
                                APIName, Passed, Passed
                            ])
                    end,
                    rebar_api:info("All restcheck tests passed for ~s. ~B/~B.", [
                        APIName, Passed, Passed
                    ]);
                {Passed, Failed} ->
                    case LogEnabled of
                        false ->
                            ok;
                        true ->
                            file_log(LogFile, "~B out of ~B restcheck tests failed for ~s", [
                                Failed, Failed + Passed, APIName
                            ])
                    end,
                    rebar_api:abort("~B out of ~B restcheck tests failed for ~s", [
                        Failed, Failed + Passed, APIName
                    ])
            end,
            {ok, State};
        {error, Reason} ->
            case LogEnabled of
                false ->
                    ok;
                true ->
                    file_log(LogFile, "Errors running restcheck: ~s", [format_error(Reason)])
            end,
            rebar_api:abort("Errors running restcheck: ~s\n", [format_error(Reason)])
    end.

-spec format_error(Reason) -> Result when
    Reason :: any(),
    Result :: iodata().
format_error({bad_request, _Req}) ->
    "Bad request";
format_error({method_not_allowed, {_Path, _Method}}) ->
    "Method not allowed";
format_error({server_error, _StatusCode}) ->
    "Server error";
format_error(Reason) ->
    erlang:term_to_binary(Reason).

%%%-----------------------------------------------------------------------------
%%% LIB EXPORTS
%%%-----------------------------------------------------------------------------
-spec run(Conf) -> Result when
    Conf :: conf(),
    Result :: {ok, TestResults} | {error, Reason},
    TestResults :: [test_result()],
    Reason :: term().
run(Conf) ->
    case generate_and_load_suite(Conf) of
        {ok, {APIName, Tests}} ->
            ClientName = ?CLIENT_NAME(APIName),
            Host = maps:get(host, Conf, <<"localhost">>),
            Port = maps:get(port, Conf, 8080),
            SSL = maps:get(ssl, Conf, false),
            ClientConf = #{
                host => Host,
                port => Port,
                ssl => SSL
            },
            {ok, _Pid} = restcheck_client:start_link(ClientName, ClientConf),
            TestResults = lists:map(
                fun({OperationId, LazyTest}) ->
                    {OperationId, LazyTest()}
                end,
                Tests
            ),
            restcheck_client:stop(ClientName),
            {ok, TestResults};
        {error, Reason} ->
            {error, Reason}
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec file_log(LogFile, Format, Args) -> ok when
    LogFile :: file:name_all(),
    Format :: string(),
    Args :: [term()].
file_log(LogFile, Format, Args) ->
    {ok, LogFd} = file:open(LogFile, [append]),
    Now = calendar:system_time_to_rfc3339(erlang:system_time(second)),
    case unicode:characters_to_binary(io_lib:format("~s " ++ Format ++ "~n", [Now | Args])) of
        {error, Bin, Reason} ->
            rebar_api:warn("Unable to format log entry: ~p. Bin: ~p", [Reason, Bin]);
        {incomplete, Rest, Reason} ->
            rebar_api:warn("Unable to format log entry: ~p. Rest: ~p", [Reason, Rest]);
        Data ->
            ok = file:write(LogFd, Data)
    end,
    ok = file:close(LogFd).

-spec generate_and_load_suite(Conf) -> Result when
    Conf :: conf(),
    Result :: {ok, {APIName, Tests}} | {error, Reason},
    APIName :: binary(),
    Tests :: [{OperationId, LazyTest}],
    OperationId :: binary(),
    LazyTest :: fun(() -> true | {false, Reason :: term()}),
    Reason :: term().
generate_and_load_suite(Conf) ->
    SpecPath = maps:get(spec_path, Conf),
    SpecParser = maps:get(spec_format, Conf, erf_oas_3_0),
    case erf_parser:parse(SpecPath, SpecParser) of
        {ok, API} ->
            APIName = maps:get(name, API),
            Backend = maps:get(pbt_backend, Conf, restcheck_triq),
            ClientName = ?CLIENT_NAME(APIName),
            NumRequests = maps:get(num_requests, Conf, 100),
            TestOpts = #{
                auth => maps:get(auth, Conf, undefined),
                timeout => maps:get(timeout, Conf, 5000)
            },
            DTOs = maps:get(schemas, API, []),
            load_dtos(maps:to_list(DTOs)),
            {SuiteName, Suite} = restcheck_suite:generate(API),
            ok = restcheck_suite:load(Suite),
            Tests = lists:flatmap(
                fun(Endpoint) ->
                    lists:map(
                        fun(Operation) ->
                            OperationId = maps:get(id, Operation),
                            Property = ?PROP_NAME(OperationId),
                            ForAll = SuiteName:Property(Backend, ClientName, TestOpts),
                            Test =
                                fun() ->
                                    restcheck_pbt:quickcheck(
                                        Backend,
                                        ForAll,
                                        NumRequests,
                                        fun(_Format, _Args) -> ok end
                                    )
                                end,
                            {OperationId, Test}
                        end,
                        maps:get(operations, Endpoint, [])
                    )
                end,
                maps:get(endpoints, API, [])
            ),
            {ok, {APIName, Tests}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec load_dtos(DTOs) -> Result when
    DTOs :: [{erf_parser:ref(), ndto:schema()}],
    Result :: ok.
load_dtos([]) ->
    ok;
load_dtos([{RawName, Schema} | DTOs]) ->
    Name = erlang:binary_to_atom(RawName),
    DTO = ndto:generate(Name, Schema),
    ndto:load(DTO),
    load_dtos(DTOs).
