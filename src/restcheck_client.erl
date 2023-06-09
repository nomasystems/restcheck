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
%
%% @private
%% @doc <code>restcheck</code>'s HTTP client. <br/>
%% It internally uses the <code>buoy</code> HTTP client.
-module(restcheck_client).

%%% INCLUDE FILES
-include_lib("buoy/include/buoy.hrl").
-include_lib("kernel/include/logger.hrl").

%%% START/STOP EXPORTS
-export([
    start_link/2,
    stop/1
]).

%%% EXTERNAL EXPORTS
-export([
    request/3
]).

%%% INTERNAL EXPORTS
-export([
    init/1
]).

%%% TYPES
-type auth() :: basic_auth().
-type basic_auth() :: #{
    username := binary(),
    password := binary()
}.
-type request_body() :: njson:t().
-type response_body() :: njson:t().
-type client_config() :: #{
    host := binary(),
    port => integer(),
    ssl => boolean()
}.
-type req_config() :: #{
    headers => [{binary(), binary()}],
    query_parameters => [{binary(), binary()}],
    body => request_body(),
    auth => auth(),
    path => binary(),
    method =>
        get
        | post
        | put
        | delete
        | patch
        | head
        | options
        | trace
        | connect
        | binary()
}.
-type req_opts() :: #{
    timeout => non_neg_integer()
}.
-type response() :: #{
    status := inet:status_code(),
    headers => [{binary(), binary()}],
    body => response_body()
}.

%%% EXPORT TYPES
-export_type([
    auth/0
]).

%%% MACROS
-define(PERSISTENT_TERM(Name),
    (erlang:binary_to_atom(<<"restcheck_client_pt_", (erlang:atom_to_binary(Name))/binary>>))
).
-define(SOCKET_OPTIONS, [
    binary,
    {packet, line},
    {packet, raw},
    {send_timeout, 50},
    {send_timeout_close, true}
]).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
-spec start_link(Name, ClientConfig) -> Result when
    Name :: atom(),
    ClientConfig :: client_config(),
    Result :: supervisor:startlink_ret().
%% @doc Starts a new client with given name and config.
start_link(Name, ClientConfig) ->
    supervisor:start_link({local, Name}, ?MODULE, [Name, ClientConfig]).

-spec stop(Name) -> Result when
    Name :: atom(),
    Result :: ok | {error, Reason},
    Reason :: term().
%% @doc Stops the client with given name.
stop(Name) ->
    case persistent_term:get(?PERSISTENT_TERM(Name), undefined) of
        undefined ->
            {error, {restcheck_client_not_started, Name}};
        ClientConfig ->
            Protocol = protocol(maps:get(ssl, ClientConfig, false)),
            Host = maps:get(host, ClientConfig),
            Port = maps:get(port, ClientConfig, 80),
            BuoyUrl = #buoy_url{
                protocol = Protocol,
                hostname = Host,
                port = Port,
                % Ignored by buoy but needed to correct record construction
                host = <<Host/binary, ":", (erlang:integer_to_binary(Port))/binary>>,
                % Ignored by buoy but needed to correct record construction
                path = <<"/">>
            },
            case buoy_pool:stop(BuoyUrl) of
                ok ->
                    case persistent_term:erase(?PERSISTENT_TERM(Name)) of
                        true ->
                            case erlang:whereis(Name) of
                                undefined ->
                                    {error, {restcheck_client_not_started, Name}};
                                Pid ->
                                    true = erlang:exit(Pid, normal),
                                    ok
                            end;
                        false ->
                            {error, {restcheck_client_not_started, Name}}
                    end;
                {error, _Reason} ->
                    {error, {restcheck_client_not_started, Name}}
            end
    end.

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec request(Name, Config, Opts) -> Result when
    Name :: atom(),
    Config :: req_config(),
    Opts :: req_opts(),
    Result :: {ok, Response} | {error, Reason},
    Response :: response(),
    Reason :: term().
%% @doc Sends a request.
request(Name, Config, Opts) ->
    case persistent_term:get(?PERSISTENT_TERM(Name), undefined) of
        undefined ->
            {error, {restcheck_client_not_started, Name}};
        ClientConfig ->
            RawMethod = maps:get(method, Config, get),
            Method = method(RawMethod),
            Host = maps:get(host, ClientConfig),
            Port = maps:get(port, ClientConfig),
            Protocol = protocol(maps:get(ssl, ClientConfig, false)),
            RawPath = maps:get(path, Config, <<"/">>),
            Path =
                case maps:get(query_parameters, Config, undefined) of
                    undefined ->
                        RawPath;
                    RawQueryParameters ->
                        QueryParameters =
                            lists:map(
                                fun
                                    ({Key, Atom}) when is_atom(Atom) ->
                                        {Key, erlang:atom_to_binary(Atom)};
                                    ({Key, Integer}) when is_integer(Integer) ->
                                        {Key, erlang:integer_to_binary(Integer)};
                                    ({Key, Float}) when is_float(Float) ->
                                        {Key, erlang:float_to_binary(Float)};
                                    ({Key, Value}) ->
                                        {Key, Value}
                                end,
                                RawQueryParameters
                            ),
                        QueryString =
                            case uri_string:compose_query(QueryParameters) of
                                Binary when is_binary(Binary) ->
                                    Binary;
                                String when is_list(String) ->
                                    erlang:list_to_binary(String);
                                Error ->
                                    ?LOG_WARNING(
                                        "[restcheck_client] Failed to compose query string for request to ~s. Error: ~p.",
                                        [RawPath, Error]
                                    ),
                                    <<>>
                            end,
                        <<RawPath/binary, "?", (QueryString)/binary>>
                end,
            RawHeaders = [
                {<<"content-type">>, <<"application/json">>}
                | maps:get(headers, Config, [])
            ],
            Headers =
                case maps:get(auth, Config, undefined) of
                    #{username := Username, password := Password} ->
                        [
                            {
                                <<"Authorization">>,
                                <<"Basic ",
                                    (base64:encode(<<Username/binary, ":", Password/binary>>))/binary>>
                            }
                            | RawHeaders
                        ];
                    _Auth ->
                        RawHeaders
                end,
            Body = body(maps:get(body, Config, <<>>)),
            Timeout =
                case maps:get(timeout, Opts, undefined) of
                    undefined ->
                        5000;
                    TimeoutOpt ->
                        TimeoutOpt
                end,
            BuoyUrl = #buoy_url{
                protocol = Protocol,
                hostname = Host,
                port = Port,
                host = <<Host/binary, ":", (erlang:integer_to_binary(Port))/binary>>,
                path = Path
            },
            BuoyOpts = #{
                body => Body,
                headers => Headers,
                timeout => Timeout
            },
            case buoy:request(Method, BuoyUrl, BuoyOpts) of
                {ok, BuoyResp} ->
                    RespStatus = BuoyResp#buoy_resp.status_code,
                    RespHeaders = headers(BuoyResp#buoy_resp.headers),
                    RespBody = body(
                        proplists:get_value(<<"content-type">>, RespHeaders, undefined),
                        BuoyResp#buoy_resp.body
                    ),
                    Response = #{
                        status => RespStatus,
                        headers => RespHeaders,
                        body => RespBody
                    },
                    {ok, Response};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
init([Name, ClientConfig]) ->
    ok = persistent_term:put(?PERSISTENT_TERM(Name), ClientConfig),
    Protocol = protocol(maps:get(ssl, ClientConfig, false)),
    Host = maps:get(host, ClientConfig),
    Port = maps:get(port, ClientConfig, 80),
    BuoyUrl = #buoy_url{
        protocol = Protocol,
        hostname = Host,
        port = Port,
        % Ignored by buoy but needed to correct record construction
        host = <<Host/binary, ":", (erlang:integer_to_binary(Port))/binary>>,
        % Ignored by buoy but needed to correct record construction
        path = <<"/">>
    },
    SocketOptions =
        case Protocol of
            http ->
                ?SOCKET_OPTIONS;
            https ->
                [{log_level, error} | ?SOCKET_OPTIONS]
        end,
    case buoy_pool:start(BuoyUrl, [{socket_options, SocketOptions}]) of
        ok ->
            {ok, {#{}, []}};
        {error, Reason} ->
            erlang:exit(Reason)
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
body(Body) ->
    njson:encode(Body).

body(_ContentType, undefined) ->
    undefined;
body(<<"application/json">>, BuoyBody) ->
    njson:decode(BuoyBody);
body(_ContentType, BuoyBody) ->
    BuoyBody.

headers(BuoyHeaders) ->
    lists:map(
        fun(Header) ->
            [RawName, RawValue] = binary:split(Header, <<":">>),
            Name = string:casefold(string:trim(RawName, both)),
            Value = string:trim(RawValue, both),
            {Name, Value}
        end,
        BuoyHeaders
    ).

method(get) ->
    get;
method(post) ->
    post;
method(put) ->
    put;
method(head) ->
    head;
method(delete) ->
    {custom, <<"DELETE">>};
method(patch) ->
    {custom, <<"PATCH">>};
method(options) ->
    {custom, <<"OPTIONS">>};
method(trace) ->
    {custom, <<"TRACE">>};
method(connect) ->
    {custom, <<"CONNECT">>};
method(Other) ->
    {custom, Other}.

protocol(true) ->
    https;
protocol(false) ->
    http.
