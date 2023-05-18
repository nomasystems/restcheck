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
    request/1,
    request/2
]).

%%% INTERNAL EXPORTS
-export([
    init/1
]).

%%% TYPES
-type client_config() :: #{
    host := binary(),
    port => integer(),
    ssl => boolean()
}.
-type req_config() :: #{
    headers => #{binary() => binary()},
    parameters => #{binary() => binary()},
    body => binary() | map(),
    timeout => non_neg_integer(),
    auth => #{
        username := binary(),
        password := binary()
    },
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
-type response() :: #{
    status := 100..599,
    headers => #{binary() => term()},
    body => undefined | #{binary() => term()} | binary()
}.

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
-spec request(Name) -> Result when
    Name :: atom(),
    Result :: {ok, Response} | {error, Reason},
    Response :: response(),
    Reason :: term().
%% @equiv request(Name, #{})
request(Name) ->
    request(Name, #{}).

-spec request(Name, Config) -> Result when
    Name :: atom(),
    Config :: req_config(),
    Result :: {ok, Response} | {error, Reason},
    Response :: response(),
    Reason :: term().
%% @doc Sends a request.
request(Name, Config) ->
    case persistent_term:get(?PERSISTENT_TERM(Name), undefined) of
        undefined ->
            {error, {restcheck_client_not_started, Name}};
        ClientConfig ->
            RawMethod = maps:get(method, Config, 'GET'),
            Method = method(RawMethod),
            Host = maps:get(host, ClientConfig),
            Port = maps:get(port, ClientConfig),
            Protocol = protocol(maps:get(ssl, ClientConfig, false)),
            RawPath = maps:get(path, Config, <<"/">>),
            Path =
                case maps:get(parameters, Config, undefined) of
                    undefined ->
                        RawPath;
                    Parameters ->
                        QueryString =
                            case uri_string:compose_query(maps:to_list(Parameters)) of
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
            RawHeaders = maps:get(headers, Config, #{}),
            Headers =
                case maps:get(auth, Config, undefined) of
                    undefined ->
                        RawHeaders;
                    #{username := Username, password := Password} ->
                        RawHeaders#{
                            <<"Authorization">> =>
                                <<"Basic ",
                                    (base64:encode(<<Username/binary, ":", Password/binary>>))/binary>>
                        }
                end,
            Body = body(maps:get(body, Config, <<>>)),
            Timeout = maps:get(timeout, Config, 5000),
            BuoyUrl = #buoy_url{
                protocol = Protocol,
                hostname = Host,
                port = Port,
                host = <<Host/binary, ":", (erlang:integer_to_binary(Port))/binary>>,
                path = Path
            },
            BuoyOpts = #{
                body => Body,
                headers => maps:to_list(Headers),
                timeout => Timeout
            },
            case buoy:request(Method, BuoyUrl, BuoyOpts) of
                {ok, BuoyResp} ->
                    RespStatus = BuoyResp#buoy_resp.status_code,
                    RespHeaders = headers(BuoyResp#buoy_resp.headers),
                    RespBody = body(
                        maps:get(<<"content-type">>, RespHeaders, undefined),
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
                ?SOCKET_OPTIONS ++ [{log_level, error}]
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
body(Map) when is_map(Map) ->
    njson:encode(Map);
body(Body) ->
    Body.

body(<<"application/json">>, BuoyBody) ->
    njson:decode(BuoyBody);
body(_ContentType, BuoyBody) ->
    BuoyBody.

headers(BuoyHeaders) ->
    lists:foldl(
        fun(Header, Acc) ->
            [RawName, RawValue] = binary:split(Header, <<":">>),
            Name = string:casefold(string:trim(RawName, both)),
            Value = string:trim(RawValue, both),
            Acc#{Name => Value}
        end,
        #{},
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
    Other.

protocol(true) ->
    https;
protocol(false) ->
    http.
