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
%% @doc <code>restcheck</code>'s HTTP client.
-module(restcheck_client).

%%% START/STOP EXPORTS
-export([
    start_link/1,
    start_link/2,
    stop/1
]).

%%% EXTERNAL EXPORTS
-export([
    request/1,
    request/2
]).

%%% TYPES
-type req_config() :: #{
    host => binary(),
    port => integer(),
    ssl => boolean(),
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
        'GET'
        | 'POST'
        | 'PUT'
        | 'DELETE'
        | 'PATCH'
        | 'HEAD'
        | 'OPTIONS'
        | 'TRACE'
        | 'CONNECT'
        | binary()
}.
-type response() :: #{
    status := 100..599,
    headers => #{binary() => term()},
    body => #{binary() => term()}
}.

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
% TODO: remove this when implemented
-dialyzer({no_return, start_link/1}).
-spec start_link(Name) -> Result when
    Name :: atom(),
    Result :: {ok, Pid} | {error, Reason},
    Pid :: pid(),
    Reason :: term().
%% @equiv start_link(Name, #{})
start_link(Name) ->
    start_link(Name, #{}).

-spec start_link(Name, BaseConfig) -> Result when
    Name :: atom(),
    BaseConfig :: req_config(),
    Result :: {ok, Pid} | {error, Reason},
    Pid :: pid(),
    Reason :: term().
%% @doc Starts a new client with given name and base config.
start_link(_Name, _BaseConfig) ->
    %% TODO: Implement
    erlang:error({not_implemented, 'restcheck_client:start_link/2'}).

-spec stop(Name) -> Result when
    Name :: atom(),
    Result :: ok | {error, Reason},
    Reason :: term().
%% @doc Stops the client with given name.
stop(_Name) ->
    %% TODO: Implement
    erlang:error({not_implemented, 'restcheck_client:stop/1'}).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
% TODO: remove this when implemented
-dialyzer({no_return, request/1}).
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
%% @doc Sends a request. Given config is merged with base config.
request(_Name, _Config) ->
    %% TODO: Implement
    erlang:error({not_implemented, 'restcheck_client:request/2'}).
