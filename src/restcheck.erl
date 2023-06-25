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

%%% EXTERNAL EXPORTS
-export([
    test/1
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
    num_requests => pos_integer()
}.

%%% EXPORT TYPES
-export_type([
    conf/0
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec test(Conf) -> Result when
    Conf :: conf(),
    Result :: ok | {error, Reason},
    Reason :: term().
test(Conf) ->
    SpecPath = maps:get(spec_path, Conf),
    SpecParser = maps:get(spec_format, Conf, erf_oas_3_0),
    case erf_parser:parse(SpecPath, SpecParser) of
        {ok, API} ->
            Backend = maps:get(pbt_backend, Conf, restcheck_triq),
            restcheck_runner:run(Conf, API, Backend);
        {error, Reason} ->
            {error, Reason}
    end.
