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
-module(restcheck_triq).

%%% BEHAVIOURS
-behaviour(restcheck_backend).

%%% EXTERNAL EXPORTS
-export([
    dto/1,
    quickcheck/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec dto(Schema) -> Generator when
    Schema :: restcheck_pbt:schema(),
    Generator :: restcheck_pbt:generator().
%% @doc Returns a <code>triq</code> generator of DTOs from a given schema.
dto(_Schema) ->
    % TODO: Implement.
    erlang:error({not_implemented, 'restcheck_triq:dto/1'}).

-spec quickcheck(Property) -> Result when
    Property :: restcheck_pbt:property(),
    Result :: ok | {error, Reason},
    Reason :: term().
%% @doc Runs a property-based test using <code>triq</code>.
quickcheck(_Property) ->
    % TODO: Implement.
    erlang:error({not_implemented, 'restcheck_triq:quickcheck/1'}).
