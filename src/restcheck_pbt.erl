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
-module(restcheck_pbt).

%%% EXTERNAL EXPORTS
-export([
    dto/1,
    dto/2,
    quickcheck/1,
    quickcheck/2
]).

%%% TYPES
-type backend() :: restcheck_backend:t().
-type generator() :: term().
-type property() :: term().
-type schema() :: ndto:schema().

%%% EXPORT TYPES
-export_type([
    backend/0,
    generator/0,
    property/0,
    schema/0
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec dto(Schema) -> Generator when
    Schema :: schema(),
    Generator :: generator().
%% @equiv dto(Schema, restcheck_triq)
dto(Schema) ->
    dto(Schema, restcheck_triq).

-spec dto(Schema, Backend) -> Generator when
    Schema :: schema(),
    Backend :: backend(),
    Generator :: generator().
%% @doc Asks the given backend for a DTO generator from a given schema.
dto(Schema, Backend) ->
    Backend:dto(Schema).

-spec quickcheck(Property) -> Result when
    Property :: property(),
    Result :: ok | {error, Reason},
    Reason :: term().
%% @equiv quickcheck(Property, restcheck_triq)
quickcheck(Property) ->
    quickcheck(Property, restcheck_triq).

-spec quickcheck(Property, Backend) -> Result when
    Property :: property(),
    Backend :: backend(),
    Result :: ok | {error, Reason},
    Reason :: term().
%% @doc Runs a property-based test using the given backend.
quickcheck(Property, Backend) ->
    Backend:quickcheck(Property).
