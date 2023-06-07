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
%% @doc Property-based testing interface for <code>restcheck</code>.
-module(restcheck_pbt).

%%% EXTERNAL EXPORTS
-export([
    dto/2,
    forall/3,
    quickcheck/2
]).

%%% TYPES
-type backend() :: restcheck_backend:t().
-type generator() :: term().
-type prop() :: fun((term()) -> boolean()).
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
-spec dto(Backend, Schema) -> Generator when
    Backend :: backend(),
    Schema :: schema(),
    Generator :: generator().
%% @doc Asks the given backend for a DTO generator from a given schema.
dto(Backend, Schema) ->
    Backend:dto(Schema).

-spec forall(Backend, Generators, Prop) -> ForAll when
    Backend :: backend(),
    Generators :: [generator()],
    Prop :: prop(),
    ForAll :: property().
%% @doc Wraps a <code>forall</code> property in the given backend format.
forall(Backend, Generators, Prop) ->
    Backend:forall(Generators, Prop).

-spec quickcheck(Backend, Property) -> Result when
    Backend :: backend(),
    Property :: property(),
    Result :: ok | {error, Reason},
    Reason :: term().
%% @doc Runs a property-based test using the given backend.
quickcheck(Backend, Property) ->
    Backend:quickcheck(Property).
