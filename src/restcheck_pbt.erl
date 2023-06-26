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
    noshrink/2,
    quickcheck/3,
    quickcheck/4
]).

%%% TYPES
-type backend() :: restcheck_backend:t().
-type generator() :: term().
-type num_tests() :: pos_integer().
-type output_fun() :: fun((string(), [term()]) -> ok).
-type prop() :: fun((term()) -> true | {false, Reason :: term()}).
-type property() :: term().
-type schema() :: ndto:schema().

%%% EXPORT TYPES
-export_type([
    backend/0,
    generator/0,
    num_tests/0,
    output_fun/0,
    property/0,
    prop/0,
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

-spec noshrink(Backend, Generator) -> NoShrinkGenerator when
    Backend :: backend(),
    Generator :: generator(),
    NoShrinkGenerator :: generator().
%% @doc Disables shrinking for a given generator for a given backend.
noshrink(Backend, Generator) ->
    Backend:noshrink(Generator).

-spec quickcheck(Backend, Property, NumTests) -> Result when
    Backend :: backend(),
    Property :: property(),
    NumTests :: num_tests(),
    Result :: true | {false, Reason},
    Reason :: term().
%% @equiv quickcheck(Backend, Property, NumTests, fun(Format, Data) -> io:format(Format, Data) end)
quickcheck(Backend, Property, NumTests) ->
    quickcheck(Backend, Property, NumTests, fun(Format, Data) -> io:format(Format, Data) end).

-spec quickcheck(Backend, Property, NumTests, OutputFun) -> Result when
    Backend :: backend(),
    Property :: property(),
    NumTests :: num_tests(),
    OutputFun :: output_fun(),
    Result :: true | {false, Reason},
    Reason :: term().
%% @doc Runs a property-based test using the given backend.
quickcheck(Backend, Property, NumTests, OutputFun) ->
    Backend:quickcheck(Property, NumTests, OutputFun).
