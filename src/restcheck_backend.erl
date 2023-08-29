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
-module(restcheck_backend).

%%% TYPES
-type t() :: module().
% A module that implements this behaviour.

%%% EXPORT TYPES
-export_type([t/0]).

%%%-----------------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%-----------------------------------------------------------------------------
-callback dto(Schema) -> Generator when
    Schema :: restcheck_pbt:schema(),
    Generator :: restcheck_pbt:generator().
%% Returns a DTO generator from a given schema.

-callback forall(Generators, Prop) -> ForAll when
    Generators :: [restcheck_pbt:generator()],
    Prop :: restcheck_pbt:prop(),
    ForAll :: restcheck_pbt:property().
%% Wraps a <code>forall</code> property.

-callback noshrink(Generator) -> NoShrinkGenerator when
    Generator :: restcheck_pbt:generator(),
    NoShrinkGenerator :: restcheck_pbt:generator().
%% Prevents a generator from shrinking.

-callback quickcheck(Property, NumTests, OutputFun) -> Result when
    Property :: restcheck_pbt:property(),
    NumTests :: restcheck_pbt:num_tests(),
    OutputFun :: restcheck_pbt:output_fun(),
    Result :: ok | {error, Reason},
    Reason :: term().
%% Runs a property-based test.
