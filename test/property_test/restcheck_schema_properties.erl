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
%% limitations under the License.
-module(restcheck_schema_properties).

%%% INCLUDE FILES
-include_lib("stdlib/include/assert.hrl").
-include_lib("triq/include/triq.hrl").

%%% MACROS
-define(NUMTESTS, 3).

%%%-----------------------------------------------------------------------------
%%% PROPERTIES
%%%-----------------------------------------------------------------------------
prop_conmutative_intersection() ->
    triq:numtests(
        ?NUMTESTS,
        ?FORALL(
            {Schema1, Schema2},
            {restcheck_schema_dom:schema(), restcheck_schema_dom:schema()},
            begin
                I1_2 = restcheck_schema:intersection([Schema1, Schema2]),
                I2_1 = restcheck_schema:intersection([Schema2, Schema1]),
                ?assertEqual(I1_2, I2_1),

                true
            end
        )
    ).

prop_identity() ->
    ?FORALL(
        Schema,
        restcheck_schema_dom:schema(),
        begin
            EmptySchema = restcheck_schema:empty_schema(),
            ?assertEqual(Schema, restcheck_schema:union([Schema, EmptySchema])),

            UniversalSchema = restcheck_schema:universal_schema(),
            ?assertEqual(Schema, restcheck_schema:intersection([Schema, UniversalSchema])),

            true
        end
    ).

prop_idempotent() ->
    ?FORALL(
        Schema,
        restcheck_schema_dom:schema(),
        begin
            ?assertEqual(Schema, restcheck_schema:intersection([Schema, Schema])),
            true
        end
    ).

prop_domination() ->
    ?FORALL(
        Schema,
        restcheck_schema_dom:schema(),
        begin
            EmptySchema = restcheck_schema:empty_schema(),
            ?assertEqual(EmptySchema, restcheck_schema:intersection([Schema, EmptySchema])),
            true
        end
    ).
