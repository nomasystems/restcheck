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
-module(restcheck_schema_SUITE).

%%% INCLUDE FILES
-include_lib("stdlib/include/assert.hrl").

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%% MACROS
-define(ENUM_SCHEMA, #{enum => [1, <<"string">>, true]}).
-define(BOOLEAN_SCHEMA, #{type => boolean}).
-define(INTEGER_SCHEMA, #{
    type => integer,
    minimum => 2,
    exclusive_minimum => true,
    maximum => 6
}).
-define(NUMBER_SCHEMA, #{
    type => float,
    minimum => 4,
    exclusive_minimum => true,
    maximum => 8,
    exclusive_maximum => true
}).
-define(STRING_SCHEMA, #{
    type => string,
    min_length => 3,
    max_length => 6,
    pattern => <<"a{3}">>
}).
-define(ARRAY_SCHEMA, #{
    type => array,
    items => ?NUMBER_SCHEMA,
    min_items => 1,
    max_items => 3
}).
-define(OBJECT_SCHEMA, #{
    type => object,
    properties => #{
        <<"foo">> => ?INTEGER_SCHEMA,
        <<"bar">> => ?STRING_SCHEMA
    },
    min_properties => 3,
    additional_properties => true
}).
-define(INTEGER_SCHEMA_2, #{
    type => integer,
    minimum => 4,
    exclusive_minimum => true,
    maximum => 8,
    exclusive_maximum => true
}).
-define(FLOAT_SCHEMA_A, #{
    type => float,
    minimum => 2,
    exclusive_minimum => true,
    maximum => 6
}).
-define(INTERSECTION_SCHEMA, #{all_of => [?INTEGER_SCHEMA, ?INTEGER_SCHEMA_2]}).
-define(UNION_SCHEMA, #{any_of => [?BOOLEAN_SCHEMA, ?STRING_SCHEMA]}).
-define(SYMMETRIC_DIFFERENCE_SCHEMA, #{one_of => [?FLOAT_SCHEMA_A, ?NUMBER_SCHEMA]}).
-define(COMPLEMENT_SCHEMA, #{'not' => ?BOOLEAN_SCHEMA}).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        {group, properties},
        complement,
        intersection,
        union
    ].

groups() ->
    [
        {properties, [parallel], [
            prop_conmutative_intersection,
            prop_identity,
            prop_idempotent,
            prop_domination,
            prop_empty_schema_complement,
            prop_universal_schema_complement
        ]}
    ].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    Config = nct_util:setup_suite(Conf),
    ct_property_test:init_per_suite(Config).

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    nct_util:teardown_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% INIT CASE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_testcase(Case, Conf) ->
    ct:print("Starting test case ~p", [Case]),
    nct_util:init_traces(Case),
    Conf.

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
    nct_util:end_traces(Case),
    ct:print("Test case ~p completed", [Case]),
    Conf.

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
prop_conmutative_intersection(Conf) ->
    ct_property_test:quickcheck(
        restcheck_schema_properties:prop_conmutative_intersection(),
        Conf
    ).

prop_identity(Conf) ->
    ct_property_test:quickcheck(
        restcheck_schema_properties:prop_identity(),
        Conf
    ).

prop_idempotent(Conf) ->
    ct_property_test:quickcheck(
        restcheck_schema_properties:prop_idempotent(),
        Conf
    ).

prop_domination(Conf) ->
    ct_property_test:quickcheck(
        restcheck_schema_properties:prop_domination(),
        Conf
    ).

prop_empty_schema_complement(_Conf) ->
    EmptySet = false,
    UniversalSchema = restcheck_schema:universal_schema(),
    ?assertEqual(UniversalSchema, restcheck_schema:complement(EmptySet)),
    ok.

prop_universal_schema_complement(_Conf) ->
    UniversalSchema1 = restcheck_schema:universal_schema(),
    UniversalSchema2 = true,
    EmptySet = false,
    ?assertEqual(EmptySet, restcheck_schema:complement(UniversalSchema1)),
    ?assertEqual(EmptySet, restcheck_schema:complement(UniversalSchema2)),
    ok.

complement(_Conf) ->
    %% TODO: implement enum complement
    %% TODO: implement enum validation for non-strings
    % EnumComplement = restcheck_schema:complement(?ENUM_SCHEMA),
    % ok = generate_and_load(enum_complement, EnumComplement),
    % false = enum_complement:is_valid(1),
    % true = enum_complement:is_valid(false),

    BooleanComplement = restcheck_schema:complement(?BOOLEAN_SCHEMA),
    ok = generate_and_load(boolean_complement, BooleanComplement),
    assert_invalid(boolean_complement:is_valid(false)),
    ?assertEqual(true, boolean_complement:is_valid(1)),

    IntegerComplement = restcheck_schema:complement(?INTEGER_SCHEMA),
    ok = generate_and_load(integer_complement, IntegerComplement),
    assert_invalid(integer_complement:is_valid(3)),
    ?assertEqual(true, integer_complement:is_valid(1)),
    ?assertEqual(true, integer_complement:is_valid(true)),

    NumberComplement = restcheck_schema:complement(?NUMBER_SCHEMA),
    ok = generate_and_load(number_complement, NumberComplement),
    assert_invalid(number_complement:is_valid(5.0)),
    assert_invalid(number_complement:is_valid(5.5)),
    ?assertEqual(true, number_complement:is_valid(9.0)),
    ?assertEqual(true, number_complement:is_valid(true)),

    StringComplement = restcheck_schema:complement(?STRING_SCHEMA),
    ok = generate_and_load(string_complement, StringComplement),
    assert_invalid(string_complement:is_valid(<<"123aaa">>)),
    ?assertEqual(true, string_complement:is_valid(<<"123aa6">>)),
    ?assertEqual(true, string_complement:is_valid(<<"aaa4567">>)),
    ?assertEqual(true, string_complement:is_valid(true)),

    ArrayComplement = restcheck_schema:complement(?ARRAY_SCHEMA),
    ok = generate_and_load(array_complement, ArrayComplement),
    assert_invalid(array_complement:is_valid([5.0])),
    ?assertEqual(true, array_complement:is_valid([9.0])),
    ?assertEqual(true, array_complement:is_valid(true)),

    ObjectComplement = restcheck_schema:complement(?OBJECT_SCHEMA),
    ok = generate_and_load(object_complement, ObjectComplement),
    assert_invalid(
        object_complement:is_valid(#{<<"foo">> => 4, <<"bar">> => <<"aaa">>, <<"baz">> => true})
    ),
    ?assertEqual(
        true,
        object_complement:is_valid(#{<<"foo">> => 1, <<"bar">> => <<"aaa">>, <<"baz">> => true})
    ),
    ?assertEqual(
        true,
        object_complement:is_valid(#{<<"foo">> => 4, <<"bar">> => <<"1aa">>, <<"baz">> => true})
    ),
    ?assertEqual(true, object_complement:is_valid(true)),

    IntersectionComplement = restcheck_schema:complement(?INTERSECTION_SCHEMA),
    ok = generate_and_load(intersection_complement, IntersectionComplement),
    assert_invalid(intersection_complement:is_valid(5)),
    ?assertEqual(true, intersection_complement:is_valid(3)),
    ?assertEqual(true, intersection_complement:is_valid(true)),

    UnionComplement = restcheck_schema:complement(?UNION_SCHEMA),
    ok = generate_and_load(union_complement, UnionComplement),
    assert_invalid(union_complement:is_valid(true)),
    assert_invalid(union_complement:is_valid(<<"12aaa6">>)),
    ?assertEqual(true, union_complement:is_valid(<<"123456">>)),
    ?assertEqual(true, union_complement:is_valid(<<"aaa4567">>)),
    ?assertEqual(true, union_complement:is_valid([1, 2, 3])),

    SymmetricDifferenceComplement = restcheck_schema:complement(?SYMMETRIC_DIFFERENCE_SCHEMA),
    ok = generate_and_load(symmetric_difference_complement, SymmetricDifferenceComplement),
    assert_invalid(symmetric_difference_complement:is_valid(3.0)),
    assert_invalid(symmetric_difference_complement:is_valid(7.0)),
    ?assertEqual(true, symmetric_difference_complement:is_valid(5.0)),
    ?assertEqual(true, symmetric_difference_complement:is_valid(true)),

    ComplementComplement = restcheck_schema:complement(?COMPLEMENT_SCHEMA),
    ok = generate_and_load(complement_complement, ComplementComplement),
    assert_invalid(complement_complement:is_valid(5)),
    ?assertEqual(true, complement_complement:is_valid(true)),

    ok.

intersection(_Conf) ->
    %% TODO: implement enum validation for non-strings
    % EnumIntersection = restcheck_schema:intersection([?ENUM_SCHEMA, #{enum => [true, #{<<"foo">> => <<"bar">>}]}]),
    % ok = generate_and_load(enum_intersection, EnumIntersection),
    % false = enum_intersection:is_valid(#{<<"foo">> => <<"bar">>}),
    % false = enum_intersection:is_valid([1, 2, 3]),
    % true = enum_intersection:is_valid(true),

    BooleanIntersection = restcheck_schema:intersection([
        ?BOOLEAN_SCHEMA, #{type => boolean}
    ]),
    ok = generate_and_load(boolean_intersection, BooleanIntersection),
    assert_invalid(boolean_intersection:is_valid(<<"string">>)),
    ?assertEqual(true, boolean_intersection:is_valid(true)),

    IntegerIntersection = restcheck_schema:intersection([
        ?INTEGER_SCHEMA, #{type => integer, minimum => 4}
    ]),
    ok = generate_and_load(integer_intersection, IntegerIntersection),
    assert_invalid(integer_intersection:is_valid(3)),
    ?assertEqual(true, integer_intersection:is_valid(4)),

    NumberIntersection = restcheck_schema:intersection([
        ?NUMBER_SCHEMA, #{type => float, maximum => 10}
    ]),
    ok = generate_and_load(number_intersection, NumberIntersection),
    assert_invalid(number_intersection:is_valid(1.0)),
    ?assertEqual(true, number_intersection:is_valid(7.0)),

    StringIntersection = restcheck_schema:intersection([
        ?STRING_SCHEMA, #{type => string, pattern => <<"b{3}">>}
    ]),
    ok = generate_and_load(string_intersection, StringIntersection),
    assert_invalid(string_intersection:is_valid(<<"123aaa">>)),
    assert_invalid(string_intersection:is_valid(<<"bbb">>)),
    ?assertEqual(true, string_intersection:is_valid(<<"aaabbb">>)),

    ArrayIntersection = restcheck_schema:intersection([
        ?ARRAY_SCHEMA, #{type => array, items => ?INTEGER_SCHEMA}
    ]),
    ok = generate_and_load(array_intersection, ArrayIntersection),
    assert_invalid(array_intersection:is_valid([2])),
    assert_invalid(array_intersection:is_valid([5.0, 5.1, 5.2])),
    ?assertEqual(true, array_intersection:is_valid([5, 5, 5])),

    ObjectIntersection = restcheck_schema:intersection([
        ?OBJECT_SCHEMA, #{type => object, max_properties => 4}
    ]),
    ok = generate_and_load(object_intersection, ObjectIntersection),
    assert_invalid(
        object_intersection:is_valid(#{
            <<"foo">> => 4,
            <<"bar">> => <<"aaa">>,
            <<"baz">> => true,
            <<"foobar">> => 1,
            <<"qux">> => <<"quux">>
        })
    ),
    assert_invalid(
        object_intersection:is_valid(#{
            <<"foo">> => 4, <<"bar">> => false, <<"baz">> => true, <<"foobar">> => 1
        })
    ),
    ?assertEqual(
        true,
        object_intersection:is_valid(#{<<"foo">> => 4, <<"bar">> => <<"aaa">>, <<"baz">> => true})
    ),

    IntersectionIntersection = restcheck_schema:intersection([
        ?INTERSECTION_SCHEMA, #{all_of => [#{type => integer, minimum => 5}]}
    ]),
    ok = generate_and_load(intersection_intersection, IntersectionIntersection),
    assert_invalid(intersection_intersection:is_valid(4)),
    assert_invalid(intersection_intersection:is_valid(9)),
    ?assertEqual(true, intersection_intersection:is_valid(5)),

    UnionIntersection = restcheck_schema:intersection([
        ?UNION_SCHEMA, #{any_of => [?BOOLEAN_SCHEMA, ?NUMBER_SCHEMA]}
    ]),
    ok = generate_and_load(union_intersection, UnionIntersection),
    assert_invalid(union_intersection:is_valid(5.0)),
    assert_invalid(union_intersection:is_valid(<<"foo">>)),
    ?assertEqual(true, union_intersection:is_valid(true)),

    SymmetricDifferenceIntersection = restcheck_schema:intersection([
        ?SYMMETRIC_DIFFERENCE_SCHEMA, #{one_of => [?FLOAT_SCHEMA_A, ?STRING_SCHEMA]}
    ]),
    ok = generate_and_load(symmetric_difference_intersection, SymmetricDifferenceIntersection),
    assert_invalid(symmetric_difference_intersection:is_valid(<<"foo">>)),
    assert_invalid(symmetric_difference_intersection:is_valid(5.0)),
    ?assertEqual(true, symmetric_difference_intersection:is_valid(3.0)),

    ComplementIntersection = restcheck_schema:intersection([
        ?COMPLEMENT_SCHEMA, #{'not' => ?BOOLEAN_SCHEMA}
    ]),
    ok = generate_and_load(complement_intersection, ComplementIntersection),
    assert_invalid(complement_intersection:is_valid(true)),
    ?assertEqual(true, complement_intersection:is_valid(<<"foo">>)),

    ok.

union(_Conf) ->
    %% TODO: implement enum validation for non-strings
    % EnumUnion = restcheck_schema:union([?ENUM_SCHEMA, #{enum => [#{<<"foo">> => <<"bar">>}]}]),
    % ok = generate_and_load(enum_union, EnumUnion),
    % false = enum_union:is_valid(false),
    % true = enum_union:is_valid(#{<<"foo">> => <<"bar">>}),

    BooleanUnion = restcheck_schema:union([?BOOLEAN_SCHEMA, #{type => boolean}]),
    ok = generate_and_load(boolean_union, BooleanUnion),
    ?assertEqual(true, boolean_union:is_valid(true)),
    assert_invalid(boolean_union:is_valid(1)),

    IntegerUnion = restcheck_schema:union([
        ?INTEGER_SCHEMA, #{type => integer, minimum => 6}
    ]),
    ok = generate_and_load(integer_union, IntegerUnion),
    ?assertEqual(true, integer_union:is_valid(4)),
    ?assertEqual(true, integer_union:is_valid(7)),
    assert_invalid(integer_union:is_valid(1)),
    assert_invalid(integer_union:is_valid(true)),

    NumberUnion = restcheck_schema:union([
        ?NUMBER_SCHEMA, #{type => float, maximum => 5}
    ]),
    ok = generate_and_load(number_union, NumberUnion),
    ?assertEqual(true, number_union:is_valid(1.0)),
    ?assertEqual(true, number_union:is_valid(7.0)),
    assert_invalid(number_union:is_valid(9)),
    assert_invalid(number_union:is_valid(true)),

    StringUnion = restcheck_schema:union([
        ?STRING_SCHEMA, #{type => string, pattern => <<"b{3}">>}
    ]),
    ok = generate_and_load(string_union, StringUnion),
    ?assertEqual(true, string_union:is_valid(<<"bbb">>)),
    ?assertEqual(true, string_union:is_valid(<<"aaa">>)),
    assert_invalid(string_union:is_valid(<<"foo">>)),
    assert_invalid(string_union:is_valid(true)),

    ArrayUnion = restcheck_schema:union([
        ?ARRAY_SCHEMA, #{type => array, items => ?BOOLEAN_SCHEMA}
    ]),
    ok = generate_and_load(array_union, ArrayUnion),
    ?assertEqual(true, array_union:is_valid([true])),
    ?assertEqual(true, array_union:is_valid([5.0, 6.0, 7.9999])),
    assert_invalid(array_union:is_valid([<<"foo">>, <<"bar">>, <<"baz">>])),
    assert_invalid(array_union:is_valid(true)),

    ObjectUnion = restcheck_schema:union([
        ?OBJECT_SCHEMA,
        #{
            type => object,
            properties => #{
                <<"qux">> => #{type => boolean}
            },
            required => [<<"qux">>]
        }
    ]),
    ok = generate_and_load(object_union, ObjectUnion),
    ?assertEqual(
        true, object_union:is_valid(#{<<"foo">> => 4, <<"bar">> => <<"aaa">>, <<"baz">> => true})
    ),
    ?assertEqual(true, object_union:is_valid(#{<<"qux">> => true})),
    assert_invalid(object_union:is_valid(#{<<"foo">> => <<"foobar">>})),
    assert_invalid(object_union:is_valid(false)),

    IntersectionUnion = restcheck_schema:union([
        ?INTERSECTION_SCHEMA, #{all_of => [?BOOLEAN_SCHEMA]}
    ]),
    ok = generate_and_load(intersection_union, IntersectionUnion),
    ?assertEqual(true, intersection_union:is_valid(true)),
    ?assertEqual(true, intersection_union:is_valid(5)),
    assert_invalid(intersection_union:is_valid(5.1)),
    assert_invalid(intersection_union:is_valid(<<"foo">>)),

    UnionUnion = restcheck_schema:union([
        ?UNION_SCHEMA, #{any_of => [?INTEGER_SCHEMA, ?NUMBER_SCHEMA]}
    ]),
    ok = generate_and_load(union_union, UnionUnion),
    ?assertEqual(true, union_union:is_valid(7.9)),
    ?assertEqual(true, union_union:is_valid(true)),
    assert_invalid(union_union:is_valid(#{<<"foo">> => <<"bar">>})),

    SymmetricDifferenceUnion = restcheck_schema:union([
        ?SYMMETRIC_DIFFERENCE_SCHEMA, #{one_of => [?BOOLEAN_SCHEMA, ?STRING_SCHEMA]}
    ]),
    ok = generate_and_load(symmetric_difference_union, SymmetricDifferenceUnion),
    ?assertEqual(true, symmetric_difference_union:is_valid(4.0)),
    ?assertEqual(true, symmetric_difference_union:is_valid(true)),
    assert_invalid(symmetric_difference_union:is_valid(5.0)),
    assert_invalid(symmetric_difference_union:is_valid(#{})),

    ComplementUnion = restcheck_schema:union([?COMPLEMENT_SCHEMA, #{'not' => ?OBJECT_SCHEMA}]),
    ok = generate_and_load(complement_union, ComplementUnion),
    ?assertEqual(true, complement_union:is_valid(true)),
    ?assertEqual(
        true,
        complement_union:is_valid(#{<<"foo">> => 4, <<"bar">> => <<"aaa">>, <<"baz">> => true})
    ),
    ?assertEqual(true, complement_union:is_valid(<<"foo">>)),

    ok.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
generate_and_load(Name, Schema) ->
    DTO = ndto:generate(Name, Schema),
    ok = ndto:load(DTO).

assert_invalid(false) ->
    ok;
assert_invalid({false, _Reason}) ->
    ok;
assert_invalid(Other) ->
    ct:fail({expected_invalid, Other}).
