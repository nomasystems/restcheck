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
-module(restcheck_triq_properties).

%%% INCLUDE FILES
-include_lib("triq/include/triq.hrl").

%%%-----------------------------------------------------------------------------
%%% PROPERTIES
%%%-----------------------------------------------------------------------------
prop_any() ->
    Schema = #{},
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        is_boolean(Value) orelse
            is_integer(Value) orelse
            is_float(Value) orelse
            is_binary(Value) orelse
            is_list(Value) orelse
            is_map(Value)
    ).

prop_enum() ->
    Enum = [
        0,
        true,
        <<"foo">>,
        [1, false, <<"bar">>, #{<<"foo">> => <<"baz">>}],
        #{<<"foo">> => <<"bar">>}
    ],
    Schema = #{<<"enum">> => Enum},
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        lists:member(Value, Enum)
    ).

prop_boolean() ->
    Schema = #{<<"type">> => <<"boolean">>},
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        is_boolean(Value)
    ).

prop_integer_1() ->
    Schema = #{
        <<"type">> => <<"integer">>,
        <<"minimum">> => 0,
        <<"maximum">> => 0
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        Value =:= 0
    ).

prop_integer_2() ->
    Schema = #{
        <<"type">> => <<"integer">>,
        <<"minimum">> => 2,
        <<"exclusiveMinimum">> => false,
        <<"maximum">> => 12,
        <<"exclusiveMaximum">> => true,
        <<"multipleOf">> => 3
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        Value >= 2 andalso Value < 12 andalso (Value rem 3) =:= 0
    ).

prop_integer_3() ->
    Schema = #{
        <<"type">> => <<"integer">>,
        <<"minimum">> => -12,
        <<"exclusiveMinimum">> => true,
        <<"maximum">> => 2,
        <<"exclusiveMaximum">> => false,
        <<"multipleOf">> => -3
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        Value > -12 andalso Value =< 2 andalso (Value rem 3) =:= 0
    ).

prop_number_1() ->
    Schema = #{
        <<"type">> => <<"number">>,
        <<"minimum">> => -2.5,
        <<"exclusiveMinimum">> => true,
        <<"maximum">> => 2.5,
        <<"exclusiveMaximum">> => false
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        Value > -2.5 andalso Value =< 2.5
    ).

prop_number_2() ->
    Schema = #{
        <<"type">> => <<"number">>,
        <<"minimum">> => -2.5,
        <<"exclusiveMinimum">> => false,
        <<"maximum">> => 2.5,
        <<"exclusiveMaximum">> => true
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        Value > -2.5 andalso Value =< 2.5
    ).

prop_string() ->
    Schema = #{
        <<"type">> => <<"string">>,
        <<"minLength">> => 2,
        <<"maxLength">> => 4
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        is_binary(Value) andalso
            string:length(Value) >= 2 andalso
            string:length(Value) =< 4
    ).

prop_string_base64() ->
    Schema = #{
        <<"type">> => <<"string">>,
        <<"format">> => <<"base64">>,
        <<"minLength">> => 4,
        <<"maxLength">> => 4
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        string:length(Value) =:= 4
    ).

'prop_string_iso8601-datetime'() ->
    Schema = #{
        <<"type">> => <<"string">>,
        <<"format">> => <<"iso8601-datetime">>
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        ncalendar:is_valid(iso8601, Value)
    ).

prop_array_1() ->
    Schema = #{
        <<"type">> => <<"array">>,
        <<"items">> => #{
            <<"type">> => <<"number">>
        }
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        is_list(Value)
    ).

prop_array_2() ->
    Schema = #{
        <<"type">> => <<"array">>,
        <<"items">> => #{
            <<"type">> => <<"boolean">>
        },
        <<"minItems">> => 1,
        <<"maxItems">> => 5,
        <<"uniqueItems">> => true
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        is_list(Value) andalso erlang:length(Value) >= 1 andalso is_boolean(erlang:hd(Value)) andalso
            erlang:length(Value) =< 2
    ).

prop_object_1() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"minProperties">> => 10,
        <<"maxProperties">> => 20,
        <<"additionalProperties">> => #{<<"type">> => <<"boolean">>}
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        begin
            PropertiesLength = erlang:length(maps:keys(Value)),
            PropertiesLength >= 10 andalso
                PropertiesLength =< 20 andalso
                lists:all(fun is_boolean/1, maps:values(Value))
        end
    ).

prop_object_2() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"foo">> => #{<<"type">> => <<"integer">>},
            <<"bar">> => #{<<"type">> => <<"boolean">>}
        },
        <<"required">> => [<<"foo">>, <<"bar">>],
        <<"minProperties">> => 1,
        <<"additionalProperties">> => false
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        is_integer(maps:get(<<"foo">>, Value)) andalso is_boolean(maps:get(<<"bar">>, Value))
    ).

prop_all_of() ->
    Schema = #{
        <<"allOf">> => [
            #{<<"type">> => <<"integer">>, <<"minimum">> => 1},
            #{<<"type">> => <<"number">>, <<"maximum">> => 2.5, <<"exclusiveMaximum">> => true}
        ]
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        is_integer(Value) andalso Value >= 1 andalso Value < 2.5
    ).

prop_any_of() ->
    Schema = #{
        <<"anyOf">> => [
            #{<<"type">> => <<"boolean">>},
            #{<<"type">> => <<"number">>},
            #{<<"type">> => <<"string">>}
        ]
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        is_boolean(Value) orelse
            is_number(Value) orelse
            is_binary(Value)
    ).

prop_one_of() ->
    Schema = #{
        <<"oneOf">> => [
            #{<<"type">> => <<"boolean">>},
            #{<<"type">> => <<"string">>}
        ]
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        (is_boolean(Value) andalso not is_binary(Value)) orelse
            (not is_boolean(Value) andalso is_binary(Value))
    ).

prop_not() ->
    Schema = #{
        <<"not">> => #{<<"type">> => <<"boolean">>}
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        not is_boolean(Value)
    ).
