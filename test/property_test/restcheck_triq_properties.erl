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
    Schema = #{enum => Enum},
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        lists:member(Value, Enum)
    ).

prop_boolean() ->
    Schema = #{type => boolean},
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        is_boolean(Value)
    ).

prop_integer_1() ->
    Schema = #{
        type => integer,
        minimum => 0,
        maximum => 0
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        Value =:= 0
    ).

prop_integer_2() ->
    Schema = #{
        type => integer,
        minimum => 2,
        exclusive_minimum => false,
        maximum => 12,
        exclusive_maximum => true,
        multiple_of => 3
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        Value >= 2 andalso Value < 12 andalso (Value rem 3) =:= 0
    ).

prop_integer_3() ->
    Schema = #{
        type => integer,
        minimum => -12,
        exclusive_minimum => true,
        maximum => 2,
        exclusive_maximum => false,
        multiple_of => -3
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        Value > -12 andalso Value =< 2 andalso (Value rem 3) =:= 0
    ).

prop_number_1() ->
    Schema = #{
        type => float,
        minimum => -2.5,
        exclusive_minimum => true,
        maximum => 2.5,
        exclusive_maximum => false
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        Value > -2.5 andalso Value =< 2.5
    ).

prop_number_2() ->
    Schema = #{
        type => float,
        minimum => -2.5,
        exclusive_minimum => false,
        maximum => 2.5,
        exclusive_maximum => true
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        Value > -2.5 andalso Value =< 2.5
    ).

prop_string() ->
    Schema = #{
        type => string,
        min_length => 2,
        max_length => 4
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
        type => string,
        format => base64,
        min_length => 4,
        max_length => 4
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        string:length(Value) =:= 4
    ).

prop_string_pattern() ->
    Pattern = <<"^[0-9]{8}T[0-9]{6}([+-][0-9]{4}|Z)?$">>,
    Schema = #{
        type => string,
        pattern => Pattern
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        is_binary(Value) andalso re:run(Value, Pattern) =/= nomatch
    ).

'prop_string_iso8601-datetime'() ->
    Schema = #{
        type => string,
        format => iso8601
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        ncalendar:is_valid(iso8601, Value)
    ).

prop_array_1() ->
    Schema = #{
        type => array,
        items => #{
            type => float
        }
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        is_list(Value)
    ).

prop_array_2() ->
    Schema = #{
        type => array,
        items => #{
            type => boolean
        },
        min_items => 1,
        max_items => 5,
        unique_items => true
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        is_list(Value) andalso erlang:length(Value) >= 1 andalso is_boolean(erlang:hd(Value)) andalso
            erlang:length(Value) =< 2
    ).

prop_object_1() ->
    Schema = #{
        type => object,
        min_properties => 10,
        max_properties => 20,
        additional_properties => #{type => boolean}
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
        type => object,
        properties => #{
            <<"foo">> => #{type => integer},
            <<"bar">> => #{type => boolean}
        },
        required => [<<"foo">>, <<"bar">>],
        min_properties => 1,
        additional_properties => false
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        is_integer(maps:get(<<"foo">>, Value)) andalso is_boolean(maps:get(<<"bar">>, Value))
    ).

prop_all_of() ->
    Schema = #{
        all_of => [
            #{type => integer, minimum => 1},
            #{type => float, maximum => 2.5, exclusive_maximum => true}
        ]
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        is_integer(Value) andalso Value >= 1 andalso Value < 2.5
    ).

prop_any_of() ->
    Schema = #{
        any_of => [
            #{type => boolean},
            #{type => float},
            #{type => string}
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
        one_of => [
            #{type => boolean},
            #{type => string}
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
        'not' => #{type => boolean}
    },
    ?FORALL(
        Value,
        restcheck_triq:dto(Schema),
        not is_boolean(Value)
    ).
