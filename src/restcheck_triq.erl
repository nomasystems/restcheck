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

%%% INCLUDE FILES
-include("restcheck_schema.hrl").

%%% BEHAVIOURS
-behaviour(restcheck_backend).

%%% EXTERNAL EXPORTS
-export([
    dto/1,
    dto/2,
    forall/2,
    noshrink/1,
    quickcheck/3
]).

%%% TRIQ REPORTER EXPORTS
-export([
    report/2,
    report/3
]).

%%% TYPES
-type recursion_max_depth() :: non_neg_integer().

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec dto(Schema) -> Generator when
    Schema :: restcheck_pbt:schema(),
    Generator :: restcheck_pbt:generator().
%% @equiv dto(Schema, 5)
dto(Schema) ->
    dto(Schema, 5).

-spec dto(Schema, MaxDepth) -> Generator when
    MaxDepth :: recursion_max_depth(),
    Schema :: restcheck_pbt:schema(),
    Generator :: restcheck_pbt:generator().
%% @doc Returns a <code>triq</code> generator of DTOs from a given schema and maximum recursion depth.
dto(#{<<"enum">> := _Enum} = Schema, _MaxDepth) ->
    enum(Schema);
dto(#{<<"type">> := <<"boolean">>} = Schema, _MaxDepth) ->
    boolean(Schema);
dto(#{<<"type">> := <<"integer">>} = Schema, _MaxDepth) ->
    integer(Schema);
dto(#{<<"type">> := <<"number">>} = Schema, _MaxDepth) ->
    number(Schema);
dto(#{<<"type">> := <<"string">>} = Schema, _MaxDepth) ->
    string(Schema);
dto(#{<<"type">> := <<"array">>} = Schema, MaxDepth) ->
    array(Schema, MaxDepth);
dto(#{<<"type">> := <<"object">>} = Schema, MaxDepth) ->
    object(Schema, MaxDepth);
dto(#{<<"allOf">> := _Subschemas} = Schema, MaxDepth) ->
    all_of(Schema, MaxDepth);
dto(#{<<"anyOf">> := _Subschemas} = Schema, MaxDepth) ->
    any_of(Schema, MaxDepth);
dto(#{<<"oneOf">> := _Subschemas} = Schema, MaxDepth) ->
    one_of(Schema, MaxDepth);
dto(#{<<"not">> := _Subschemas} = Schema, MaxDepth) ->
    'not'(Schema, MaxDepth);
dto(_Schema, MaxDepth) ->
    any(MaxDepth).

-spec forall(Generators, Prop) -> ForAll when
    Generators :: [restcheck_pbt:generator()],
    Prop :: restcheck_pbt:prop(),
    ForAll :: restcheck_pbt:property().
%% @doc Wraps a <code>forall</code> property in <code>triq</code> format.
forall(Generators, Prop) ->
    %% TODO: explore ways to stringify prop patterns and body
    {'prop:forall', Generators, "Generated", Prop, "begin property_body end"}.

-spec noshrink(Generator) -> NoShrinkGenerator when
    Generator :: restcheck_pbt:generator(),
    NoShrinkGenerator :: restcheck_pbt:generator().
%% @doc Prevents a <code>triq</code> generator from shrinking.
noshrink(Generator) ->
    triq_dom:noshrink(Generator).

-spec quickcheck(Property, NumTests, OutputFun) -> Result when
    Property :: restcheck_pbt:property(),
    NumTests :: restcheck_pbt:num_tests(),
    OutputFun :: restcheck_pbt:output_fun(),
    Result :: ok | {error, Reason},
    Reason :: term().
%% @doc Runs a property-based test using <code>triq</code>.
quickcheck(Property, NumTests, OutputFun) ->
    application:set_env(triq, reporter_module, ?MODULE),
    application:set_env(triq, reporter_output_fun, OutputFun),
    triq:quickcheck({'prop:numtests', NumTests, Property}).

%%%-----------------------------------------------------------------------------
%%% TRIQ REPORTER EXPORTS
%%%-----------------------------------------------------------------------------
-spec report(Event, Term) -> ok when
    Event :: testing | pass | skip | fail | check_failed | counterexample | success,
    Term :: term().
%% @private
report(Event, Term) ->
    Fun = application:get_env(triq, reporter_output_fun, fun io:format/2),
    do_report(Fun, Event, Term).

-spec report(Event, Term, IsShrinking) -> ok when
    Event :: testing | pass | skip | fail | check_failed | counterexample | success,
    Term :: term(),
    IsShrinking :: boolean().
%% @private
report(_Subject, _Data, true) -> ok;
report(Subject, Data, false) -> report(Subject, Data).

%%%-----------------------------------------------------------------------------
%%% GENERATORS
%%%-----------------------------------------------------------------------------
-spec all_of(Schema, MaxDepth) -> Dom when
    MaxDepth :: recursion_max_depth(),
    Schema :: ndto:intersection_schema(),
    Dom :: restcheck_pbt:generator().
all_of(#{<<"allOf">> := Subschemas}, MaxDepth) ->
    Schema = restcheck_schema:intersection(Subschemas),
    dto(Schema, MaxDepth).

-spec any(MaxDepth) -> Dom when
    MaxDepth :: recursion_max_depth(),
    Dom :: restcheck_pbt:generator().
any(0) ->
    Schema = #{
        <<"anyOf">> => lists:subtract(?BASIC_SCHEMAS, [
            #{<<"type">> => <<"array">>}, #{<<"type">> => <<"object">>}
        ])
    },
    dto(Schema, 0);
any(MaxDepth) ->
    Schema = #{<<"anyOf">> => ?BASIC_SCHEMAS},
    dto(Schema, MaxDepth).

-spec any_of(Schema, MaxDepth) -> Dom when
    Schema :: ndto:union_schema(),
    MaxDepth :: recursion_max_depth(),
    Dom :: restcheck_pbt:generator().
any_of(#{<<"anyOf">> := Subschemas} = _Schema, MaxDepth) ->
    triq_dom:oneof(
        [
            dto(Subschema, MaxDepth)
         || Subschema <- Subschemas
        ]
    ).

-spec array(Schema, MaxDepth) -> Dom when
    Schema :: ndto:array_schema(),
    MaxDepth :: recursion_max_depth(),
    Dom :: restcheck_pbt:generator().
array(Schema, MaxDepth) ->
    Items = maps:get(<<"items">>, Schema, #{}),
    MinItems = maps:get(<<"minItems">>, Schema, 0),
    MaxItems = maps:get(<<"maxItems">>, Schema, 3),
    UniqueItems = maps:get(<<"uniqueItems">>, Schema, false),
    triq_dom:bind(
        triq_dom:int(MinItems, MaxItems),
        fun(Length) ->
            DTO = dto(Items, MaxDepth - 1),
            Array = triq_dom:vector(Length, DTO),
            case UniqueItems of
                false ->
                    Array;
                true ->
                    triq_dom:suchthat(
                        triq_dom:bind(
                            Array,
                            fun lists:uniq/1
                        ),
                        fun(A) ->
                            erlang:length(A) >= MinItems
                        end
                    )
            end
        end
    ).

-spec boolean(Schema) -> Dom when
    Schema :: ndto:boolean_schema(),
    Dom :: restcheck_pbt:generator().
boolean(_Schema) ->
    triq_dom:bool().

-spec enum(Schema) -> Dom when
    Schema :: ndto:enum_schema(),
    Dom :: restcheck_pbt:generator().
enum(#{<<"enum">> := Enum}) ->
    triq_dom:elements(Enum).

-spec integer(Schema) -> Dom when
    Schema :: ndto:integer_schema(),
    Dom :: restcheck_pbt:generator().
integer(Schema) ->
    RawMin =
        case maps:get(<<"minimum">>, Schema, ?MIN_INT) of
            MinFloat when is_float(MinFloat) ->
                erlang:trunc(MinFloat);
            Minimum ->
                Minimum
        end,
    ExclusiveMin = maps:get(<<"exclusiveMinimum">>, Schema, false),
    Min =
        case ExclusiveMin of
            true ->
                RawMin + 1;
            false ->
                RawMin
        end,
    RawMax =
        case maps:get(<<"maximum">>, Schema, ?MAX_INT) of
            MaxFloat when is_float(MaxFloat) ->
                erlang:trunc(MaxFloat);
            Maximum ->
                Maximum
        end,
    ExclusiveMax = maps:get(<<"exclusiveMaximum">>, Schema, false),
    Max =
        case ExclusiveMax of
            true ->
                RawMax - 1;
            false ->
                RawMax
        end,
    MultipleOf = maps:get(<<"multipleOf">>, Schema, undefined),
    case MultipleOf of
        undefined ->
            triq_dom:int(Min, Max);
        _Otherwise ->
            case multiples(MultipleOf, Min, Max) of
                [] ->
                    erlang:throw({error, {invalid_schema, Schema}});
                Multiples ->
                    triq_dom:elements(Multiples)
            end
    end.

-spec 'not'(Schema, MaxDepth) -> Dom when
    Schema :: ndto:complement_schema(),
    MaxDepth :: recursion_max_depth(),
    Dom :: restcheck_pbt:generator().
'not'(#{<<"not">> := Subschema}, MaxDepth) ->
    Schema = restcheck_schema:complement(Subschema),
    dto(Schema, MaxDepth).

-spec number(Schema) -> Dom when
    Schema :: ndto:number_schema(),
    Dom :: restcheck_pbt:generator().
number(Schema) ->
    Min = maps:get(<<"minimum">>, Schema, ?MIN_INT),
    ExclusiveMin = maps:get(<<"exclusiveMinimum">>, Schema, false),
    Max = maps:get(<<"maximum">>, Schema, ?MAX_INT),
    ExclusiveMax = maps:get(<<"exclusiveMaximum">>, Schema, false),
    Integer = triq_dom:int(erlang:trunc(Min), erlang:trunc(Max)),
    Float = triq_dom:bind(
        triq_dom:int(?MAX_INT),
        fun(Int) ->
            Min + (Max - Min) * (Int / ?MAX_INT)
        end
    ),
    RawNumber = triq_dom:oneof([Integer, Float]),
    RawNumberMin =
        case ExclusiveMin of
            true ->
                triq_dom:suchthat(RawNumber, fun(Number) -> Number > Min end);
            false ->
                RawNumber
        end,
    case ExclusiveMax of
        true ->
            triq_dom:suchthat(RawNumber, fun(Number) -> Number < Max end);
        false ->
            RawNumberMin
    end.

-spec object(Schema, MaxDepth) -> Dom when
    Schema :: ndto:object_schema(),
    MaxDepth :: recursion_max_depth(),
    Dom :: restcheck_pbt:generator().
object(Schema, MaxDepth) ->
    Properties = maps:get(<<"properties">>, Schema, #{}),
    RequiredKeys = maps:get(<<"required">>, Schema, []),
    RawMinProperties = maps:get(<<"minProperties">>, Schema, 0),
    MaxProperties = maps:get(<<"maxProperties">>, Schema, 3),
    AdditionalProperties = maps:get(<<"additionalProperties">>, Schema, true),

    MinProperties =
        case erlang:length(RequiredKeys) of
            LessThan when LessThan < RawMinProperties ->
                RawMinProperties;
            GreaterThanOrEqualTo ->
                GreaterThanOrEqualTo
        end,
    Required = [{PropertyName, maps:get(PropertyName, Properties)} || PropertyName <- RequiredKeys],
    NotRequired = maps:to_list(maps:without(RequiredKeys, Properties)),

    triq_dom:bind(
        triq_dom:int(MinProperties, MaxProperties),
        fun(MissingSize) ->
            object(
                Required ++ NotRequired,
                AdditionalProperties,
                MissingSize,
                MaxDepth - 1,
                triq_dom:return(#{})
            )
        end
    ).

-spec object(Properties, ExtraSchema, MissingSize, MaxDepth, Acc) -> Object when
    Properties :: [{binary(), ndto:schema()}],
    ExtraSchema :: boolean() | ndto:schema(),
    MissingSize :: non_neg_integer(),
    MaxDepth :: recursion_max_depth(),
    Acc :: restcheck_pbt:generator(),
    Object :: restcheck_pbt:generator().
object(_Properties, _ExtraSchema, 0, _MaxDepth, Acc) ->
    Acc;
object([], false, _Missing, MaxDepth, Acc) ->
    object([], false, 0, MaxDepth, Acc);
object([], true, Missing, MaxDepth, Acc) ->
    object([], #{}, Missing, MaxDepth, Acc);
object([], ExtraSchema, Missing, MaxDepth, Acc) ->
    NewAcc =
        triq_dom:bind(
            {triq_dom:non_empty(triq_dom:unicode_binary()), dto(ExtraSchema, MaxDepth - 1), Acc},
            fun({PropertyName, PropertyValue, AccValue}) ->
                maps:put(PropertyName, PropertyValue, AccValue)
            end
        ),
    object([], ExtraSchema, Missing - 1, MaxDepth, NewAcc);
object([{PropertyName, PropertySchema} | Properties], ExtraSchema, Missing, MaxDepth, Acc) ->
    NewAcc =
        triq_dom:bind(
            {dto(PropertySchema, MaxDepth - 1), Acc},
            fun({PropertyValue, AccValue}) ->
                maps:put(PropertyName, PropertyValue, AccValue)
            end
        ),
    object(Properties, ExtraSchema, Missing - 1, MaxDepth, NewAcc).

-spec one_of(Schema, MaxDepth) -> Dom when
    Schema :: ndto:symmetric_difference_schema(),
    MaxDepth :: recursion_max_depth(),
    Dom :: restcheck_pbt:generator().
one_of(#{<<"oneOf">> := Subschemas}, MaxDepth) ->
    Schema = restcheck_schema:symmetric_difference(Subschemas),
    dto(Schema, MaxDepth).

-spec string(Schema) -> Dom when
    Schema :: ndto:string_schema(),
    Dom :: restcheck_pbt:generator().
string(#{<<"pattern">> := _Pattern}) ->
    %% TODO: implement pattern
    erlang:throw({restcheck_triq, pattern, not_implemented});
string(Schema) ->
    MinLength = maps:get(<<"minLength">>, Schema, 1),
    MaxLength = maps:get(<<"maxLength">>, Schema, 255),
    Format = maps:get(<<"format">>, Schema, undefined),
    triq_dom:bind(
        triq_dom:int(MinLength, MaxLength),
        fun(Length) ->
            triq_dom:bind(
                string_format(Format, Length),
                fun(Vector) ->
                    unicode:characters_to_binary(Vector, utf8, utf8)
                end
            )
        end
    ).

-spec string_format(Format, Length) -> FormatGenerator when
    Format :: undefined | binary(),
    Length :: non_neg_integer(),
    FormatGenerator :: restcheck_pbt:generator().
string_format(undefined, Length) ->
    triq_dom:vector(
        Length,
        triq_dom:unicode_char()
    );
string_format(<<"base64">>, Length) ->
    0 = (Length rem 4),
    triq_dom:vector(
        Length,
        triq_dom:elements(base64_chars())
    );
string_format(<<"iso8601-datetime">>, _Length) ->
    triq_dom:bind(
        {
            triq_dom:int(9999),
            triq_dom:int(1, 12),
            triq_dom:int(23),
            triq_dom:int(59),
            triq_dom:int(59),
            triq_dom:elements(timezones())
        },
        fun({Year, Month, Hour, Min, Second, Timezone}) ->
            MaxDay =
                case Month of
                    2 ->
                        case calendar:is_leap_year(Year) of
                            true ->
                                29;
                            _false ->
                                28
                        end;
                    Thirty when
                        Thirty =:= 4 orelse
                            Thirty =:= 6 orelse
                            Thirty =:= 9 orelse
                            Thirty =:= 11
                    ->
                        30;
                    _Otherwise ->
                        31
                end,
            triq_dom:bind(
                triq_dom:int(1, MaxDay),
                fun(Day) ->
                    unicode:characters_to_binary(
                        io_lib:format("~4..0B~2..0B~2..0BT~2..0B~2..0B~2..0B~s", [
                            Year, Month, Day, Hour, Min, Second, Timezone
                        ])
                    )
                end
            )
        end
    ).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
base64_chars() ->
    lists:append(
        [
            [43, 47, 61],
            lists:seq(48, 57),
            lists:seq(65, 90),
            lists:seq(97, 122)
        ]
    ).

do_report(Fun, testing, [Module, Fun]) ->
    Fun("Testing ~p:~p/0~n", [Module, Fun]);
do_report(Fun, pass, _) ->
    Fun(".", []);
do_report(Fun, skip, _) ->
    Fun("x", []);
do_report(Fun, fail, false) ->
    Fun("Failed!~n", []);
do_report(Fun, fail, Value) ->
    Fun("Failed with: ~p~n", [Value]);
do_report(Fun, check_failed, [Count, Error]) ->
    Fun("~nFailed after ~p tests with ~p~n", [Count, Error]);
do_report(Fun, counterexample, CounterExample) ->
    Fun("Simplified:~n", []),
    lists:foreach(
        fun({Syntax, _Fun, Val, _Dom}) ->
            Fun("\t~s = ~w~n", [Syntax, Val])
        end,
        CounterExample
    );
do_report(Fun, success, Count) ->
    Fun("~nRan ~p tests~n", [Count]).

multiples(MultipleOf, undefined, Max) ->
    multiples(MultipleOf, ?MIN_INT, Max);
multiples(MultipleOf, Min, undefined) ->
    multiples(MultipleOf, Min, ?MAX_INT);
multiples(MultipleOf, Min, Max) when MultipleOf =< 0 ->
    multiples(MultipleOf * -1, Min, Max);
multiples(MultipleOf, Min, Max) ->
    FirstMultiple = MultipleOf * ((Min + MultipleOf - 1) div MultipleOf),
    multiples(MultipleOf, Max, FirstMultiple, []).

multiples(_MultipleOf, Max, Current, Acc) when Current > Max ->
    lists:reverse(Acc);
multiples(MultipleOf, Max, Current, Acc) ->
    multiples(MultipleOf, Max, Current + MultipleOf, [Current | Acc]).

timezones() ->
    [
        <<"-1200">>,
        <<"-1100">>,
        <<"-1000">>,
        <<"-0930">>,
        <<"-0900">>,
        <<"-0800">>,
        <<"-0700">>,
        <<"-0600">>,
        <<"-0500">>,
        <<"-0430">>,
        <<"-0400">>,
        <<"-0330">>,
        <<"-0300">>,
        <<"-0230">>,
        <<"-0200">>,
        <<"-0100">>,
        <<"+0000">>,
        <<"+0100">>,
        <<"+0200">>,
        <<"+0300">>,
        <<"+0330">>,
        <<"+0400">>,
        <<"+0430">>,
        <<"+0500">>,
        <<"+0530">>,
        <<"+0545">>,
        <<"+0600">>,
        <<"+0630">>,
        <<"+0700">>,
        <<"+0730">>,
        <<"+0800">>,
        <<"+0900">>,
        <<"+0930">>,
        <<"+1000">>,
        <<"+1030">>,
        <<"+1100">>,
        <<"+1130">>,
        <<"+1200">>,
        <<"+1245">>,
        <<"+1300">>,
        <<"+1345">>,
        <<"+1400">>
    ].
