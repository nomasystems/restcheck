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

%%% MACROS
-define(DEFAULT_RECURSION_MAX_DEPTH, 5).
-define(DEFAULT_MAX_STRING_LENGTH, 255).
-define(DEFAULT_MAX_ARRAY_ITEMS, 3).

%%% TYPES
-type recursion_max_depth() :: non_neg_integer().
-type opts() :: #{
    recursion_max_depth => recursion_max_depth(),
    max_string_length => pos_integer(),
    max_array_items => non_neg_integer()
}.

%%% EXPORT TYPES
-export_type([
    opts/0,
    recursion_max_depth/0
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec dto(Schema) -> Generator when
    Schema :: restcheck_pbt:schema(),
    Generator :: restcheck_pbt:generator().
%% @equiv dto(Schema, #{})
dto(Schema) ->
    dto(Schema, #{}).

-spec dto(Schema, Opts) -> Generator when
    Schema :: restcheck_pbt:schema(),
    Opts :: opts(),
    Generator :: restcheck_pbt:generator().
%% @doc Returns a <code>triq</code> generator of DTOs from a given schema and generation options.
dto(#{enum := _Enum} = Schema, _Opts) ->
    enum(Schema);
dto(#{type := boolean} = Schema, _Opts) ->
    boolean(Schema);
dto(#{type := integer} = Schema, _Opts) ->
    integer(Schema);
dto(#{type := float} = Schema, _Opts) ->
    number(Schema);
dto(#{type := string} = Schema, Opts) ->
    string(Schema, Opts);
dto(#{type := array} = Schema, Opts) ->
    array(Schema, Opts);
dto(#{type := object} = Schema, Opts) ->
    object(Schema, Opts);
dto(#{all_of := _Subschemas} = Schema, Opts) ->
    all_of(Schema, Opts);
dto(#{any_of := _Subschemas} = Schema, Opts) ->
    any_of(Schema, Opts);
dto(#{one_of := _Subschemas} = Schema, Opts) ->
    one_of(Schema, Opts);
dto(#{'not' := _Subschemas} = Schema, Opts) ->
    'not'(Schema, Opts);
dto(_Schema, Opts) ->
    any(Opts).

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
-spec all_of(Schema, Opts) -> Dom when
    Opts :: opts(),
    Schema :: ndto:intersection_schema(),
    Dom :: restcheck_pbt:generator().
all_of(#{all_of := Subschemas}, Opts) ->
    Schema = restcheck_schema:intersection(Subschemas),
    dto(Schema, Opts).

-spec any(Opts) -> Dom when
    Opts :: opts(),
    Dom :: restcheck_pbt:generator().
any(Opts) ->
    Subschemas =
        case max_depth(Opts) of
            0 ->
                lists:subtract(?BASIC_SCHEMAS, [#{type => array}, #{type => object}]);
            _Deeper ->
                ?BASIC_SCHEMAS
        end,
    dto(#{any_of => Subschemas}, Opts).

-spec any_of(Schema, Opts) -> Dom when
    Schema :: ndto:union_schema(),
    Opts :: opts(),
    Dom :: restcheck_pbt:generator().
any_of(#{any_of := Subschemas} = _Schema, Opts) ->
    triq_dom:oneof(
        [
            dto(Subschema, Opts)
         || Subschema <- Subschemas
        ]
    ).

-spec array(Schema, Opts) -> Dom when
    Schema :: ndto:array_schema(),
    Opts :: opts(),
    Dom :: restcheck_pbt:generator().
array(Schema, Opts) ->
    Items = maps:get(items, Schema, #{}),
    MinItems = maps:get(min_items, Schema, 0),
    DefaultMaxItems = maps:get(max_array_items, Opts, ?DEFAULT_MAX_ARRAY_ITEMS),
    MaxItems = erlang:max(MinItems, maps:get(max_items, Schema, DefaultMaxItems)),
    UniqueItems = maps:get(unique_items, Schema, false),
    triq_dom:bind(
        triq_dom:int(MinItems, MaxItems),
        fun(Length) ->
            DTO = dto(Items, deeper(Opts)),
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
enum(#{enum := Enum}) ->
    triq_dom:elements(Enum).

-spec integer(Schema) -> Dom when
    Schema :: ndto:integer_schema(),
    Dom :: restcheck_pbt:generator().
integer(Schema) ->
    RawMin =
        case maps:get(minimum, Schema, ?MIN_INT) of
            MinFloat when is_float(MinFloat) ->
                erlang:trunc(MinFloat);
            Minimum ->
                Minimum
        end,
    ExclusiveMin = maps:get(exclusive_minimum, Schema, false),
    Min =
        case ExclusiveMin of
            true ->
                RawMin + 1;
            false ->
                RawMin
        end,
    RawMax =
        case maps:get(maximum, Schema, ?MAX_INT) of
            MaxFloat when is_float(MaxFloat) ->
                erlang:trunc(MaxFloat);
            Maximum ->
                Maximum
        end,
    ExclusiveMax = maps:get(exclusive_maximum, Schema, false),
    Max =
        case ExclusiveMax of
            true ->
                RawMax - 1;
            false ->
                RawMax
        end,
    MultipleOf = maps:get(multiple_of, Schema, undefined),
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

-spec 'not'(Schema, Opts) -> Dom when
    Schema :: ndto:complement_schema(),
    Opts :: opts(),
    Dom :: restcheck_pbt:generator().
'not'(#{'not' := Subschema}, Opts) ->
    Schema = restcheck_schema:complement(Subschema),
    dto(Schema, Opts).

-spec number(Schema) -> Dom when
    Schema :: ndto:float_schema(),
    Dom :: restcheck_pbt:generator().
number(Schema) ->
    Min = maps:get(minimum, Schema, ?MIN_INT),
    ExclusiveMin = maps:get(exclusive_minimum, Schema, false),
    Max = maps:get(maximum, Schema, ?MAX_INT),
    ExclusiveMax = maps:get(exclusive_maximum, Schema, false),
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

-spec object(Schema, Opts) -> Dom when
    Schema :: ndto:object_schema(),
    Opts :: opts(),
    Dom :: restcheck_pbt:generator().
object(Schema, Opts) ->
    Properties = maps:get(properties, Schema, #{}),
    RequiredKeys = maps:get(required, Schema, []),
    RawMinProperties = maps:get(min_properties, Schema, 0),
    AdditionalProperties = maps:get(additional_properties, Schema, true),

    MinProperties =
        case erlang:length(RequiredKeys) of
            LessThan when LessThan < RawMinProperties ->
                RawMinProperties;
            GreaterThanOrEqualTo ->
                GreaterThanOrEqualTo
        end,
    MaxProperties = maps:get(max_properties, Schema, MinProperties + 1),

    Required = [{PropertyName, maps:get(PropertyName, Properties)} || PropertyName <- RequiredKeys],
    NotRequired = maps:to_list(maps:without(RequiredKeys, Properties)),

    triq_dom:bind(
        triq_dom:int(MinProperties, MaxProperties),
        fun(MissingSize) ->
            object(
                Required ++ NotRequired,
                AdditionalProperties,
                MissingSize,
                deeper(Opts),
                triq_dom:return(#{})
            )
        end
    ).

-spec object(Properties, ExtraSchema, MissingSize, Opts, Acc) -> Object when
    Properties :: [{binary(), ndto:schema()}],
    ExtraSchema :: boolean() | ndto:schema(),
    MissingSize :: non_neg_integer(),
    Opts :: opts(),
    Acc :: restcheck_pbt:generator(),
    Object :: restcheck_pbt:generator().
object(_Properties, _ExtraSchema, 0, _Opts, Acc) ->
    Acc;
object([], false, _Missing, Opts, Acc) ->
    object([], false, 0, Opts, Acc);
object([], true, Missing, Opts, Acc) ->
    object([], #{}, Missing, Opts, Acc);
object([], ExtraSchema, Missing, Opts, Acc) ->
    NewAcc =
        triq_dom:bind(
            {
                triq_dom:non_empty(triq_dom:unicode_binary()),
                dto(ExtraSchema, deeper(Opts)),
                Acc
            },
            fun({PropertyName, PropertyValue, AccValue}) ->
                maps:put(PropertyName, PropertyValue, AccValue)
            end
        ),
    object([], ExtraSchema, Missing - 1, Opts, NewAcc);
object([{PropertyName, PropertySchema} | Properties], ExtraSchema, Missing, Opts, Acc) ->
    NewAcc =
        triq_dom:bind(
            {dto(PropertySchema, deeper(Opts)), Acc},
            fun({PropertyValue, AccValue}) ->
                maps:put(PropertyName, PropertyValue, AccValue)
            end
        ),
    object(Properties, ExtraSchema, Missing - 1, Opts, NewAcc).

-spec one_of(Schema, Opts) -> Dom when
    Schema :: ndto:symmetric_difference_schema(),
    Opts :: opts(),
    Dom :: restcheck_pbt:generator().
one_of(#{one_of := Subschemas}, Opts) ->
    Schema = restcheck_schema:symmetric_difference(Subschemas),
    dto(Schema, Opts).

-spec string(Schema, Opts) -> Dom when
    Schema :: ndto:string_schema(),
    Opts :: opts(),
    Dom :: restcheck_pbt:generator().
string(#{pattern := Pattern}, _Opts) ->
    Regex = pattern_strip_anchors(unicode:characters_to_list(Pattern)),
    {AST, _Rest} = pattern_parse_alt(Regex),
    triq_dom:bind(
        pattern_gen(AST),
        fun(Codepoints) ->
            unicode:characters_to_binary(Codepoints, utf8, utf8)
        end
    );
string(Schema, Opts) ->
    MinLength = maps:get(min_length, Schema, 1),
    DefaultMaxLength = maps:get(max_string_length, Opts, ?DEFAULT_MAX_STRING_LENGTH),
    MaxLength = erlang:max(MinLength, maps:get(max_length, Schema, DefaultMaxLength)),
    Format = maps:get(format, Schema, undefined),
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
    Format :: undefined | ndto:format(),
    Length :: non_neg_integer(),
    FormatGenerator :: restcheck_pbt:generator().
string_format(undefined, Length) ->
    triq_dom:unicode_binary(Length);
string_format(base64, Length) ->
    0 = (Length rem 4),
    triq_dom:vector(
        Length,
        triq_dom:elements(base64_chars())
    );
string_format(iso8601, _Length) ->
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
-spec max_depth(Opts) -> MaxDepth when
    Opts :: opts(),
    MaxDepth :: recursion_max_depth().
max_depth(Opts) ->
    maps:get(recursion_max_depth, Opts, ?DEFAULT_RECURSION_MAX_DEPTH).

-spec deeper(Opts) -> Deeper when
    Opts :: opts(),
    Deeper :: opts().
deeper(Opts) ->
    Opts#{recursion_max_depth => max_depth(Opts) - 1}.

%%% Generate strings matching an OpenAPI `pattern` (a regular expression). We
%%% parse a common subset of regex (literals, character classes, `.`, groups,
%%% alternation and the *, +, ?, {n}, {n,}, {n,m} quantifiers) into an AST and
%%% turn it into a triq generator. Unbounded quantifiers are capped so generated
%%% strings stay small.
pattern_strip_anchors(Chars0) ->
    Chars1 =
        case Chars0 of
            [$^ | Rest] -> Rest;
            _ -> Chars0
        end,
    case lists:reverse(Chars1) of
        [$$ | RevRest] -> lists:reverse(RevRest);
        _ -> Chars1
    end.

pattern_parse_alt(Chars) ->
    {Seq, Rest} = pattern_parse_seq(Chars),
    case Rest of
        [$| | Rest1] ->
            {Next, Rest2} = pattern_parse_alt(Rest1),
            Alts =
                case Next of
                    {alt, More} -> [Seq | More];
                    _ -> [Seq, Next]
                end,
            {{alt, Alts}, Rest2};
        _ ->
            {Seq, Rest}
    end.

pattern_parse_seq(Chars) ->
    pattern_parse_seq(Chars, []).

pattern_parse_seq([], Acc) ->
    {{seq, lists:reverse(Acc)}, []};
pattern_parse_seq([C | _] = Chars, Acc) when C =:= $| orelse C =:= $) ->
    {{seq, lists:reverse(Acc)}, Chars};
pattern_parse_seq(Chars, Acc) ->
    {Term, Rest} = pattern_parse_term(Chars),
    pattern_parse_seq(Rest, [Term | Acc]).

pattern_parse_term(Chars) ->
    {Atom, Rest} = pattern_parse_atom(Chars),
    pattern_parse_quantifier(Atom, Rest).

pattern_parse_quantifier(Atom, [$* | Rest]) ->
    {{repeat, Atom, 0, 6}, Rest};
pattern_parse_quantifier(Atom, [$+ | Rest]) ->
    {{repeat, Atom, 1, 6}, Rest};
pattern_parse_quantifier(Atom, [$? | Rest]) ->
    {{repeat, Atom, 0, 1}, Rest};
pattern_parse_quantifier(Atom, [${ | Rest]) ->
    pattern_parse_brace(Atom, Rest);
pattern_parse_quantifier(Atom, Rest) ->
    {Atom, Rest}.

pattern_parse_brace(Atom, Chars) ->
    {Min, Rest1} = pattern_parse_int(Chars),
    case Rest1 of
        [$} | Rest2] ->
            {{repeat, Atom, Min, Min}, Rest2};
        [$,, $} | Rest2] ->
            {{repeat, Atom, Min, Min + 6}, Rest2};
        [$, | Rest2] ->
            {Max, Rest3} = pattern_parse_int(Rest2),
            [$} | Rest4] = Rest3,
            {{repeat, Atom, Min, Max}, Rest4}
    end.

pattern_parse_int(Chars) ->
    pattern_parse_int(Chars, []).

pattern_parse_int([C | Rest], Acc) when C >= $0 andalso C =< $9 ->
    pattern_parse_int(Rest, [C | Acc]);
pattern_parse_int(Rest, Acc) ->
    {erlang:list_to_integer(lists:reverse(Acc)), Rest}.

pattern_parse_atom([$( | Rest0]) ->
    Rest1 =
        case Rest0 of
            [$?, $: | R] -> R;
            _ -> Rest0
        end,
    {AST, Rest2} = pattern_parse_alt(Rest1),
    [$) | Rest3] = Rest2,
    {{group, AST}, Rest3};
pattern_parse_atom([$[ | Rest]) ->
    pattern_parse_class(Rest);
pattern_parse_atom([$\\, Escaped | Rest]) ->
    {pattern_escape(Escaped), Rest};
pattern_parse_atom([$. | Rest]) ->
    {{class, pattern_printable()}, Rest};
pattern_parse_atom([C | Rest]) ->
    {{lit, C}, Rest}.

pattern_parse_class([$^ | Rest]) ->
    {Set, Rest1} = pattern_parse_class_body(Rest, []),
    {{class, pattern_printable() -- Set}, Rest1};
pattern_parse_class(Rest) ->
    {Set, Rest1} = pattern_parse_class_body(Rest, []),
    {{class, Set}, Rest1}.

pattern_parse_class_body([$] | Rest], Acc) ->
    {lists:usort(lists:append(Acc)), Rest};
pattern_parse_class_body([$\\, Escaped | Rest], Acc) ->
    {class, Chars} = pattern_escape(Escaped),
    pattern_parse_class_body(Rest, [Chars | Acc]);
pattern_parse_class_body([A, $-, B | Rest], Acc) when B =/= $] ->
    pattern_parse_class_body(Rest, [lists:seq(A, B) | Acc]);
pattern_parse_class_body([C | Rest], Acc) ->
    pattern_parse_class_body(Rest, [[C] | Acc]).

pattern_escape($d) ->
    {class, lists:seq($0, $9)};
pattern_escape($w) ->
    {class, lists:seq($a, $z) ++ lists:seq($A, $Z) ++ lists:seq($0, $9) ++ [$_]};
pattern_escape($s) ->
    {class, [$\s, $\t]};
pattern_escape(C) ->
    {lit, C}.

pattern_printable() ->
    lists:seq($a, $z) ++ lists:seq($A, $Z) ++ lists:seq($0, $9).

pattern_gen({alt, Alts}) ->
    triq_dom:oneof([pattern_gen(Alt) || Alt <- Alts]);
pattern_gen({seq, Terms}) ->
    pattern_gen_seq(Terms);
pattern_gen({group, AST}) ->
    pattern_gen(AST);
pattern_gen({lit, C}) ->
    triq_dom:return([C]);
pattern_gen({class, Chars}) ->
    triq_dom:bind(
        triq_dom:elements(Chars),
        fun(C) -> triq_dom:return([C]) end
    );
pattern_gen({repeat, Term, Min, Max}) ->
    triq_dom:bind(
        triq_dom:int(Min, Max),
        fun(N) ->
            triq_dom:bind(
                triq_dom:vector(N, pattern_gen(Term)),
                fun(Lists) -> triq_dom:return(lists:append(Lists)) end
            )
        end
    ).

pattern_gen_seq([]) ->
    triq_dom:return([]);
pattern_gen_seq([Term | Terms]) ->
    triq_dom:bind(
        pattern_gen(Term),
        fun(Head) ->
            triq_dom:bind(
                pattern_gen_seq(Terms),
                fun(Tail) -> triq_dom:return(Head ++ Tail) end
            )
        end
    ).

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
