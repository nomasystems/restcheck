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
-module(restcheck_triq_SUITE).

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        any,
        enum,
        boolean,
        {group, integer},
        {group, number},
        {group, string},
        {group, array},
        {group, object},
        all_of,
        any_of,
        one_of,
        'not'
    ].

groups() ->
    [
        {integer, [parallel], [
            integer_1,
            integer_2,
            integer_3
        ]},
        {number, [parallel], [
            number_1,
            number_2
        ]},
        {string, [parallel], [
            string,
            string_base64,
            'string_iso8601-datetime'
        ]},
        {array, [parallel], [
            array_1,
            array_2
        ]},
        {object, [parallel], [
            object_1,
            object_2
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
any(Conf) ->
    ct_property_test:quickcheck(
        restcheck_triq_properties:prop_any(),
        Conf
    ).

enum(Conf) ->
    ct_property_test:quickcheck(
        restcheck_triq_properties:prop_enum(),
        Conf
    ).

boolean(Conf) ->
    ct_property_test:quickcheck(
        restcheck_triq_properties:prop_boolean(),
        Conf
    ).

integer_1(Conf) ->
    ct_property_test:quickcheck(
        restcheck_triq_properties:prop_integer_1(),
        Conf
    ).

integer_2(Conf) ->
    ct_property_test:quickcheck(
        restcheck_triq_properties:prop_integer_2(),
        Conf
    ).

integer_3(Conf) ->
    ct_property_test:quickcheck(
        restcheck_triq_properties:prop_integer_3(),
        Conf
    ).

number_1(Conf) ->
    ct_property_test:quickcheck(
        restcheck_triq_properties:prop_number_1(),
        Conf
    ).

number_2(Conf) ->
    ct_property_test:quickcheck(
        restcheck_triq_properties:prop_number_2(),
        Conf
    ).

string(Conf) ->
    ct_property_test:quickcheck(
        restcheck_triq_properties:prop_string(),
        Conf
    ).

string_base64(Conf) ->
    ct_property_test:quickcheck(
        restcheck_triq_properties:prop_string_base64(),
        Conf
    ).

'string_iso8601-datetime'(Conf) ->
    ct_property_test:quickcheck(
        restcheck_triq_properties:'prop_string_iso8601-datetime'(),
        Conf
    ).

array_1(Conf) ->
    ct_property_test:quickcheck(
        restcheck_triq_properties:prop_array_1(),
        Conf
    ).

array_2(Conf) ->
    ct_property_test:quickcheck(
        restcheck_triq_properties:prop_array_2(),
        Conf
    ).

object_1(Conf) ->
    ct_property_test:quickcheck(
        restcheck_triq_properties:prop_object_1(),
        Conf
    ).

object_2(Conf) ->
    ct_property_test:quickcheck(
        restcheck_triq_properties:prop_object_2(),
        Conf
    ).

all_of(Conf) ->
    ct_property_test:quickcheck(
        restcheck_triq_properties:prop_all_of(),
        Conf
    ).

any_of(Conf) ->
    ct_property_test:quickcheck(
        restcheck_triq_properties:prop_any_of(),
        Conf
    ).

one_of(Conf) ->
    ct_property_test:quickcheck(
        restcheck_triq_properties:prop_one_of(),
        Conf
    ).

'not'(Conf) ->
    ct_property_test:quickcheck(
        restcheck_triq_properties:prop_not(),
        Conf
    ).
