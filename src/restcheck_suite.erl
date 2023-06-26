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

%% @doc Test suite generation interface for <code>restcheck</code>.
-module(restcheck_suite).

%%% INCLUDE FILES
-include("restcheck_suite.hrl").

%%% EXTERNAL EXPORTS
-export([
    generate/1,
    load/1,
    load/2
]).

%%% TYPES
-type t() :: erl_syntax:syntaxTree().

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec generate(API) -> Result when
    API :: erf_parser:api(),
    Result :: {ModuleName, Suite},
    ModuleName :: module(),
    Suite :: t().
%% @doc Generates a <code>restcheck</code> test suite from an API AST.
generate(API) ->
    ModuleHeader = erl_syntax:comment(?COPYRIGHT ++ [?NOTE]),
    ModuleName =
        erlang:binary_to_atom(
            erf_util:to_snake_case(
                <<"restcheck_", (maps:get(name, API))/binary, "_suite">>
            )
        ),
    ModuleAttr = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(ModuleName)]),
    Schemas = maps:get(schemas, API),
    ExportedFuns = lists:flatmap(
        fun(Endpoint) ->
            Path = maps:get(path, Endpoint),
            EndpointParameters = maps:get(parameters, Endpoint),
            lists:map(
                fun(Operation) ->
                    PropName = erlang:binary_to_atom(
                        erf_util:to_snake_case(<<"prop_", (maps:get(id, Operation))/binary>>)
                    ),
                    Method = maps:get(method, Operation),
                    OperationParameters = maps:get(parameters, Operation),
                    Parameters = EndpointParameters ++ OperationParameters,
                    RequestBody = maps:get(request_body, Operation),
                    Responses = maps:get(responses, Operation),
                    {RawValues, RawGenerators} =
                        lists:foldl(
                            fun(#{ref := Ref, name := Name}, {ValuesAcc, GeneratorsAcc}) ->
                                Value = erl_syntax:variable(
                                    erlang:binary_to_atom(
                                        erf_util:to_pascal_case(Name)
                                    )
                                ),
                                Schema = schema_ast(maps:get(Ref, Schemas)),
                                Generator =
                                    erl_syntax:application(
                                        erl_syntax:atom(restcheck_pbt),
                                        erl_syntax:atom(noshrink),
                                        [
                                            erl_syntax:variable('Backend'),
                                            erl_syntax:application(
                                                erl_syntax:atom(restcheck_pbt),
                                                erl_syntax:atom(dto),
                                                [
                                                    erl_syntax:variable('Backend'),
                                                    Schema
                                                ]
                                            )
                                        ]
                                    ),
                                {[Value | ValuesAcc], [Generator | GeneratorsAcc]}
                            end,
                            {[], []},
                            Parameters
                        ),
                    RequestBodyValue = erl_syntax:variable(
                        erlang:binary_to_atom(
                            erf_util:to_pascal_case(RequestBody)
                        )
                    ),
                    RequestBodySchema = schema_ast(maps:get(RequestBody, Schemas)),
                    RequestBodyGenerator = erl_syntax:application(
                        erl_syntax:atom(restcheck_pbt),
                        erl_syntax:atom(dto),
                        [
                            erl_syntax:variable('Backend'),
                            RequestBodySchema
                        ]
                    ),
                    {Values, Generators} = {[RequestBodyValue | RawValues], [
                        RequestBodyGenerator | RawGenerators
                    ]},
                    Prop = prop_ast(Path, Method, Parameters, RequestBody, Responses),
                    erl_syntax:function(
                        erl_syntax:atom(PropName),
                        [
                            erl_syntax:clause(
                                [
                                    erl_syntax:variable('Backend'),
                                    erl_syntax:variable('ClientName'),
                                    erl_syntax:variable('Opts')
                                ],
                                none,
                                [
                                    erl_syntax:application(
                                        erl_syntax:atom('restcheck_pbt'),
                                        erl_syntax:atom('forall'),
                                        [
                                            erl_syntax:variable('Backend'),
                                            erl_syntax:tuple(Generators),
                                            erl_syntax:fun_expr([
                                                erl_syntax:clause(
                                                    [erl_syntax:tuple(Values)],
                                                    none,
                                                    Prop
                                                )
                                            ])
                                        ]
                                    )
                                ]
                            )
                        ]
                    )
                end,
                maps:get(operations, Endpoint)
            )
        end,
        maps:get(endpoints, API)
    ),
    ExportHeader = erl_syntax:comment([?EXPORTS_HEADER]),
    ExportAttr = erl_syntax:attribute(erl_syntax:atom(export), [
        erl_syntax:list([
            erl_syntax:arity_qualifier(erl_syntax:function_name(ExportedFun), erl_syntax:integer(3))
         || ExportedFun <- ExportedFuns
        ])
    ]),
    ExportHeader2 = erl_syntax:comment([?CLINE, ?EXPORTS_HEADER, ?CLINE]),

    Suite = erl_syntax:form_list(
        [
            erl_syntax:set_precomments(
                ModuleAttr,
                [ModuleHeader]
            ),
            erl_syntax:set_precomments(
                ExportAttr,
                [ExportHeader]
            ),
            erl_syntax:set_precomments(
                erl_syntax:form_list(ExportedFuns),
                [ExportHeader2]
            )
        ]
    ),
    {ModuleName, Suite}.

-spec load(Suite) -> Result when
    Suite :: t(),
    Result :: ok | {ok, Warnings} | error | {error, {Errors, Warnings}},
    Errors :: [term()],
    Warnings :: [term()].
%% @equiv load(Suite, [])
load(Suite) ->
    load(Suite, []).

-spec load(Suite, Options) -> Result when
    Suite :: t(),
    Options :: [compile:option()],
    Result :: ok | {ok, Warnings} | error | {error, {Errors, Warnings}},
    Errors :: [term()],
    Warnings :: [term()].
%% @doc Loads a <code>restcheck</code> test suite into the Erlang Runtime System
load(Suite, Options) ->
    Forms = erl_syntax:revert_forms(Suite),
    case compile:forms(Forms, Options) of
        {ok, ModuleName, Bin} when is_atom(ModuleName) andalso is_binary(Bin) ->
            case load_binary(ModuleName, Bin) of
                ok ->
                    ok;
                {error, What} ->
                    {error, {[What], []}}
            end;
        {ok, ModuleName, Bin, Warnings} when is_atom(ModuleName) andalso is_binary(Bin) ->
            case load_binary(ModuleName, Bin) of
                ok ->
                    {ok, Warnings};
                {error, What} ->
                    {error, {[What], Warnings}}
            end;
        {error, Errors, Warnings} ->
            {error, {Errors, Warnings}};
        error ->
            error
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec load_binary(ModuleName, Bin) -> Result when
    ModuleName :: atom(),
    Bin :: binary(),
    Result :: ok | {error, What},
    What :: term().
load_binary(ModuleName, Bin) ->
    case
        code:load_binary(
            ModuleName, erlang:atom_to_list(ModuleName) ++ ".erl", Bin
        )
    of
        {module, ModuleName} ->
            ok;
        {error, What} ->
            {error, What}
    end.

-spec path_ast(RawPath) -> PathAST when
    RawPath :: binary(),
    PathAST :: erl_syntax:syntaxTree().
path_ast(RawPath) ->
    path_ast(erlang:binary_to_list(RawPath), [], []).

path_ast([], Acc, []) ->
    erl_syntax:binary(
        lists:reverse(Acc)
    );
path_ast([], Acc, StepAcc) ->
    erl_syntax:binary(
        lists:reverse([
            erl_syntax:binary_field(
                erl_syntax:string(
                    lists:reverse(StepAcc)
                )
            )
            | Acc
        ])
    );
path_ast([${ | Rest], Acc, StepAcc) ->
    NewAcc = [
        erl_syntax:binary_field(
            erl_syntax:string(
                lists:reverse(StepAcc)
            )
        )
        | Acc
    ],
    path_ast(Rest, NewAcc, []);
path_ast([$} | Rest], Acc, StepAcc) ->
    NewAcc = [
        erl_syntax:binary_field(
            erl_syntax:application(
                erl_syntax:atom(uri_string),
                erl_syntax:atom(quote),
                [
                    erl_syntax:variable(
                        erlang:binary_to_atom(
                            erf_util:to_pascal_case(
                                erlang:list_to_binary(
                                    lists:reverse(StepAcc)
                                )
                            )
                        )
                    )
                ]
            ),
            [erl_syntax:atom(binary)]
        )
        | Acc
    ],
    path_ast(Rest, NewAcc, []);
path_ast([C | Rest], Acc, StepAcc) ->
    path_ast(Rest, Acc, [C | StepAcc]).

-spec prop_ast(RawPath, Method, Parameters, RequestBody, Responses) -> PropAST when
    RawPath :: binary(),
    Method :: atom(),
    Parameters :: [erf_parser:parameter()],
    RequestBody :: erf_parser:ref(),
    Responses :: #{'*' | 100..599 => erf_parser:ref()},
    PropAST :: [erl_syntax:syntaxTree()].
prop_ast(RawPath, Method, Parameters, RequestBody, Responses) ->
    Path = path_ast(RawPath),
    Headers = lists:filter(
        fun
            (#{type := header}) -> true;
            (_Parameter) -> false
        end,
        Parameters
    ),
    QueryParameters = lists:filter(
        fun
            (#{type := query}) -> true;
            (_Parameter) -> false
        end,
        Parameters
    ),
    StatusResponses = maps:filter(fun(Key, _) -> is_integer(Key) end, Responses),
    DefaultResponse =
        case maps:get('*', Responses, undefined) of
            undefined ->
                erl_syntax:atom(false);
            Ref ->
                erl_syntax:case_expr(
                    erl_syntax:application(
                        erl_syntax:atom(erlang:binary_to_atom(Ref)),
                        erl_syntax:atom(is_valid),
                        [
                            erl_syntax:variable('ResponseBody')
                        ]
                    ),
                    [
                        erl_syntax:clause(
                            [erl_syntax:atom(false)],
                            none,
                            [
                                erl_syntax:tuple([
                                    erl_syntax:atom(false),
                                    erl_syntax:tuple([
                                        erl_syntax:atom(invalid_response_body),
                                        erl_syntax:variable('ResponseBody')
                                    ])
                                ])
                            ]
                        ),
                        erl_syntax:clause(
                            [erl_syntax:atom(true)],
                            none,
                            [erl_syntax:atom(true)]
                        )
                    ]
                )
        end,
    [
        erl_syntax:match_expr(
            erl_syntax:variable('Path'),
            Path
        ),
        erl_syntax:match_expr(
            erl_syntax:variable('Method'),
            erl_syntax:atom(Method)
        ),
        erl_syntax:match_expr(
            erl_syntax:variable('Headers'),
            erl_syntax:list(
                [
                    erl_syntax:tuple([
                        erl_syntax:binary([
                            erl_syntax:binary_field(
                                erl_syntax:string(
                                    "Connection"
                                )
                            )
                        ]),
                        erl_syntax:binary([
                            erl_syntax:binary_field(
                                erl_syntax:string(
                                    "Keep-Alive"
                                )
                            )
                        ])
                    ])
                ],
                erl_syntax:list([
                    erl_syntax:tuple([
                        erl_syntax:binary([
                            erl_syntax:binary_field(
                                erl_syntax:string(
                                    erlang:binary_to_list(
                                        HeaderName
                                    )
                                )
                            )
                        ]),
                        erl_syntax:variable(
                            erlang:binary_to_atom(
                                erf_util:to_pascal_case(HeaderName)
                            )
                        )
                    ])
                 || #{name := HeaderName} <- Headers
                ])
            )
        ),
        erl_syntax:match_expr(
            erl_syntax:variable('Auth'),
            erl_syntax:application(
                erl_syntax:atom('maps'),
                erl_syntax:atom(get),
                [
                    erl_syntax:atom(auth),
                    erl_syntax:variable('Opts'),
                    erl_syntax:map_expr([])
                ]
            )
        ),
        erl_syntax:match_expr(
            erl_syntax:variable('QueryParameters'),
            erl_syntax:list([
                erl_syntax:tuple([
                    erl_syntax:binary([
                        erl_syntax:binary_field(
                            erl_syntax:string(
                                erlang:binary_to_list(
                                    QueryParameterName
                                )
                            )
                        )
                    ]),
                    erl_syntax:variable(
                        erlang:binary_to_atom(
                            erf_util:to_pascal_case(QueryParameterName)
                        )
                    )
                ])
             || #{name := QueryParameterName} <- QueryParameters
            ])
        ),
        erl_syntax:match_expr(
            erl_syntax:variable('Body'),
            erl_syntax:application(
                erl_syntax:atom('njson'),
                erl_syntax:atom(encode),
                [
                    erl_syntax:variable(
                        erlang:binary_to_atom(erf_util:to_pascal_case(RequestBody))
                    )
                ]
            )
        ),
        erl_syntax:match_expr(
            erl_syntax:variable('ReqConfig'),
            erl_syntax:map_expr([
                erl_syntax:map_field_assoc(
                    erl_syntax:atom(path),
                    erl_syntax:variable('Path')
                ),
                erl_syntax:map_field_assoc(
                    erl_syntax:atom(method),
                    erl_syntax:variable('Method')
                ),
                erl_syntax:map_field_assoc(
                    erl_syntax:atom(headers),
                    erl_syntax:variable('Headers')
                ),
                erl_syntax:map_field_assoc(
                    erl_syntax:atom(auth),
                    erl_syntax:variable('Auth')
                ),
                erl_syntax:map_field_assoc(
                    erl_syntax:atom(query_parameters),
                    erl_syntax:variable('QueryParameters')
                ),
                erl_syntax:map_field_assoc(
                    erl_syntax:atom(body),
                    erl_syntax:variable('Body')
                )
            ])
        ),
        erl_syntax:case_expr(
            erl_syntax:application(
                erl_syntax:atom('restcheck_client'),
                erl_syntax:atom(request),
                [
                    erl_syntax:variable('ClientName'),
                    erl_syntax:variable('ReqConfig'),
                    erl_syntax:variable('Opts')
                ]
            ),
            [
                erl_syntax:clause(
                    [
                        erl_syntax:tuple([
                            erl_syntax:atom(ok),
                            erl_syntax:map_expr([
                                erl_syntax:map_field_exact(
                                    erl_syntax:atom(status),
                                    erl_syntax:variable('ResponseStatus')
                                ),
                                erl_syntax:map_field_exact(
                                    erl_syntax:atom(headers),
                                    %% TODO: Check response headers
                                    erl_syntax:variable('_ResponseHeaders')
                                ),
                                erl_syntax:map_field_exact(
                                    erl_syntax:atom(body),
                                    erl_syntax:variable('ResponseBody')
                                )
                            ])
                        ])
                    ],
                    none,
                    [
                        erl_syntax:case_expr(
                            erl_syntax:variable('ResponseStatus'),
                            [
                                erl_syntax:clause(
                                    [erl_syntax:integer(400)],
                                    none,
                                    [
                                        erl_syntax:tuple([
                                            erl_syntax:atom(false),
                                            erl_syntax:tuple([
                                                erl_syntax:atom(
                                                    'bad_request'
                                                ),
                                                erl_syntax:variable('ReqConfig')
                                            ])
                                        ])
                                    ]
                                ),
                                erl_syntax:clause(
                                    [erl_syntax:integer(405)],
                                    none,
                                    [
                                        erl_syntax:tuple([
                                            erl_syntax:atom(false),
                                            erl_syntax:tuple([
                                                erl_syntax:atom(
                                                    'method_not_allowed'
                                                ),
                                                erl_syntax:tuple([
                                                    erl_syntax:variable('Path'),
                                                    erl_syntax:variable('Method')
                                                ])
                                            ])
                                        ])
                                    ]
                                ),
                                erl_syntax:clause(
                                    [erl_syntax:variable('ServerError')],
                                    erl_syntax:infix_expr(
                                        erl_syntax:variable('ServerError'),
                                        erl_syntax:operator('>='),
                                        erl_syntax:integer(500)
                                    ),
                                    [
                                        erl_syntax:tuple([
                                            erl_syntax:atom(false),
                                            erl_syntax:tuple([
                                                erl_syntax:atom(
                                                    'server_error'
                                                ),
                                                erl_syntax:variable('ServerError')
                                            ])
                                        ])
                                    ]
                                )
                                | lists:map(
                                    fun({StatusCode, Ref}) ->
                                        erl_syntax:clause(
                                            [erl_syntax:integer(StatusCode)],
                                            none,
                                            [
                                                erl_syntax:case_expr(
                                                    erl_syntax:application(
                                                        erl_syntax:atom(erlang:binary_to_atom(Ref)),
                                                        erl_syntax:atom(is_valid),
                                                        [
                                                            erl_syntax:variable('ResponseBody')
                                                        ]
                                                    ),
                                                    [
                                                        erl_syntax:clause(
                                                            [erl_syntax:atom(false)],
                                                            none,
                                                            [
                                                                erl_syntax:tuple([
                                                                    erl_syntax:atom(
                                                                        'invalid_response_body'
                                                                    ),
                                                                    erl_syntax:variable(
                                                                        'ResponseBody'
                                                                    )
                                                                ])
                                                            ]
                                                        ),
                                                        erl_syntax:clause(
                                                            [erl_syntax:atom(true)],
                                                            none,
                                                            [erl_syntax:atom(true)]
                                                        )
                                                    ]
                                                )
                                            ]
                                        )
                                    end,
                                    maps:to_list(StatusResponses)
                                )
                            ] ++
                                [
                                    erl_syntax:clause(
                                        [erl_syntax:variable('_Default')],
                                        none,
                                        [
                                            DefaultResponse
                                        ]
                                    )
                                ]
                        )
                    ]
                ),
                erl_syntax:clause(
                    [erl_syntax:tuple([erl_syntax:atom(error), erl_syntax:variable('Reason')])],
                    none,
                    [
                        erl_syntax:tuple([
                            erl_syntax:atom(false),
                            erl_syntax:tuple([
                                erl_syntax:atom(http_request_failed), erl_syntax:variable('Reason')
                            ])
                        ])
                    ]
                )
            ]
        )
    ].

-spec schema_ast(Schema) -> SchemaAST when
    Schema :: erf_parser:schema(),
    SchemaAST :: erl_syntax:syntaxTree().
schema_ast(Schema) ->
    erl_syntax:abstract(Schema).
