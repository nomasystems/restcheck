# restcheck
[![restcheck ci](https://github.com/nomasystems/restcheck/actions/workflows/ci.yml/badge.svg)](https://github.com/nomasystems/restcheck/actions/workflows/ci.yml)
[![restcheck docs](https://github.com/nomasystems/restcheck/actions/workflows/docs.yml/badge.svg)](https://nomasystems.github.io/restcheck)

`restcheck` is a REST API fuzzing tool based on property-based testing techniques. It provides an interface for the automatic generation and execution of integration test suites that validate the behaviour of a REST API against an OpenAPI 3.0 specification.

## Quickstart
1. Get an OpenAPI 3.0 specification for the API you want to test. For example: [users.json](examples/users/users.json).

2. Add `restcheck` as a project plugin in your `rebar3` project.
```erl
{project_plugins, [
    {restcheck, {git, "https://github.com/nomasystems/restcheck.git", {branch, "main"}}}
]}.
```

3. Configure it within the `restcheck` key of your `rebar.config`.
```erl
{restcheck, [
    {spec_path, string()},
    {spec_parser, module()}, % defaults to erf_oas_3_0
    {pbt_backend, module()}, % defaults to restcheck_triq
    {host, string()}, % defaults to "localhost"
    {port, inet:port_number()}, % defaults to 8080
    {ssl, boolean()}, % defaults to false
    {auth, restcheck_client:auth()}, % disabled by default
    {timeout, pos_integer()}, % defaults to 5000
    {num_requests, pos_integer()}, % defaults to 5000
    {log_file, string()} % disabled by default
]}.
```

4. Run `rebar3 restcheck` and check your API behaviour.
```
$ rebar3 restcheck
===> Analyzing applications...
===> Compiling restcheck
===> Running restcheck for Users REST API
+-----+--------------------------------+--------------------------------+
| OK? |                    OperationId |                    Description |
+-----+--------------------------------+--------------------------------+
|  ✅ |                    create_user |                        Success |
|  ✅ |                       get_user |                        Success |
|  ✅ |                    delete_user |                        Success |
+-----+--------------------------------+--------------------------------+
===> All restcheck tests passed for Users REST API. (3/3)
```

## Choosing a custom property-based testing framework

By default, `restcheck` provides a built-in implementation for its property-based testing features using the [`triq`](https://triq.gitlab.io/) library. However, if you have specific requirements or prefer to use a custom property-based testing framework, `restcheck` offers flexibility in selecting your own implementation.

To choose a custom property-based testing framework, follow these steps:

1. Implement the [`restcheck_backend`](src/restcheck_backend.erl) behaviour in your custom module.

2. Update the `pbt_backend` configuration key in your `rebar.config` file with the module name of your custom implementation.

## Contributing

We :heart: contributions! Please feel free to submit issues, create pull requests or just spread the word about `restcheck` in the open-source community. Don't forget to check out our [contribution guidelines](CONTRIBUTING.md) to ensure smooth collaboration! :rocket:

## Support

If you need help or have any questions, please don't hesitate to open an issue or contact the maintainers directly.

## License

`restcheck` is released under the Apache 2.0 License. For more information, please see the [LICENSE](LICENSE) file.
> This project uses OpenAPI specification (OAS) schemas and examples, which are licensed under the Apache 2.0 license. See the associated [LICENSE](priv/oas/LICENSE) file for more information.
