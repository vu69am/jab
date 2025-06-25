# p1_utils

[![CI](https://github.com/processone/p1_utils/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/processone/p1_utils/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/processone/p1_utils/badge.svg?branch=master&service=github)](https://coveralls.io/github/processone/p1_utils?branch=master)
[![Hex version](https://img.shields.io/hexpm/v/p1_utils.svg "Hex version")](https://hex.pm/packages/p1_utils)

p1_utils is an application containing ProcessOne modules and tools that are leveraged in other development projects:

* `p1_fsm` and `p1_server` are drop-in replacements of Erlang gen_fsm and gen_server, offering extra option for better 
  reliability in production. They support mostly priority messages and message queue length controls.
* `p1_nif_utils` is an helper utilities for handling NIF code.
* `treap` is a treap algorithm implementation. It is a randomized binary search tree. See: https://en.wikipedia.org/wiki/Treap
* `p1_time_compat` is a module to ease support and migration of Erlang
  time management function from Erlang R16/R17 to Erlang R18.
* `p1_http` is an http client which provides a common API for inets / lhttpc / ibrowse
* `p1_proxy_protocol` decodes HAproxy protocol (v1 and v2) headers.

If you have `rebar` binary, you can generate `p1_utils` documentation by running `rebar3 edoc`.

## EDoc documentation

You can check this library's 
[EDoc documentation](edoc.html), 
generated automatically from the source code comments.
