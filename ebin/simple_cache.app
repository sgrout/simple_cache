%%%-------------------------------------------------------------------
%%% @author sgrout
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2018 17:35
%%%-------------------------------------------------------------------
{application, simple_cache, [
  {description, "A simple caching system"},
  {vsn, "0.1.0"},
  {modules, [
    sc_app,
    sc_sup]
  },
  {registered, [sc_sup]},
  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {sc_app, []}},
  {env, []}
]}.