%% -*- mode:Erlang; fill-column:79 -*-
{application, fconf,
 [{description, "Generic config file handler."},
  {vsn, "0.1.2.0"},
  {modules, [fconf_app,
             fconf_sup,
             fconf_conf_sup,
             fconf,
             fconf_engine,
             fconf_registry]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {fconf_app, []}}]}.
