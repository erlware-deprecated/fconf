%% -*- mode:Erlang; fill-column:79 -*-
{application, fconf,
 [{description, "Generic config file handler."},
  {vsn, "0.1.0"},           
  {modules, [fconf_app,   
             fconf_sup,
             fconf]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {fconf_app, []}}]}.