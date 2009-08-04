{ application, friendfs,
  [
    { description, "Distributed filesystem for Erlang." },
    { vsn, "0.1.0" },
    { modules, [ friendfs ] },
    { registered, [  ] },
    { applications, [ kernel, stdlib, fuserl ] },
    { mod, { friendfs, [] } },
    { env, [ { linked_in, false }, 
             { mount_point, "/mnt/ffs" }, 
	     { mount_opts, "allow_other,default_permissions" }] }
  ]
}.
