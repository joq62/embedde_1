%% This is the application resource file (.app file) for the 'base'
%% application.
{application, sd_service,
[{description, "sd_service  " },
{vsn, "1.0.0" },
{modules, 
	  [sd_service_app,sd_service_sup,sd_service,sd]},
{registered,[sd_service]},
{applications, [kernel,stdlib]},
{mod, {sd_service_app,[]}},
{start_phases, []}
]}.
