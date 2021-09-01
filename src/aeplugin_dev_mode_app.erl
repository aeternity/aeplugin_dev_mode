-module(aeplugin_dev_mode_app).
-behavior(application).

-export([ start/2,
          stop/1 ]).

-export([ check_env/0 ]).

start(_Type, _Args) ->
    {ok, Pid} = aep_dev_mode_sup:start_link(),
    ok = start_http_api(),
    {ok, Pid}.

stop(_State) ->
    ok.

check_env() ->
    ok.

start_http_api() ->
    Port = get_http_api_port(),
    Dispatch = cowboy_rounter:compile(
                 {'_', [{"/", aep_dev_mode_handler, []}]}),
    {ok, _} = cowboy:start_clear(devmode_listener,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}),
    ok.

get_http_api_port() ->
    list_to_integer(os:getenv("AE_DEVMODE_PORT", "3313")).
