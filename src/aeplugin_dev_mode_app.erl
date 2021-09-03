-module(aeplugin_dev_mode_app).
-behavior(application).

-export([ start/2,
          stop/1 ]).

-export([ check_env/0 ]).

start(_Type, _Args) ->
    {ok, Pid} = aeplugin_dev_mode_sup:start_link(),
    ok = start_http_api(),
    {ok, Pid}.

stop(_State) ->
    ok.

check_env() ->
    case aec_conductor:get_beneficiary() of
        {ok, _} ->
            ok;
        {error, beneficiary_not_configured} ->
            lager:warning("Beneficiary not configured. Dev mode may not work", [])
    end,
    ok.

start_http_api() ->
    Port = get_http_api_port(),
    Handler = aeplugin_dev_mode_handler,
    Dispatch = cowboy_router:compile(
                 [
                   {'_', [{"/", Handler, []},
                          {"/emit_kb/", Handler, []}]}
                 ]),
    {ok, _} = cowboy:start_clear(devmode_listener,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}),
    ok.

get_http_api_port() ->
    list_to_integer(os:getenv("AE_DEVMODE_PORT", "3313")).
