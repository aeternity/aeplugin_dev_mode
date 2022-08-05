-module(aeplugin_dev_mode_app).
-behavior(application).

-export([ start/2,
          start_phase/3,
          stop/1 ]).

-export([ check_env/0 ]).

-export([ info/0 ]).

-define(PLUGIN_NAME_STR, <<"aeplugin_dev_mode">>).
-define(SCHEMA_FNAME, "aeplugin_dev_mode_config_schema.json").
-define(OS_ENV_PFX, "DEVMODE").

start(_Type, _Args) ->
    Workspace = determine_workspace(),
    lager:info("Devmode Workspace: ~p ~n", [Workspace]),
    lager:info("---------->>> We are in process: ~p ~n", [self()]),
    Lookup = ets: lookup(acc_gen_settings, generate_accounts),
    lager:info("---------->>> ETS lookup? ~p ~n", [Lookup]),
    {ok, Pid} = aeplugin_dev_mode_sup:start_link(),
    ok = start_http_api(),
    {ok, Pid}.

start_phase(check_config, _Type, _Args) ->
    case aeu_plugins:check_config(?PLUGIN_NAME_STR, ?SCHEMA_FNAME, ?OS_ENV_PFX) of
        Config when is_map(Config) ->
            apply_config(Config);
        not_found ->
            ok
    end.

stop(_State) ->
    stop_http_api(),
    ok.


determine_workspace() ->
    %% TODO: Determine some work space name for existing databases, too
    case os:getenv("AE__CHAIN__DB_PATH") of
        false -> <<"Existing Database">>;
        Path -> lists:last(filename:split(Path))
    end.


is_empty_dir(Dir) ->
    case file:list_dir_all(Dir) of
        {ok, []} -> true;
        {error, _} -> true;
        _ -> false
    end.
         


check_env() ->
    set_acc_gen_start_settings(),
    case aeu_plugins:is_dev_mode() of
        true ->
            #{pubkey := Pub} = aecore_env:patron_keypair_for_testing(),
            EncPubkey = aeser_api_encoder:encode(account_pubkey, Pub),
            aeu_plugins:suggest_config([<<"mining">>, <<"beneficiary">>], EncPubkey),
            aeu_plugins:suggest_config([<<"mining">>, <<"beneficiary_reward_delay">>], 2);
        false ->
            ok
    end,
    ok.

set_acc_gen_start_settings() ->
    case os:getenv("AE__CHAIN__DB_PATH") of
        false -> ok;
        Path -> 
            case is_empty_dir(Path) of 
                true ->
                    lager:info("---------->>> Found empty workspace, telling devmode to generate Accounts ! ~p ~n", [placeholder]),
                    lager:info("---------->>> We are in process: ~p ~n", [self()]),
                    ets:new(acc_gen_settings, [set, named_table]),
                    ets:insert(acc_gen_settings, {generate_accounts, true});
                    %% TODO: Add further account creation options here, for now use default acc generating
                false -> 
                    ok
            end
    end.


start_http_api() ->
    Port = get_http_api_port(),
    Dispatch = cowboy_router:compile(aeplugin_dev_mode_handler:routes()),
    {ok, _} = cowboy:start_clear(devmode_listener,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}),
    ok.

stop_http_api() ->
    cowboy:stop_listener(devmode_listener).

get_http_api_port() ->
    list_to_integer(os:getenv("AE_DEVMODE_PORT", "3313")).

apply_config(Config) ->
    maybe_set_keyblock_interval(Config),
    maybe_set_microblock_interval(Config),
    maybe_set_auto_emit(Config),
    ok.

info() ->
    #{ keyblock_interval => aeplugin_dev_mode_emitter:get_keyblock_interval()
     , microblock_interval => aeplugin_dev_mode_emitter:get_microblock_interval()
     , auto_demit_microblocks => aeplugin_dev_mode_emitter:get_auto_emit_microblocks()
     }.

maybe_set_keyblock_interval(#{<<"keyblock_interval">> := Interval}) ->
    aeplugin_dev_mode_emitter:set_keyblock_interval(Interval);
maybe_set_keyblock_interval(_) ->
    ok.

maybe_set_microblock_interval(#{<<"microblock_interval">> := Interval}) ->
    aeplugin_dev_mode_emitter:set_microblock_interval(Interval);
maybe_set_microblock_interval(_) ->
    ok.

maybe_set_auto_emit(#{<<"auto_emit_microblocks">> := Bool}) ->
    aeplugin_dev_mode_emitter:auto_emit_microblocks(Bool);
maybe_set_auto_emit(_) ->
    ok.
