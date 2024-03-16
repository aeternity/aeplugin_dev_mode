-module(aeplugin_dev_mode_app).
-behavior(application).

-export([ start/2,
          start_phase/3,
          stop/1 ]).

-export([ check_env/0 ]).

-export([ info/0 ]).

-export([emitter/0]).

-include("aeplugin_dev_mode.hrl").

start(_Type, _Args) ->
    {ok, Pid} = aeplugin_dev_mode_sup:start_link(),
    ok = start_http_api(),
    {ok, Pid}.


start_phase(_Phase, _Type, _Args) ->
    ok.

stop(_State) ->
    stop_http_api(),
    ok.


check_env() ->
    aeu_plugins:check_config(?PLUGIN_NAME_STR, ?SCHEMA_FNAME, ?OS_ENV_PFX),
    WorkspaceDir = determine_workspace_dir(),
    lager:info("Active Workspace Directory: ~p ~n", [WorkspaceDir]),
    Accs = aeplugin_dev_mode_prefunded:check_accounts(WorkspaceDir),
    case Accs =/= [] of
        true ->
            lager:info("Got accs: ~p ~n", [Accs]),
            Pub = case Accs of
                [ #{pub_key := PubKey} | _ ] ->
                    PubKey;
                Other ->
                    erlang:error({invalid_devmode_data_format_found, Other})
                end, 
            maybe_set_beneficiary(enc_pub_key(Pub));
        false ->
            % if it's devmode and there are no accounts found, it can only be a synced node or
            % one that has a DB with the previous hardcoded test keypair.
            lager:info("No generated devmode accounts found. Assumingly, this is chaindata of a previously synced chain."),
            #{pubkey := Pub} = aecore_env:patron_keypair_for_testing(),
            maybe_set_beneficiary(enc_pub_key(Pub))
        end,
    ok.

enc_pub_key(Pub) ->
    aeapi:format_account_pubkey(Pub).

maybe_set_beneficiary(Pub) ->
    lager:debug("Pub = ~p", [Pub]),
    case aecore_env:is_dev_mode() of
        true ->
            aeu_plugins:suggest_config([<<"mining">>, <<"beneficiary">>], Pub),
            aeu_plugins:suggest_config([<<"mining">>, <<"beneficiary_reward_delay">>], 2);
        false ->
            ignore
    end.

determine_workspace_dir() ->
    WsName = find_config([<<"workspace_name">>], <<>>),
    WsPath = find_config([<<"workspace_path">>], aeu_plugins:data_dir(?PLUGIN_NAME_STR)),
    Dir = filename:join(WsPath, WsName),
    ok = filelib:ensure_dir(filename:join(Dir, "foo")),
    Dir.

find_config(Key, Default) ->
    {ok, Value} = aeu_plugins:find_config(?PLUGIN_NAME_STR, Key, [user_config, {value, Default}]),
    Value.


start_http_api() ->
    Port = get_http_api_port(),
    Dispatch = cowboy_router:compile(aeplugin_dev_mode_handler:routes()),
    {ok, _} = cowboy:start_clear(devmode_listener,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}),
    lager:info("Dev mode handler ready on port ~p", [Port]),
    ok.

stop_http_api() ->
    cowboy:stop_listener(devmode_listener).

get_http_api_port() ->
    list_to_integer(os:getenv("AE_DEVMODE_PORT", "3313")).

info() ->
    M = emitter(),
    #{ keyblock_interval      => M:get_keyblock_interval()
     , microblock_interval    => M:get_microblock_interval()
     , auto_emit_microblocks  => M:get_auto_emit_microblocks()
     }.

emitter() ->
    K = {?MODULE, dev_mode_emitter},
    case persistent_term:get(K, undefined) of
        undefined ->
            try aedevmode_emitter:module_info(module) of
                M ->
                    persistent_term:put(K, M)
            catch
                error:_ ->
                    Mine = aeplugin_dev_mode_emitter,
                    persistent_term:put(K, Mine),
                    Mine
            end;
        M ->
            M
    end.
