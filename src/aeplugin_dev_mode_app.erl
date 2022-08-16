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
    % Workspace = determine_workspace(),
    % lager:info("Devmode Workspace: ~p ~n", [Workspace]),
    % Lookup = ets:lookup(acc_gen_settings, generate_accounts),
    % lager:info("---------->>> ETS lookup? ~p ~n", [Lookup]),

    % check_for_prefunded_accounts(),
    {ok, Pid} = aeplugin_dev_mode_sup:start_link(),
    ok = start_http_api(),
    {ok, Pid}.


% check_for_prefunded_accounts() ->
%     % Save to ets table if present, set empty value if not available.
%     AccountData = case try_reading_prefunded_accounts() of
%                 not_found -> not_defined;
%                 Accs -> Accs
%                end.

%             Process account data and store it in ETS !


try_reading_prefunded_accounts() ->
    Workspace = lists:last(filename:split(os:getenv("AE__CHAIN__DB_PATH"))),
    io:fwrite("---------->>> Workspace: ~p ~n", [Workspace]),        
    FilePath = filename:join(os:getenv("AE__CHAIN__DB_PATH"), "devmode_accs_" ++ Workspace ++ ".json"),
    io:fwrite("---------->>> Final calculated accounts file path: ~p ~n", [FilePath]),
    case file:read_file(FilePath) of
        {ok, Accs} -> Accs;
        {error, _} -> not_found
    end.
        
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
    % set_acc_gen_start_settings(),

    %% make acc gen code available:
% TODO: check if this is necessary
    % case os:getenv("AE__CHAIN__DB_PATH") of
    %     false -> ok;
    %     Path ->
    %         code:add_patha(Path)
    % end,

    % Seed = ebip39:mnemonic_to_seed(ebip39:generate_mnemonic(128), <<"">>),
    % lager:info("===================>>>> seed? ~p ~n", [Seed]),

    Workspace = determine_workspace(),
    maybe_generate_accounts(Workspace),



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

% Tried to create an everlasting ETS table to circumvent the unavailability of module
% 's in Ulf's lifecycle hook functions, the child process that the ETS ownership was handed
%  to kept dying for some reason though..

% set_acc_gen_start_settings() ->
%     case os:getenv("AE__CHAIN__DB_PATH") of
%         false -> ok;
%         Path -> 
%             case is_empty_dir(Path) of 
%                 true ->
%                     lager:info("---------->>> Found empty workspace, telling devmode to generate Accounts ! ~p ~n", [placeholder]),
%                     lager:info("---------->>> We are in process: ~p ~n", [self()]),
%                     % ETSowner = spawn(aeplugin_dev_mode_app, ets_owner_fun,[]),                    
%                     % Everlasting = fun(F) -> receive _ -> F end end,

%                     Everlasting = fun Ever() -> receive Msg -> io:fwrite("------> Received: ~p ~n", [Msg]), Ever end end,
%                     % ETSowner = spawn(Everlasting(Everlasting)),
%                     ETSowner = spawn(Everlasting),
%                     lager:info("---------->>> Spawned ETS owner: ~p ~n", [ETSowner]),
%                     ets:new(acc_gen_settings, [set, named_table]),
%                     ets:insert(acc_gen_settings, {generate_accounts, true}),
%                     ets:give_away(acc_gen_settings, ETSowner, none),

%                     LookupTest = ets:lookup(acc_gen_settings, generate_accounts),
%                     lager:info("Lookup test: ~p ~n", [LookupTest]);

%                     %% TODO: Add further account creation options here, for now use default acc generating
%                 false -> 
%                     ok
%             end
%     end.

maybe_generate_accounts(Workspace) ->
    %% A CLI tool will provide a DB path representing either a work space, or some existing database (maybe for the sake of using some synced node data)
    %% So here we check whether that DB path already has any data present. if not, it's a new workspace and we generate accounts. the node later looks 
    %% for that file when in devmode and, if present uses it instead of its hardcoded accounts json.
    case os:getenv("AE__CHAIN__DB_PATH") of
        false -> ok;
        Path -> 
            case is_empty_dir(Path) of 
                true ->
                    lager:info("---------->>> Found empty workspace, generating Accounts !"),
                    %% TODO: Add further account creation options here, for now use default acc generating

                    AccountsList = try aeplugin_dev_mode_acc_gen:generate_accounts() of
                                Accs when is_map(Accs) -> Accs
                            catch
                                error:_ -> 
                                    erlang:error(failed_generating_devmode_accs)
                            end,
                    
                    #{nodeFormat := AccountsInNodeFormat} = AccountsList,
                    AccountsJSON = 
                            try jsx:encode(AccountsInNodeFormat) of
                                JSON -> JSON
                            catch
                                error:_ ->
                                    erlang:error(failed_jsonencoding_generated_devmode_accounts)
                            end,
                    JSONfilePath = filename:join(Path, "devmode_accs_" ++ Workspace ++ ".json"),
                    {ok, File} = file:open(JSONfilePath, [write]),
                    lager:info("---------->>> Writing accounts file to: ~p ~n", [JSONfilePath]),
                    file:write(File, AccountsJSON),
                    AccountsList;
                false -> ok
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
