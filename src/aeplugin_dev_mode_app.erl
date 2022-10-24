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
    {ok, Pid} = aeplugin_dev_mode_sup:start_link(),
    ok = start_http_api(),
    {ok, Pid}.


try_reading_prefunded_accounts(WorkspacePath) ->
    
    {WorkspaceName, AccFilePath, KeyFilePath} = workspacename_accfilepath_keyfilepath(WorkspacePath),
    lager:info("Trying to load data for prefunded accounts from: ~p ~n", [KeyFilePath]),
    NodeFormat = case file:read_file(AccFilePath) of
                    {ok, Accs} -> 
                            lager:info("Account data successfully loaded."),
                            maps:from_list(jsx:decode(Accs)),
                            % for the node to be aware of where to read the custom prefunded accounts file from:
                            os:putenv("AE__SYSTEM__CUSTOM_PREFUNDED_ACCS_FILE", filename:join(WorkspacePath, "devmode_prefunded_accs_" ++ WorkspaceName ++ ".json"));
                    _ -> {not_found, not_found}
                 end,
    
    {Readable, Devmode} = case file:read_file(KeyFilePath) of
                                    {ok, KeyAccs} -> 
                                            Decoded = maps:from_list(jsx:decode(KeyAccs)),
                                            #{ <<"readableFormat">> := DecodedReadableFormat} = Decoded,
                                            #{ <<"devmodeFormat">> := DecodedDevmodeFormat} = Decoded,
                                            {DecodedReadableFormat, DecodedDevmodeFormat};
                                    _ -> {not_found, not_found} %% struktur passt nicht zur erwarteten return value 1! etwas anderes überlegen
                                end,

    % TODO: check if devmodeFormat is still necessary and clean out
    case (NodeFormat =:= {not_found, not_found}) or (Readable =:= {not_found, not_found}) of
        true -> not_found;
        false -> #{nodeFormat => NodeFormat,
                  readableFormat => Readable,
                  devmodeFormat => Devmode}
    end.
    
        
start_phase(check_config, _Type, _Args) ->
    case aeu_env:user_config([<<"system">>, <<"dev_mode_accounts">>]) of
        undefined -> 
            %% In this case, we should be dealing with a pre-synced database and no prefunded accounts.
            % Todo: This needs explicit testing
            aeplugin_dev_mode_emitter:set_prefilled_accounts_info(#{
                nodeFormat => [],
                readableFormat => [],
                devmodeFormat => []});
        {ok, Accs} -> 
                % Due to some weird quirk, the setting is not returned as a list of maps as it was stored,
                % but tagged tuples and lists etc. Here it is brought in shape again.
        BasicMap = maps:from_list(Accs),
        #{readableFormat := Readable} = BasicMap,
        #{nodeFormat := Node} = BasicMap,
        #{devmodeFormat := Devmode} = BasicMap,
        % check if removal of numeration is necessary
            FixedReadable = case Readable of 
                                    [{1,_}|_] -> [maps:from_list(OneReadable) || {_, OneReadable} <- Readable];
                                    [{<<"1">>,_}|_] -> [maps:from_list(OneReadable) || {_, OneReadable} <- Readable];
                                    _ -> Readable
                                end,
            FixedNode = maps:from_list(Node),
            aeplugin_dev_mode_emitter:set_prefilled_accounts_info(#{
                                                                    nodeFormat => FixedNode,
                                                                    readableFormat => FixedReadable,
                                                                    devmodeFormat => Devmode})
    end,

    case aeu_plugins:check_config(?PLUGIN_NAME_STR, ?SCHEMA_FNAME, ?OS_ENV_PFX) of
        Config when is_map(Config) ->
            apply_config(Config);
        not_found ->
            ok
    end.

stop(_State) ->
    stop_http_api(),
    ok.

determine_workspace_dir() ->
    % Check if a workspace is defined. if not, make the custom DB path OR the default db path the workspace.
    case os:getenv("AE__CHAIN__DB_PATH") of 
        false -> 
            % if no custom env vars provided, take parent dir of mnesia's 'data' directory as default
            DefaultDir = filename:join(lists:droplast(filename:split(mnesia:system_info(directory)))),
            lager:info("No workspace provided, taking as default dir: ~p ~n", [DefaultDir]),
            DefaultDir;
        DefinedWorkspace ->
            case filelib:ensure_dir(DefinedWorkspace) of
                ok -> DefinedWorkspace;
                _ -> erlang:error(invalid_workspace_path_provided)
            end
    end.


check_env() ->
    WorkspaceDir = determine_workspace_dir(),
    lager:info("Active Workspace Directgory: ~p ~n", [WorkspaceDir]),
    Accs = read_or_maybe_generate_accounts(WorkspaceDir),

    case Accs =/= not_found of
        true ->
            #{readableFormat := ReadableFormat} = Accs,
            Pub = case ReadableFormat of
                [[{<<"1">>, Tuples} | _] | _ ] ->
                    PropMap = maps:from_list(Tuples),
                    #{<<"pub_key">> := PubKey} = PropMap,
                    PubKey;
                [ #{1 := #{ pub_key := PubKey}} | _] ->
                    atom_to_binary(PubKey)
            end, 

            lager:info("Setting the first devmode account as mining beneficiary: ~p ~n", [Pub]),
            aeu_plugins:suggest_config([<<"mining">>, <<"beneficiary">>], Pub);
        false ->
            % if it's devmode and there are no accounts found, it can only be a synced node or
            % one that has a DB with the previous hardcoded test keypair.
            lager:info("No generated devmode accounts found. Assumingly, this is chaindata of a previously synced chain."),
            #{pubkey := Pub} = aecore_env:patron_keypair_for_testing(),
            EncPubkey = aeser_api_encoder:encode(account_pubkey, Pub),
            aeu_plugins:suggest_config([<<"mining">>, <<"beneficiary">>], EncPubkey)
        end,
        aeu_plugins:suggest_config([<<"mining">>, <<"beneficiary_reward_delay">>], 2),
    ok.    

read_or_maybe_generate_accounts(WorkspacePath) ->
    %% A CLI tool will provide a path to a new DB folder or an existing DB (maybe for the sake of using some synced node data)
    %% So here we check whether that DB path already has any accounts data present. if not and there is no DB present, it's a new workspace, we generate accounts and 
    % set the env var for the node whete to look for the accounts. the node later looks 
    %% for that file if the env var is set and, if present, uses it instead of its hardcoded accounts json.

            case try_reading_prefunded_accounts(WorkspacePath) of 
                FoundAccounts when is_map(FoundAccounts) ->
                    FoundAccounts;

                not_found -> 
                    lager:info("No accounts found, checking for presence of mnesia folder....."),
                % check if there is already a mnesia folder present. 
                % if so, it's most likely an already existing chain sync and therefor don't generate any accounts.
                % if not in a synced dir and no accounts files present, generate new json file.
                    case filelib:is_dir(filename:join(WorkspacePath, "mnesia")) of
                        true -> 
                            lager:info("mnesia fodler found, not generating accounts."),
                            not_found;
                        false -> 
                            %% Generate accounts
                            %% TODO: Add further account creation options here, for now use default acc generating
                            lager:info("No accounts found, generating..... "),
                            AccountsList = try aeplugin_dev_mode_acc_gen:generate_accounts() of
                                        Accs when is_map(Accs) -> Accs
                                    catch
                                        error:_ -> 
                                            erlang:error(failed_generating_devmode_accs)
                                    end,
                            
                            #{nodeFormat := AccountsInNodeFormat} = AccountsList,
                            #{readableFormat := AccountsInReadableFormat} = AccountsList,
                            #{devmodeFormat := AccountsInDevmodeFormat} = AccountsList,
                            AccountsJSON = 
                                    try jsx:encode(AccountsInNodeFormat) of
                                        JSON -> JSON
                                    catch
                                        error:_ ->
                                            erlang:error(failed_jsonencoding_generated_devmode_accounts)
                                    end,

                            {WorkspaceName, _, _} = workspacename_accfilepath_keyfilepath(WorkspacePath),        
                            % put sys env var for the node to know to use the custom accs
                            os:putenv("AE__SYSTEM__CUSTOM_PREFUNDED_ACCS_FILE", filename:join(WorkspacePath, "devmode_prefunded_accs_" ++ WorkspaceName ++ ".json")),
                            % save the list of account addresses for the node to prefund
                            JSONfilePath = filename:join(WorkspacePath, "devmode_prefunded_accs_" ++ WorkspaceName ++ ".json"),
                            {ok, File} = file:open(JSONfilePath, [write]),
                            file:write(File, AccountsJSON),

                            KeysJSON = 
                                try jsx:encode(#{ readableFormat => AccountsInReadableFormat, devmodeFormat => AccountsInDevmodeFormat}) of
                                    KJSON -> KJSON
                                catch
                                    error:_ ->
                                        erlang:error(failed_jsonencoding_generated_devmode_keys)
                                end,
                            % save the keys of the accounts for devmode
                            KeysJSONfilePath = filename:join(WorkspacePath, "devmode_acc_keys_" ++ WorkspaceName ++ ".json"),
                            {ok, AccountsFile} = file:open(KeysJSONfilePath, [write]),
                            lager:info("Saving account key information to: ~p ~n", [JSONfilePath]),
                            file:write(AccountsFile, KeysJSON),
                            AccountsList
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

workspacename_accfilepath_keyfilepath(WorkspacePath) ->
    WorkspaceName = lists:last(filename:split(WorkspacePath)),
    lager:info("Current active Workspace name: ~p ~n", [WorkspaceName]),
    FilePath = filename:join(WorkspacePath, "devmode_prefunded_accs_" ++ WorkspaceName ++ ".json"),
    KeyFilePath = filename:join(WorkspacePath, "devmode_acc_keys_" ++ WorkspaceName ++ ".json"),
    
    {WorkspaceName, FilePath, KeyFilePath}.