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


try_reading_prefunded_accounts() ->
    Workspace = lists:last(filename:split(os:getenv("AE__CHAIN__DB_PATH"))),
    lager:info("Devmode Project workspace set to: ~p ~n", [os:getenv("AE__CHAIN__DB_PATH")]),
    FilePath = filename:join(os:getenv("AE__CHAIN__DB_PATH"), "devmode_prefunded_accs_" ++ Workspace ++ ".json"),
    KeyFilePath = filename:join(os:getenv("AE__CHAIN__DB_PATH"), "devmode_acc_keys_" ++ Workspace ++ ".json"),
    lager:info("Trying to load data for prefunded accounts from: ~p ~n", [FilePath]),

    NodeFormat = case file:read_file(FilePath) of
                    {ok, Accs} -> 
                            lager:info("Account data successfully loaded."),
                            maps:from_list(jsx:decode(Accs));
                    {error, _} -> not_found
                 end,
    
    {Readable, Devmode} = case file:read_file(KeyFilePath) of
                                    {ok, KeyAccs} -> 
                                            Decoded = maps:from_list(jsx:decode(KeyAccs)),
                                            #{ <<"readableFormat">> := DecodedReadableFormat} = Decoded,
                                            #{ <<"devmodeFormat">> := DecodedDevmodeFormat} = Decoded,
                                            {DecodedReadableFormat, DecodedDevmodeFormat};
                                    {error, _} -> not_found
                                end,
  
    #{nodeFormat => NodeFormat,
       readableFormat => Readable,
       devmodeFormat => Devmode}.
        
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
    Workspace = determine_workspace(),
    Accs = maybe_generate_accounts(Workspace),

    case  aeu_plugins:is_dev_mode() and (Accs =/= not_found) of
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
            aeu_plugins:suggest_config([<<"mining">>, <<"beneficiary">>], Pub),
            aeu_plugins:suggest_config([<<"mining">>, <<"beneficiary_reward_delay">>], 2);
        false ->
            case aeu_plugins:is_dev_mode() of
                 true ->
                    % if it's devmode and there are no accounts found, it can only be a synced node or
                    % one that has a DB with the previous hardcoded test keypair. Figuring out this last case is not 
                    % worth the time, let's assume !accounts => synced node, and set the prefilled accounts as empty.
                    lager:info("No devmode accounts found. Assumingly, this is chaindata of a previously synced chain."),
                    #{pubkey := Pub} = aecore_env:patron_keypair_for_testing(),
                    EncPubkey = aeser_api_encoder:encode(account_pubkey, Pub),
                    aeu_plugins:suggest_config([<<"mining">>, <<"beneficiary">>], EncPubkey),
                    aeu_plugins:suggest_config([<<"mining">>, <<"beneficiary_reward_delay">>], 2);
                 false ->
                    ok
            end
    end,
    ok.    

maybe_generate_accounts(Workspace) ->
    %% A CLI tool will provide a DB path representing either a work space, or some existing database (maybe for the sake of using some synced node data)
    %% So here we check whether that DB path already has any data present. if not, it's a new workspace and we generate accounts. the node later looks 
    %% for that file if in devmode and, if present, uses it instead of its hardcoded accounts json.
    case os:getenv("AE__CHAIN__DB_PATH") of
        false -> ok;
        Path -> 
            case is_empty_dir(Path) of 
                % no accounts present, generate new json file.
                true ->
                    lager:info("New workspace found, generating prefunded accounts."),
                    %% TODO: Add further account creation options here, for now use default acc generating

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

                    % save the list of account addresses for the node to prefund
                    JSONfilePath = filename:join(Path, "devmode_prefunded_accs_" ++ Workspace ++ ".json"),
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
                    KeysJSONfilePath = filename:join(Path, "devmode_acc_keys_" ++ Workspace ++ ".json"),
                    {ok, AccountsFile} = file:open(KeysJSONfilePath, [write]),
                    lager:info("Saving account key information to: ~p ~n", [JSONfilePath]),
                    file:write(AccountsFile, KeysJSON),
                     AccountsList;   
                    
                false -> 
                    % dir not empty, accounts could be present, check.
                    case try_reading_prefunded_accounts() of
                        % somebody loaded up a DB from a mainnet sync or something.
                        not_found -> not_found;
                        % Existing accounts json found, return.
                        FoundAccounts when is_map(FoundAccounts) -> FoundAccounts
                    end
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
