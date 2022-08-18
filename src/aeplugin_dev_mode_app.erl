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
    FilePath = filename:join(os:getenv("AE__CHAIN__DB_PATH"), "devmode_prefunded_accs_" ++ Workspace ++ ".json"),
    KeyFilePath = filename:join(os:getenv("AE__CHAIN__DB_PATH"), "devmode_acc_keys_" ++ Workspace ++ ".json"),

    io:fwrite("---------->>> Final calculated accounts file path: ~p ~n", [FilePath]),
    
    NodeFormat = case file:read_file(FilePath) of
                    {ok, Accs} -> 
                            io:fwrite("Node format read from drive, json decoded: ~p ~n", [jsx:decode(Accs)]),    
                            maps:from_list(jsx:decode(Accs));
                    {error, _} -> not_found
                 end,
    
    {Readable, Devmode} = case file:read_file(KeyFilePath) of
                                    {ok, KeyAccs} -> 
                                            Decoded = maps:from_list(jsx:decode(KeyAccs)),
                                            io:fwrite("---------->>>Decoded from harddrive?? ~p ~n", [Decoded]),
                                            
                                            % #{ <<"readableFormat">> := [DecodedReadableFormat | _]} = Decoded,
                                            #{ <<"readableFormat">> := DecodedReadableFormat} = Decoded,
                                            
                                            #{ <<"devmodeFormat">> := DecodedDevmodeFormat} = Decoded,
                                             io:fwrite("Decoded readable format: ~p ~n", [DecodedReadableFormat]),  
                                            ListOfMaps = [maps:from_list(OneAcc) || [{_ , OneAcc}] <- DecodedReadableFormat],
                                            io:fwrite("---------->>> Devmode keys as List of maps: ~p ~n", [ListOfMaps]),
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
                io:fwrite("---------->>> Read accounts from settings : ~p ~n", [Accs]),
                % Due to some weird quirk, the setting is not returned as a list of maps as it was stored,
                % but tagged tuples and lists etc. Here it is brought in shape again.
        BasicMap = maps:from_list(Accs),
        #{readableFormat := Readable} = BasicMap,
        #{nodeFormat := Node} = BasicMap,
        #{devmodeFormat := Devmode} = BasicMap,
        io:fwrite("---------->>> Only the readable from settings: ~p ~n", [Readable]),
        % check if removal of numeration is necessary
            FixedReadable = case Readable of 
                                    [{1,_}|_] -> io:fwrite("---------->>> Applying fix"), [maps:from_list(OneReadable) || {_, OneReadable} <- Readable];
                                    [{<<"1">>,_}|_] -> io:fwrite("---------->>> Applying fix"), [maps:from_list(OneReadable) || {_, OneReadable} <- Readable];
                                    _ -> Readable
                                end,
            % FixedReadable = [maps:from_list(OneReadable) || {_, OneReadable} <- Readable],
            FixedNode = maps:from_list(Node),
            io:fwrite("---------->>> Fixed Readable: : ~p ~n", [FixedReadable]),
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
    Accs = maybe_generate_accounts(Workspace),

    lager:info("---------->>> Got passed following accs: ~p ~n", [Accs])
    ,
    case  aeu_plugins:is_dev_mode() and (Accs =/= not_found) of
        true ->

            #{devmodeFormat := DevmodeFormatAccList} = Accs,
            {Pub, Priv} = lists:nth(1, DevmodeFormatAccList),
            lager:info("---------->>> Pub and priv are : ~p ~n", [Pub]),
            EncPubkey = aeser_api_encoder:encode(account_pubkey, Pub),
            lager:info("---------->>> Pubkey encoded like : ~p ~n", [EncPubkey]),

            %TODO: make dynamic !
            aeu_plugins:suggest_config([<<"mining">>, <<"beneficiary">>], <<"ak_2uKv5p1udex76mkpe6sPQfpmvSb6ShRxikWi2LPErUP16VfvsJ">>),
            aeu_plugins:suggest_config([<<"mining">>, <<"beneficiary_reward_delay">>], 2),
            % #{devmodeFormat:= OnlyDevmode} = Accs,
            % aeu_plugins:suggest_config([<<"system">>, <<"dev_mode_accounts">>], jsx:encode(OnlyDevmode)); % TODO: put whole list 
            
            aeu_plugins:suggest_config([<<"system">>, <<"dev_mode_accounts">>], Accs); 
        false ->
            case aeu_plugins:is_dev_mode() of
                 true ->
                    % if it's devmode and there are no accounts found, it can only be a synced node or
                    % one that has a DB with the previous hardcoded test keypair. Figuring out this last case is not 
                    % worth the time, let's assume !accounts => synced node, and set the prefilled accounts as empty.
                    #{pubkey := Pub} = aecore_env:patron_keypair_for_testing(),
                    EncPubkey = aeser_api_encoder:encode(account_pubkey, Pub),
                    aeu_plugins:suggest_config([<<"mining">>, <<"beneficiary">>], EncPubkey),
                    aeu_plugins:suggest_config([<<"mining">>, <<"beneficiary_reward_delay">>], 2),
                    aeu_plugins:suggest_config([<<"system">>, <<"dev_mode_accounts">>], #{readableFormat => [], 
                                                                                            devmodeFormat => [], 
                                                                                            nodeFormat => []}); 
                 false ->
                    ok
            end
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
    %% for that file if in devmode and, if present, uses it instead of its hardcoded accounts json.
    case os:getenv("AE__CHAIN__DB_PATH") of
        false -> ok;
        Path -> 
            case is_empty_dir(Path) of 
                % no accounts present, generate new json file.
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
                    lager:info("---------->>> Writing accounts file to: ~p ~n", [JSONfilePath]),
                    file:write(File, AccountsJSON),

                            % save the keys of the accounts for devmode
                    KeysJSON = 
                        try jsx:encode(#{ readableFormat => AccountsInReadableFormat, devmodeFormat => AccountsInDevmodeFormat}) of
                            KJSON -> KJSON
                        catch
                            error:_ ->
                                erlang:error(failed_jsonencoding_generated_devmode_keys)
                        end,

                    % save the account keys for devmode to pick up later
                    KeysJSONfilePath = filename:join(Path, "devmode_acc_keys_" ++ Workspace ++ ".json"),
                    {ok, AccountsFile} = file:open(KeysJSONfilePath, [write]),
                    lager:info("---------->>> Keys JSON ~p ~n", [KeysJSON]),
                    lager:info("---------->>> Keys raw: ~p ~n", [AccountsInDevmodeFormat]),
                    file:write(AccountsFile, KeysJSON),

                    lager:info("---------->>> Passed accounts list is: ~p ~n", [AccountsList]),
                     % fallback:
                     AccountsList;   
                      
                    %%TODO:  This is just for removing unnecessary enumeration in the readableFormat, eventually needed later.

                    % % lager:info("---------->>> AccountsInReadableFormat: ~p ~n", [AccountsInReadableFormat]),
                    % FlattenedReadableFormat = lists:map(fun(Account) -> 
                    %         [{ _, AccountMap}] = maps:to_list(Account),
                    %             AccountMap
                    %     end, AccountsInReadableFormat),
                    % % [OneAcc || [{_ , OneAcc}] <- AccountsInReadableFormat], 
                    % lager:info("---------->>> FlattenedReadableFormat: ~p ~n", [FlattenedReadableFormat]),

                    
                    % maps:update(readableFormat, FlattenedReadableFormat, AccountsList);
                false -> 
                    % accounts could be present, check.
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
