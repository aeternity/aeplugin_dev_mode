-module(aeplugin_dev_mode_prefunded).

-export([ check_accounts/1
        , get_prefunded/0
        , format_gen_acct/1 ]).

-include("aeplugin_dev_mode.hrl").

check_accounts(WorkspaceDir) ->
    MnesiaExists = mnesia_db_exists(),
    Accounts = case read_accounts(WorkspaceDir) of
                   {ok, Filename, As} ->
                       lager:info("Read prefunded accounts. Will try auto-creating", []),
                       suggest_prefunded(Filename, As),
                       As;
                   {error, _} when not MnesiaExists ->
                       lager:info("No accounts found, generating..... "),
                       generate_accounts(WorkspaceDir);
                   {error, _} ->
                       lager:info("Chain db exists, and no prefunded dev mode accounts", []),
                       []
               end,
    set_prefunded(Accounts),
    Accounts.

set_prefunded(Accs) ->
    persistent_term:put({?MODULE, prefunded_accounts}, Accs).

get_prefunded() ->
    persistent_term:get({?MODULE, prefunded_accounts}, []).

generate_accounts(WorkspaceDir) ->
    case generate_accounts_(WorkspaceDir) of
        {ok, Filename, Accs} ->
            suggest_prefunded(Filename, Accs),
            Accs;
        error ->
            lager:error("Account generation failed", []),
            []
    end.

mnesia_db_exists() ->
    filelib:is_file(filename:join(mnesia_monitor:get_env(dir), "schema.DAT")).

suggest_prefunded(Filename, Accs) ->
    NodePrefunded = prefunded_pub_name(Filename),
    ok = generate_node_prefunded(Accs, NodePrefunded),
    aeu_plugins:suggest_config(prefunded_accs_cfg_key(), NodePrefunded).

prefunded_pub_name(Filename) ->
    Dir = filename:dirname(Filename),
    Ext = filename:extension(Filename),
    Base = filename:basename(Filename, Ext),
    filename:join(Dir, iolist_to_binary([Base, "-PUB", Ext])).

generate_node_prefunded(Accs, Filename) ->
    Out = lists:foldl(fun generate_pub_acc/2, #{}, Accs),
    JSON = jsx:encode(Out),
    ok = file:write_file(Filename, JSON).

generate_pub_acc(#{pub_key := Pub, initial_balance := Amt}, Acc) ->
    Key = aeapi:format_account_pubkey(Pub),
    Acc#{Key => Amt}.

generate_accounts_(WSDir) ->
    try aeplugin_dev_mode_acc_gen:generate_accounts() of
        Accs ->
            lager:info("Gen => ~p", [Accs]),
            format_generated_accounts(Accs, WSDir)
    catch
        error:E1:ST1 ->
            lager:error("Failed generating devmode accounts: ~p/~p", [E1, ST1]),
            error
    end.

format_generated_accounts(Accs, WSDir) ->
    Filename = accounts_file(WSDir),
    case format_generated_accounts_(Accs) of
        {ok, JSON} ->
            file:write_file(Filename, JSON),
            {ok, Filename, Accs};
        error ->
            error
    end.

format_generated_accounts_(Accounts) ->
    Out = [format_gen_acct(A) || A <- Accounts],
    try jsx:encode(Out) of
        JSON ->
            {ok, JSON}
    catch
        error:E:ST ->
            lager:error("Failed JSON-encoding accounts: ~p/~p", [E, ST]),
            error
    end.

format_gen_acct(#{pub_key := Pub, priv_key := Priv} = A) ->
    A#{pub_key := aeapi:format_account_pubkey(Pub),
       priv_key := encode_priv_key(Priv)}.

read_accounts(WSDir) ->
    Filename = accounts_file(WSDir),
    read_prefunded_accounts_file(Filename).

read_prefunded_accounts_file(F) ->
    case file:read_file(F) of
        {ok, Bin} ->
            try jsx:decode(Bin, [return_maps]) of
                Encoded ->
                    %% While jsx is capable of parsing keys to existing atoms,
                    %% We don't need to be so strict about not allowing unknown
                    %% keys in the file. So we explicitly convert what we need.
                    {ok, F, [#{ pub_key => ok(aeapi:decode_account_pubkey(Pub))
                              , priv_key => decode_priv_key(Priv)
                              , initial_balance => Bal} ||
                                #{ <<"pub_key">> := Pub
                                 , <<"priv_key">> := Priv
                                 , <<"initial_balance">> := Bal} <- Encoded]}
            catch
                error:E ->
                    lager:error("Error parsing JSON (~s): ~p", [F, E]),
                    {error, json_parse_error}
            end;
        {error, E} = Error ->
            lager:error("Cannot read prefunded accounts file ~s: ~p", [F, E]),
            Error
    end.

ok({ok, Value}) -> Value.

prefunded_accs_cfg_key() ->
    [<<"system">>, <<"custom_prefunded_accs_file">>].

accounts_file(WSDir) ->
    case aeu_plugins:find_config(?PLUGIN_NAME_STR,
                                 [<<"prefunded">>, <<"file">>],
                                 [user_config]) of
        {ok, Filename} ->
            case filename:dirname(Filename) of
                <<".">> ->
                    filename:join(WSDir, Filename);
                _ ->
                    Filename
            end;
        undefined ->
            filename:join(WSDir, "devmode_prefunded_accounts.json")
    end.

%% See PR #19 to https://github.com/aeternity/aeserialization
%%
-define(BASE64, 2).
encode_priv_key(Priv) ->
    encode(account_secret_key, Priv).

encode(Type, Payload) ->
    Pfx = type2pfx(Type),
    Enc = case type2enc(Type) of
              ?BASE64 -> base64_check(Payload)
          end,
    <<Pfx/binary, "_", Enc/binary>>.

decode_priv_key(Bin0) ->
    Res = case split(Bin0) of
              [Pfx, Payload] ->
                  Type = pfx2type(Pfx),
                  Bin = decode_check(Type, Payload),
                  case type_size_check(Type, Bin) of
                      ok -> {Type, Bin};
                      {error, Reason} -> erlang:error(Reason)
                  end;
              _ ->
                  erlang:error(missing_prefix)
          end,
    {account_secret_key, Priv} = Res,
    Priv.

byte_size_for_type(account_secret_key) -> 32.

type2enc(account_secret_key) -> ?BASE64.

type2pfx(account_secret_key) -> <<"sk">>.

pfx2type(<<"sk">>) -> account_secret_key.

type_size_check(Type, Bin) ->
    case byte_size_for_type(Type) of
        not_applicable -> ok;
        CorrectSize ->
            Size = byte_size(Bin),
            case Size =:= CorrectSize of
                true -> ok;
                false -> {error, incorrect_size}
            end
    end.

base64_check(Bin) ->
    C = check_str(Bin),
    binary_to_base64(iolist_to_binary([Bin, C])).

split(Bin) ->
    binary:split(Bin, [<<"_">>], []).

decode_check(Type, Bin) ->
    Dec =
        case type2enc(Type) of
%%            ?BASE58 -> base58_to_binary(Bin);
            ?BASE64 -> base64_to_binary(Bin)
        end,
    Sz = byte_size(Dec),
    BSz = Sz - 4,
    <<Body:BSz/binary, C:4/binary>> = Dec,
    C = check_str(Body),
    Body.

check_str(Bin) ->
    <<C:32/bitstring,_/binary>> =
        sha256_hash(sha256_hash(Bin)),
    C.

sha256_hash(Bin) ->
    crypto:hash(sha256, Bin).

binary_to_base64(Bin) ->
    base64:encode(Bin).

base64_to_binary(Bin) when is_binary(Bin) ->
    base64:decode(Bin).
