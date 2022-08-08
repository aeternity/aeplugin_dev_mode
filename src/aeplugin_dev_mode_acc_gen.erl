-module(aeplugin_dev_mode_acc_gen).
-export([generate_from_mnemonic/3, generate_accounts/2, generate_accounts/0]).

generate_from_mnemonic(Mnemonic, Quantity, Balance) ->
    Seed = ebip39:mnemonic_to_seed(Mnemonic, <<"">>),
    Derived = derive_from_seed(Seed, Quantity),
    to_all_formats(Derived, Balance).

generate_accounts() ->
    generate_accounts(2, 10000000000000000000000000000000).
    
    generate_accounts(Quantity, Balance) ->
    Mnemonic = ebip39:generate_mnemonic(128),
    % io:fwrite("------> Mnemonic: ~p ~n", [Mnemonic]),
    generate_from_mnemonic(Mnemonic, Quantity, Balance).

derive_from_seed(Seed, Quantity) ->
    [ eaex10:derive_aex10_from_seed(Seed, 0, Index) || Index <- lists:seq(1, Quantity)].

%% the node, the devmode internals and the devmode's public data need different formats of the account data. combining at least the latter 2 might be worked on next.
to_all_formats(ListOfDerivedKeys, Balance) ->
    ReadableFormat = lists:map(fun(D) -> 
        #{pub_key := Public, priv_key := Private} = eaex10:private_to_public(D),    
        #{pub_key => binary_to_atom(aeser_api_encoder:encode(account_pubkey, Public)),
             priv_key => binary_to_atom(hexlify(<<Private/binary, Public/binary>>)), 
             balance => Balance } 
            end,
        ListOfDerivedKeys),

    DevmodeFormat = lists:map(fun(D) -> 
        #{pub_key := Public, priv_key := Private} = eaex10:private_to_public(D),    
        {Public, <<Private/binary, Public/binary>>} 
            end,
        ListOfDerivedKeys),

    TupleList = lists:map(fun(D) -> 
        #{pub_key := Public} = eaex10:private_to_public(D),    
        {aeser_api_encoder:encode(account_pubkey, Public), list_to_binary(integer_to_list(Balance))} 
            end,
        ListOfDerivedKeys),
    NodeFormat = maps:from_list(TupleList),
    
    io:fwrite("Node format is: ~p ~n", [NodeFormat]),

    #{readableFormat => ReadableFormat, 
        devmodeFormat => DevmodeFormat, 
        nodeFormat => NodeFormat}.

hexlify(Bin) when is_binary(Bin) ->
    << <<(hex(H)),(hex(L))>> || <<H:4,L:4>> <= Bin >>.

hex(C) when C < 10 -> $0 + C;
hex(C) -> $a + C - 10.
