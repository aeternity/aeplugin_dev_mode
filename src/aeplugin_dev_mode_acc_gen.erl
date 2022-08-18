-module(aeplugin_dev_mode_acc_gen).
-export([generate_from_mnemonic/3, generate_accounts/2, generate_accounts/0]).

%% Generates Accounts from sources like mnemonic and seed, utilising ebip39 and eaex10

generate_from_mnemonic(Mnemonic, Quantity, Balance) ->
    Seed = ebip39:mnemonic_to_seed(Mnemonic, <<"">>),
    Derived = derive_from_seed(Seed, Quantity),
    to_all_formats(Derived, Balance).

generate_accounts() ->
    generate_accounts(10, 10000000000000000000000000000000).
    
    generate_accounts(Quantity, Balance) ->
    Mnemonic = ebip39:generate_mnemonic(128),
    generate_from_mnemonic(Mnemonic, Quantity, Balance).

derive_from_seed(Seed, Quantity) ->
    [ eaex10:derive_aex10_from_seed(Seed, 0, Index) || Index <- lists:seq(1, Quantity)].

%% the node, the devmode internals and the devmode's public data need different formats of the account data. combining at least the latter 2 might be worked on next.
to_all_formats(ListOfDerivedKeys, Balance) ->
    UncountedReadableFormat = lists:map(fun(D) -> 
        #{pub_key := Public, priv_key := Private} = eaex10:private_to_public(D),    
        #{pub_key => binary_to_atom(aeser_api_encoder:encode(account_pubkey, Public)),
        priv_key => binary_to_atom(hexlify(<<Private/binary, Public/binary>>)), 
        initial_balance => Balance } 
        end,
        ListOfDerivedKeys),

    %% this numeration is added, because the node client's configuration storage alters the 
    %% data structure of the stored data somewhere under the hood. The numeration is used to 
    %% create a proper map again after reading the data from the configuration.
    ReadableFormat = enumerate(UncountedReadableFormat, [], 1),

    %% Due to changes in the devmode, this format is currently not used, but might be useful for other applications.
    DevmodeFormat = lists:map(fun(D) -> 
        #{pub_key := Public, priv_key := Private} = eaex10:private_to_public(D),    
        {Public, <<Private/binary, Public/binary>>} 
            end,
        ListOfDerivedKeys),

    TupleList = lists:map(fun(D) -> 
        #{pub_key := Public} = eaex10:private_to_public(D),    
        {aeser_api_encoder:encode(account_pubkey, Public), Balance} 
            end,
        ListOfDerivedKeys),
    NodeFormat = maps:from_list(TupleList),

    #{readableFormat => ReadableFormat, 
        devmodeFormat => DevmodeFormat, 
        nodeFormat => NodeFormat}.

hexlify(Bin) when is_binary(Bin) ->
    << <<(hex(H)),(hex(L))>> || <<H:4,L:4>> <= Bin >>.

hex(C) when C < 10 -> $0 + C;
hex(C) -> $a + C - 10.

enumerate([], Acc, _) ->
    Acc;

enumerate([H|T], Acc, Count) ->
    NewAcc = Acc ++ [#{ Count => H}],
    enumerate(T, NewAcc, Count + 1).