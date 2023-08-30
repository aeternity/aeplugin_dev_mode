-module(aeplugin_dev_mode_acc_gen).
-export([ generate_from_mnemonic/3
        , generate_accounts/2
        , generate_accounts/0]).

-include("aeplugin_dev_mode.hrl").

%% Generates Accounts from sources like mnemonic and seed, utilising ebip39 and eaex10
%%
generate_from_mnemonic(Mnemonic, Quantity, Balance) ->
    lager:info("Mnemonic = ~p", [Mnemonic]),
    Seed = ebip39:mnemonic_to_seed(Mnemonic, <<"">>),
    Derived = derive_from_seed(Seed, Quantity),
    format_accounts(Derived, Balance).

generate_accounts() ->
    generate_from_mnemonic(mnemonic(), quantity(), balance()).

generate_accounts(Quantity, Balance) ->
    generate_from_mnemonic(mnemonic(), Quantity, Balance).

derive_from_seed(Seed, Quantity) ->
    [ eaex10:derive_aex10_from_seed(Seed, 0, Index) || Index <- lists:seq(1, Quantity) ].

format_accounts(DerivedKeys, Balance) ->
    [ format_account_(K, Balance) || K <- DerivedKeys ].

format_account_(K, Balance) ->
    AEX10 = eaex10:private_to_public(K),
    (maps:with([priv_key, pub_key], AEX10))#{initial_balance => Balance}.

quantity() -> config(<<"quantity">>, 10).
balance()  -> config(<<"balance">> , 1000000000000000000000).
mnemonic() -> config(<<"mnemonic">>, ebip39:generate_mnemonic(128)).

config(Key, Default) ->
    ok(aeu_plugins:find_config(?PLUGIN_NAME_STR,
			       [<<"prefunded">>, <<"gen">>, Key],
			       [user_config, {value, Default}])).

ok({ok, Value}) -> Value.
