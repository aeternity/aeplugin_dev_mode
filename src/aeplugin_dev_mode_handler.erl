-module(aeplugin_dev_mode_handler).

-export([routes/0]).

-export([ init/2
        , content_types_provided/2
        , index_html/2
        %% , emit_keyblocks/2
        %% , emit_microblock/2
        ]).

-import(aeplugin_dev_mode_html, [html/1, meta/0]).

routes() ->
    [
     {'_', [ {"/", ?MODULE, []}
           , {"/emit_kb/", ?MODULE, []}
           , {"/emit_mb/", ?MODULE, []}
           , {"/kb_interval/", ?MODULE, []}
           , {"/mb_interval/", ?MODULE, []}
           , {"/auto_emit_mb/", ?MODULE, []}
           , {"/spend", ?MODULE, []}
           ]}
    ].

init(Req, Opts) ->
    lager:debug("Req = ~p, Opts = ~p", [Req, Opts]),
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    serve_request(Req),
    {[
       {<<"text/html">>, index_html}
     ], Req, State}.

index_html(Req, State) ->
    HTML = html(
             {html,
              [{head, [meta(),
                       {title, <<"AE Dev Mode">>},
                       {style, <<("table, th, td {"
                                  "border: 1px solid black;"
                                  "border-collapse: collapse;")>>}
                      ]},
               {body,
                [{a, #{href => <<"/">>}, <<"home">>},
                 {h2, <<"Actions">>},
                 {a, #{href => <<"/emit_mb">>, method => get}, <<"Emit microblock">>},
                 {p, []},
                 emit_kb_form(),
                 set_kb_interval_form(),
                 set_mb_interval_form(),
                 auto_emit_mb_form(),
                 spend_form(),
                 {hr, []},
                 {h2, <<"Chain:">>},
                 {p, [<<"Top height: ">>, integer_to_binary(aec_chain:top_height())]},
                 {p, [<<"Mempool size: ">>, integer_to_binary(aec_tx_pool:size())]},
                 {h3, <<"Account balances">>},
                 accounts_table()
                ]}
              ]}),
    {HTML, Req, State}.

accounts_table() ->
    Balances = account_balances(),
    {table,
     [{tr, [{th, <<"Key">>},
            {th, <<"Balance">>}]}
     | lists:map(
         fun({K, V}) ->
                 Strong = is_demo_pubkey(K),
                 EncKey = aeser_api_encoder:encode(account_pubkey, K),
                 {tr, [{td, maybe_strong(Strong, EncKey)},
                       {td, maybe_strong(Strong, integer_to_binary(V))}]}
         end, Balances) ]}.

account_balances() ->
    {ok, Trees} = aec_chain:get_block_state(aec_chain:top_block_hash()),
    aec_accounts_trees:get_all_accounts_balances(aec_trees:accounts(Trees)).

emit_kb_form() ->
    {form, #{action => <<"/emit_kb">>, method => get},
     [{label, #{for => n}, <<"N: ">>},
      {input, #{type => text, id => n, name => n}, []},
      {input, #{type => submit, value => <<"Emit keyblocks">>}, []}
     ]}.

set_kb_interval_form() ->
    Prev = aeplugin_dev_mode_emitter:get_keyblock_interval(),
    {form, #{action => <<"/kb_interval">>, method => get},
     [{label, #{for => secs}, <<"Secs: ">>},
      {input, #{type => text, id => secs, name => secs, value => integer_to_binary(Prev)}, []},
      {input, #{type => submit, value => <<"Keyblock interval (0 turns off)">>}, []}
     ]}.

set_mb_interval_form() ->
    Prev = aeplugin_dev_mode_emitter:get_microblock_interval(),
    {form, #{action => <<"/mb_interval">>, method => get},
     [{label, #{for => secs}, <<"Secs: ">>},
      {input, #{type => text, id => secs, name => secs, value => integer_to_binary(Prev)}, []},
      {input, #{type => submit, value => <<"Microblock interval (0 turns off)">>}, []}
     ]}.

auto_emit_mb_form() ->
    Bool = aeplugin_dev_mode_emitter:get_auto_emit_microblocks(),
    CBox0 = #{type => checkbox, id => auto_emit, name => auto_emit},
    CBox = if Bool -> CBox0#{checked => true};
              true -> CBox0
           end,
    {form, #{action => <<"/auto_emit_mb">>, method => get},
     [{label, #{for => auto_emit}, <<"Auto-emit microblocks">>},
      {input, #{type => hidden, value => atom_to_binary(Bool, utf8), id => previous, name => previous}, []},
      {input, CBox, []},
      {input, #{type => submit, value => <<"Set option(s)">>}, []}
     ]}.

spend_form() ->
    EncPubs = [aeser_api_encoder:encode(account_pubkey, K) || {K,_} <- demo_keypairs()],
    Options = [{option, #{value => Enc}, Enc} || Enc <- EncPubs],
    {form, #{action => <<"/spend">>, method => get},
     [{label, #{for => from}, <<"From: ">>},
      {select, #{name => from, id => from}, Options},
      {label, #{for => to}, <<"To: ">>},
      {select, #{name => to, id => to}, Options},
      {label, #{for => amount}, <<"Amount: ">>},
      {input, #{type => text, id => amount, name => amount}, []},
      {input, #{type => submit, value => <<"Spend">>}, []}
     ]}.

maybe_strong(true , Text) -> {strong, Text};
maybe_strong(false, Text) -> Text.

serve_request(#{path := <<"/emit_kb">>, qs := Qs}) ->
    Params = httpd:parse_query(Qs),
    N = case proplists:get_value(<<"n">>, Params, undefined) of
            undefined ->
                %% ignore
                0;
            NStr ->
                try binary_to_integer(NStr)
                catch
                    error:_ -> 0
                end
        end,
    case N of
        0 -> ok;
        _ ->
            aeplugin_dev_mode_emitter:emit_keyblocks(N)
    end;
serve_request(#{path := <<"/kb_interval">>, qs := Qs}) ->
    parse_qs(Qs, [{<<"secs">>, integer, undefined}],
             fun(undefined) ->
                     weird;
                (Secs) ->
                     aeplugin_dev_mode_emitter:set_keyblock_interval(Secs),
                     lager:info("New keyblock interval: ~p secs" ++ maybe_off(Secs), [Secs])
             end);
serve_request(#{path := <<"/mb_interval">>, qs := Qs}) ->
    parse_qs(Qs, [{<<"secs">>, integer, undefined}],
             fun(undefined) ->
                     weird;
                (Secs) ->
                     aeplugin_dev_mode_emitter:set_microblock_interval(Secs),
                     lager:info("New microblock interval: ~p secs" ++ maybe_off(Secs), [Secs])
             end);
serve_request(#{path := <<"/emit_mb">>}) ->
    aeplugin_dev_mode_emitter:emit_microblock();
serve_request(#{path := <<"/auto_emit_mb">>, qs := Qs}) ->
    Params = httpd:parse_query(Qs),
    case {proplists:get_value(<<"auto_emit">>, Params, undefined),
          proplists:get_value(<<"previous">>, Params, undefined)} of
        {undefined, <<"true">>}  -> set_auto_emit(false); % quirk of html checkboxes
        {<<"off">>, _} -> set_auto_emit(false);
        {<<"on">> , _} -> set_auto_emit(true);
        _ -> ok
    end;
serve_request(#{path := <<"/spend">>, qs := Qs}) ->
    Params = httpd:parse_query(Qs),
    [From, To, AmountB] = [proplists:get_value(K, Params)
                           || K <- [<<"from">>, <<"to">>, <<"amount">>]],
    {ok, FromInt} = aeser_api_encoder:safe_decode(account_pubkey, From),
    {ok, ToInt} = aeser_api_encoder:safe_decode(account_pubkey, To),
    Amount = binary_to_integer(AmountB),
    Balances = account_balances(),
    {ok, Nonce} = aec_next_nonce:pick_for_account(FromInt),
    case lists:keyfind(FromInt, 1, Balances) of
        {_, Bal} when Bal > Amount ->
            {ok, Tx} = aec_spend_tx:new(#{sender_id => acct(FromInt),
                                          recipient_id => acct(ToInt),
                                          amount => Amount,
                                          nonce => Nonce,
                                          fee => 20000 * min_gas_price(),
                                          ttl => 0,
                                          payload => <<"devmode demo">>}),
            {_,Priv} = lists:keyfind(FromInt, 1, demo_keypairs()),
            STx = sign_tx(Tx, Priv),
            Res = aec_tx_pool:push(STx),
            lager:info("Push Result = ~p", [Res]),
            ok;
        false ->
            lager:info("'From' account not a known demo account", []),
            ok
    end;
serve_request(_) ->
    ok.

maybe_off(0) ->
    " (off)";
maybe_off(_) ->
    "".

set_auto_emit(Bool) when is_boolean(Bool) ->
    aeplugin_dev_mode_emitter:auto_emit_microblocks(Bool),
    lager:info("Auto-emit microblocks set to ~s", [if Bool -> "on"; true -> "off" end]).

parse_qs(Qs, Types, F) ->
    Params = httpd:parse_query(Qs),
    try lists:map(
          fun(PSpec) ->
                  case parse_param(PSpec, Params) of
                      {ok, Val} ->
                          Val;
                      Error ->
                          lager:info("Bad parameter: ~p -> ~p", [PSpec, Error]),
                          throw(parse_error)
                  end
          end, Types) of
        Vals ->
            apply(F, Vals)
    catch
        throw:parse_error ->
            ignore
    end.

parse_param({Name, Type, Default}, Params) ->
    case lists:keyfind(Name, 1, Params) of
        {_, Val0} ->
            check_type(Val0, Type, Name);
        false ->
            {ok, Default}
    end.

check_type(V, integer, Name) ->
    try {ok, binary_to_integer(V)}
    catch
        error:_ ->
            {error, {not_an_integer, V, Name}}
    end.

sign_tx(Tx, PrivKey) ->
    Bin = aetx:serialize_to_binary(Tx),
    BinForNetwork = aec_governance:add_network_id(Bin),
    Sigs = [ enacl:sign_detached(BinForNetwork, PrivKey) ],
    aetx_sign:new(Tx, Sigs).

acct(Key) ->
    aeser_id:create(account, Key).

is_demo_pubkey(K) ->
    lists:keymember(K, 1, demo_keypairs()).

min_gas_price() ->
    aec_tx_pool:minimum_miner_gas_price().

%% Encoded pubkey of first keypair of the demo_keypairs():
%%     ak_GLab8McCgXqng1pZbQDmjbCLw6f48qGyP4zWqzqBVnYwdNWVc
%%
%% This is used as beneficiary in the examples/devmode.yaml

%% [{Pubkey, Privkey}]
demo_keypairs() ->
    [{<<34,211,105,168,28,63,144,218,27,148,69,230,108,203,60,
        118,189,48,67,20,68,151,186,192,77,185,248,60,73,145,
        254,193>>,
      <<251,183,173,174,69,15,7,18,184,88,101,70,33,94,137,156,
        241,33,30,29,169,80,68,174,19,172,112,177,60,30,238,
        119,34,211,105,168,28,63,144,218,27,148,69,230,108,203,
        60,118,189,48,67,20,68,151,186,192,77,185,248,60,73,
        145,254,193>>},
     {<<9,129,74,44,23,8,42,187,74,233,50,244,189,191,90,204,
        249,158,85,249,95,181,221,222,218,245,88,233,171,237,88,
        128>>,
      <<105,106,180,148,180,188,74,140,12,220,124,26,175,225,
        161,192,231,132,93,61,132,150,103,130,124,108,43,20,
        116,217,85,200,9,129,74,44,23,8,42,187,74,233,50,244,
        189,191,90,204,249,158,85,249,95,181,221,222,218,245,
        88,233,171,237,88,128>>},
     {<<186,190,110,128,129,49,7,206,128,220,119,85,54,62,137,
        81,19,55,187,145,79,134,92,232,173,60,3,253,120,240,53,
        192>>,
      <<202,6,50,28,196,88,45,102,43,227,98,231,98,90,153,219,
        37,203,210,186,26,129,3,203,162,28,174,227,248,32,139,
        45,186,190,110,128,129,49,7,206,128,220,119,85,54,62,
        137,81,19,55,187,145,79,134,92,232,173,60,3,253,120,
        240,53,192>>}
    ].

