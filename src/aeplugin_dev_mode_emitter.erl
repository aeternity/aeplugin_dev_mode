%% -*- erlang-indent-mode: 4; indent-tabs-mode: nil -*-
-module(aeplugin_dev_mode_emitter).
-behavior(gen_server).

-export([start_link/0]).

-export([
          emit_keyblocks/1
        , emit_microblock/0
        , mine_until_txs_on_chain/2
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

-record(st, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

emit_keyblocks(N) when is_integer(N), N > 0 ->
    gen_server:call(?MODULE, {emit_keyblocks, N}).

emit_microblock() ->
    gen_server:call(?MODULE, emit_microblock).

mine_until_txs_on_chain(TxHashes, Max) when is_list(TxHashes),
                                            is_integer(Max),
                                            Max > 0 ->
    gen_server:call(?MODULE, {mine_until_txs_on_chain, TxHashes, Max}).

init([]) ->
    aec_events:subscribe(tx_created),
    aec_events:subscribe(tx_received),
    aec_events:subscribe(top_changed),
    case aec_chain:top_height() of
        0 ->
            aec_conductor:consensus_request(emit_kb);
        _ ->
            ok
    end,
    {ok, #st{}}.

handle_call({emit_keyblocks, N}, _From, St) ->
    St1 = emit_keyblocks_(N, St),
    {reply, ok, St1};
handle_call(emit_microblock, _From, St) ->
    {reply, ok, emit_microblock_(St)};
handle_call({mine_until_txs_on_chain, TxHashes, Max}, _From, St) ->
    {Reply, St1} = mine_until_txs_on_chain_(TxHashes, Max, St),
    {reply, Reply, St1};
handle_call(_Req, _From, St) ->
    {reply, {error, unknown_call}, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({gproc_ps_event, Event, #{info := STx}}, St)
  when Event == tx_created; Event == tx_received ->
    Hashes = [aetx_sign:hash(STx) | flush_tx_events()],
    lager:info("Tx pool events hashes: ~p", [[encode_hash(H) || H <- Hashes]]),
    {noreply, mine_until_txs_on_chain_(Hashes, 10, St)};
handle_info({gproc_ps_event, top_changed, #{info := Info}}, St) ->
    case Info of
        #{block_type := key, height := Height} ->
            lager:info("New key block - height: ~p", [Height]);
        #{block_type := micro} ->
            info_micro_block(Info)
    end,
    {noreply, St};
handle_info(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

info_micro_block(#{block_type := micro, block_hash := BHash, height := Height}) ->
    case aec_chain:get_block(BHash) of
        {ok, Block} ->
            Txs = aec_blocks:txs(Block),
            lager:info("New micro block - height: ~p, ~p txs",
                       [Height, length(Txs)]);
        {error, Reason} ->
            %% Shouldn't happen
            lager:info("New micro block - height: ~p, CANNOT READ (~p)",
                       [Height, Reason])
    end.

flush_tx_events() ->
    receive
        {gproc_ps_event, Event, #{info := STx}} when Event == tx_created;
                                                     Event == tx_received ->
            [aetx_sign:hash(STx) | flush_tx_events()]
    after 0 ->
            []
    end.

encode_hash(Hash) ->
    aeser_api_encoder:encode(tx_hash, Hash).

emit_keyblocks_(N, St) ->
    aec_conductor:consensus_request({mine_blocks, N, key}),
    St.

emit_microblock_(St) ->
    aec_conductor:consensus_request(emit_mb),
    St.

mine_until_txs_on_chain_(TxHashes, Max, St) ->
    Reply = aec_conductor:consensus_request(
              {mine_blocks_until_txs_on_chain, TxHashes, Max}),
    {Reply, St}.
