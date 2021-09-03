-module(aeplugin_dev_mode_handler).

-export([ init/2
        , content_types_provided/2
        , index_html/2
        %% , emit_keyblocks/2
        %% , emit_microblock/2
        ]).

-import(aeplugin_dev_mode_html, [html/1, meta/0]).

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
                       {title, <<"AE Dev Mode">>}
                      ]},
               {body,
                [{p, [<<"Top height: ">>, integer_to_binary(aec_chain:top_height())]},
                 {p, <<"Actions">>},
                 {form, #{action => <<"/emit_kb">>, method => get},
                  [{label, #{for => n}, <<"N:">>},
                   {input, #{type => text, id => n, name => n}, []},
                   {input, #{type => submit, value => <<"Emit keyblocks">>}, []}
                  ]}
                ]}
              ]}),
    {HTML, Req, State}.


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
serve_request(_) ->
    ok.
