-module(aeplugin_dev_mode_html).
-export([html/1,
         meta/0]).

html({Tag, C}) -> tagged(Tag, [], html(C));
html({Tag, Opt, C}) -> tagged(Tag, Opt, html(C));
%% html(#{html  := C} = X) -> tagged(html,  X, html(C));
%% html(#{head  := C} = X) -> tagged(head,  X, html(C));
%% html(#{title := C} = X) -> tagged(title, X, html(C));
%% html(#{body  := C} = X) -> tagged(body,  X, html(C));
%% html(#{p     := C} = X) -> tagged(p,     X, html(C));
%% html(#{elem  := C, tag := Tag} = X) -> tagged(Tag, X, html(C));
%% html(#{form  := _} = X) -> form(X);
%% html(#{href  := URL, text := T}) -> href(T, URL);
html(L) when is_list(L) -> 
    [html(X) || X <- L];
html(B) when is_binary(B) -> 
    B.

meta() ->
    <<"<meta charset=\"utf8\">">>.

tagged(Tag, X, []) ->
    [<<"<">>, bin(Tag), opt(X), <<"/>">>];
tagged(Tag, X, Content) ->
    BTag = bin(Tag),
    [<<"<">>, BTag, opt(X), <<">">>,
     Content, <<"</">>, BTag, <<">">>].

opt(M) when is_map(M), map_size(M) > 0 ->
    [H|T] = maps:to_list(M),
    [" ", kv(H) | [[", ", kv(X)] || X <- T]];
opt(_) ->
    <<>>.

kv({K, V}) ->
    [bin(K), <<"=\"">>, bin(V), <<"\"">>].

bin(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
bin(I) when is_integer(I) ->
    integer_to_binary(I);
bin(B) when is_binary(B) ->
    B.
