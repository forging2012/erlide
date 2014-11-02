-module(erlide_builder_util).

-export([
         source_clash/1,
         code_clash/0
    ]).

%%%%%%%%%%%%%%%%%%%%

source_clash(Dirs) ->
    Struct = lists:flatten(build(Dirs, "erl")),
    search(Struct).

%%% this part is adapted from the standard code module

code_clash()  ->
    Path = code:get_path(),
    Struct = lists:flatten(build(Path, code:objfile_extension())),
    search(Struct).

search([]) -> [];
search([{Dir,File} | Tail]) ->
    case lists:keysearch(File,2,Tail) of
        false ->
            search(Tail);
        {value,{Dir2,File}} ->
            [{filename:join(Dir,File),
              filename:join(Dir2,File)} | search(Tail)]
    end.

build([], _Ext) -> [];
build([Dir|Tail], Ext) ->
    Files = filter(Ext, Dir, file:list_dir(Dir)),
    [decorate(Files, Dir) | build(Tail, Ext)].

decorate([], _) -> [];
decorate([File|Tail], Dir) ->
    [{Dir, File} | decorate(Tail, Dir)].

filter(_Ext, _Dir, {error,_}) ->
    [];
filter(Ext, _, {ok,Files}) ->
    filter2(Ext, length(Ext), Files).

filter2(_Ext, _Extlen, []) -> [];
filter2(Ext, Extlen,[File|Tail]) ->
    case has_ext(Ext,Extlen, File) of
        true -> [File | filter2(Ext, Extlen, Tail)];
        false -> filter2(Ext, Extlen, Tail)
    end.

has_ext(Ext, Extlen,File) ->
    L = length(File),
    case catch lists:nthtail(L - Extlen, File) of
        Ext -> true;
        _ -> false
    end.

%%%
