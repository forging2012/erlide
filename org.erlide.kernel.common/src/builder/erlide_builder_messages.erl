%% Author: jakob
%% Created: May 11, 2006
%% Description: parses error output from erlc into something more like compile:file(X, [return])

-module(erlide_builder_messages).

%-define(DEBUG, 1).

-include("erlide.hrl").

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
         parse/1
        ]).

% for tests
-export([parse_error/1]).

%%
%% API Functions
%%
parse(Text) ->
    parse_errors(Text, []).

%%
%% Local Functions
%%

parse_errors([], Acc) ->
    lists:reverse(Acc);
parse_errors([PossibleErrorLine | Rest], Acc) when is_list(PossibleErrorLine) ->
    case parse_error(PossibleErrorLine) of
        not_an_error_line ->
            parse_errors(Rest, Acc);
        Tuple ->
            parse_errors(Rest, [Tuple | Acc])
    end;
parse_errors([_ | Rest], Acc) ->
    parse_errors(Rest, Acc).

parse_error(" "++_) ->
    not_an_error_line;
parse_error("ERROR: "++Msg) ->
    Msg1 = extract_message(Msg),
    mk_error(project, "-1", Msg1, error);
parse_error(PossibleErrorLine) ->
    case string:tokens(PossibleErrorLine, ":") of
        [File, LineNoS, " Warning" | WarnText] ->
            mk_error(File, LineNoS, msg_text(WarnText), warning);
        [File, LineNoS | ErrText] ->
            mk_error(File, LineNoS, msg_text(ErrText), error);
        _ ->
            not_an_error_line
    end.

msg_text(Text) ->
    string:strip(string:strip(string:join(Text, ":"), right, $\n)).

mk_error(_, _, "", _) ->
    not_an_error_line;
mk_error(File, LineNoS, Msg, Severity) ->
    Line = to_line(LineNoS),
    case Line of
        -2 ->
             not_an_error_line;
        _ ->
            {File, Line, Msg, Severity}
    end.

to_line("none") ->
    -1;
to_line(LineS) ->
    try
        list_to_integer(LineS)
    catch
        _:_ ->
            -2
    end.

extract_message(Msg) ->
    try
        extract_message_aux(Msg)
    catch
        _:_ ->
            Msg0 = lists:flatten(Msg),
            Lines = string:tokens(Msg0, "\n"),
            CleanLines = [string:strip(Line) || Line<-Lines],
            string:join(CleanLines, " ")
    end.

extract_message_aux(Msg) ->
    Msg0 = lists:flatten(Msg),
    Lines = string:tokens(Msg0, "\n"),
    [Hdr0 | Rest] = Lines,
    Ix = string:str(Hdr0, ": "),
    {Hdr, Hdr1} = lists:split(Ix+1, Hdr0),
    CleanLines = string:join([Hdr1 | [string:strip(Line) || Line<-Rest]], " "),
    Hdr++extract_stacktrace_top(CleanLines).

extract_stacktrace_top(Text) ->
    {ok, Tokens, _} = erl_scan:string(Text++"."),
    {ok, Expr} = erl_parse:parse_term(Tokens),
    case Expr of
        {_, _, {error, Cause, Stack}} ->
            [{M,F,A,_}|_] = Stack,
            lists:flatten(io_lib:format("~s ~s:~s/~w", [Cause, M, F, length(A)]));
        _ ->
            Text
    end.

