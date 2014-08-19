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
    mk_error(project, "none", lists:flatten(Msg), error);
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
