:- category(degraded).

    :- info([
        version is 1:0:0,
        author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
        date is 2022-09-26,
        comment is 'Process degraded element removal, e.g. page numbers, invisible out-of-text body elements'
    ]).

	:- use_module(lists, [member/2]).
	:- use_module(library(option), [option/2, option/3]).
    :- use_module(library(pcre), [re_match/2, re_match/3,
                                  re_matchsub/4, re_split/4]).

    :- public(process_degraded/0).
    % :- mode(process_degraded, Solutions).
    :- info(process_degraded/0, [
        comment is 'Run degraded element processing'
    ]).

    process_degraded :-
        forall(( ::element(E), ::degraded_within_page(E) ),
               ::remove(E) ).

    :- protected(degraded_within_page/1).
    % :- mode(degraded_within_page, Solutions).
    :- info(degraded_within_page/1, [
        comment is 'Check if the element is degraded within page'
    ]).

    degraded_within_page(element(_, P, text, AT, _)) :-
        ::element(P, _, page, AP, _),
        out_of_page(AT, AP).

    :- protected(degraded_element/2).
    % :- mode(degraded_element, Solutions).
    :- info(degraded_element/2, [
        comment is 'Invisible and colotitle elements, which disallow inter-page merging'
    ]).

    degraded_element(S, P) :-
        ::element(P, _, page, Attrs, _),
        option(number(PN), Attrs),
        ::gettext(S, T),
        ::degraded_check(T, PN).

    :- protected(degraded_check/2).
    % :- mode(degraded_check, Solutions).
    :- info(degraded_check/2, [
        comment is 'Check wether the text is degraded'
    ]).

    degraded_check(S, _) :-
        ::empty_check(S), !.
    degraded_check(T, P) :-
        re_matchsub("\\d+", T, Dict, []),
        get_dict(0, Dict, NS),
        number_string(N, NS),
        N = P, !.

    :- protected(empty_check/1).
    % :- mode(empty_check, Solutions).
    :- info(empty_check/1, [
        comment is 'Whether the text is empty'
    ]).

    empty_check(T) :-
        re_match("^\\s*$", T, []).

    out_of_page(AT, AP) :-
        option(textleft(TL), AP),
        option(left(L), AT),
        L < TL, !.

    out_of_page(AT, AP) :-
        option(texttop(TT), AP),
        option(top(T), AT),
        T < TT, !.

    out_of_page(AT, AP) :-
        option(textleft(TL), AP),
        option(left(L), AT),
        option(textwidth(TW), AP),
        option(width(W), AT),
        TR is TL + TW,
        R is L + W,
        R > TR, !.

    out_of_page(AT, AP) :-
        option(texttop(TT), AP),
        option(top(T), AT),
        option(textheight(TH), AP),
        option(height(H), AT),
        TB is TT + TH,
        B is T + H,
        B > TB, !.

:- end_category.
