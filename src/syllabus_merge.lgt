:- category(syllabus_merge, extends(text_merge)).

    :- info([
        version is 1:0:0,
        author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
        date is 2022-09-26,
        comment is 'Description'
    ]).

    :- use_module(lists, [member/2]).
	:- use_module(library(option), [option/2, option/3]).
    :- use_module(library(pcre), [re_match/2, re_match/3,
                                  re_matchsub/4, re_split/4]).

    :- protected(lines_mergable/2).
    % :- mode(lines_mergable, Solutions).
    :- info(lines_mergable/2, [
        comment is 'Rules for assesing wether two lines can merge'
    ]).

    lines_mergable(A, B) :-
        ^^lines_mergable(A, B), !.

    lines_mergable(element(_, _, text, _, S1),
                   element(N2, P2, text, A2, S2)) :-
        \+ ::par_start(element(N2, P2, text, A2, S2)),
        ::gettext(S1, T1),
%        string_lower(T1, LT1),
        ::unterminated_sentence(T1), !.

    lines_mergable(element(_, _, text, _, _),
                   element(N2, P2, text, A2, S2)) :-
        \+ ::par_start(element(N2, P2, text, A2, S2)),
        ::gettext(S2, T2),
%        string_lower(T2, LT2),
        ::cannot_start_sentence(T2), !.

    :- protected(unterminated_sentence/1).
    % :- mode(unterminated_sentence, Solutions).
    :- info(unterminated_sentence/1, [
        comment is 'Determine wether the sentence is not terminted.'
    ]).

    unterminated_sentence(T) :-
        re_match("[-/+=–—]\s*$", T, []).
    unterminated_sentence(T) :-
        string_lower(T, L),
        re_match("url\s*:\s*$", L, []).
    unterminated_sentence(T) :-
        re_match("\s+[а-я]{1,3}\s+$", T, []).

    :- protected(cannot_start_sentence/1).
    % :- mode(cannot_start_sentence, Solutions).
    :- info(cannot_start_sentence/1, [
       comment is 'This sequence cannot start a sentence'
    ]).

    cannot_start_sentence(T) :-
        re_matchsub("^\s*[а-я]+\s*([):]?)", T, Dict, []),
        get_dict(1, Dict, ""), !.

    :- protected(text_adjust/2).
    % :- mode(text_adjust, Solutions).
    :- info(text_adjust/2, [
        comment is 'Adjust text'
    ]).

    text_adjust(T, A) :-
        re_split("-\s+$", T, [A|_], []), !.

    text_adjust(A, B) :-
        ^^text_adjust(A, B).

:- end_category.
