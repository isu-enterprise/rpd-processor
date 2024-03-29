:- category(syllabus_merge, extends(text_merge)).

    :- info([
        version is 1:0:0,
        author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
        date is 2022-09-26,
        comment is 'Paragraph merge rules applied for course descriptions'
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
                   element(_, _, text, _, S2)) :-
        ::gettext(S1, T1),
        ::unterminated_sentence(T1),
        ::gettext(S2, T2),
        ::cannot_start_sentence(T2), !.

    lines_mergable(element(_, _, text, A, _),
                   element(_, _, text, _, S2)) :-
        ::list_item(A),
        ::gettext(S2, T2),
        ::cannot_start_sentence(T2), !.

    lines_mergable(element(_, _, text, A1, _),
                   element(N2, _, text, A2, _)) :-
        option(itemset(Set), A1),
        \+ option(itemset(_), A2),
        \+ option(section(_), A2),
        ::next(N2, N3),
        ::element(element(N3, _, text, A3, _)),
        option(itemset(Set), A3), !.

    lines_mergable(element(_, _, text, A1, _),
                   element(N2, _, text, A2, _)) :-
        option(itemset(Set), A1),
        option(itemset(Set1), A2),
        Set1 \= Set, % The only foreign item.
        \+ option(section(_), A2),
        ::next(N2, N3),
        ::element(element(N3, _, text, A3, _)),
        option(itemset(Set), A3), !.

    lines_mergable(element(_, _, text, A1, _),
                   element(N2, _, text, A2, S2)) :-
        option(itemset(Set1), A1),
        \+ option(itemset(_), A2),
        \+ option(section(_), A2),
        ::next(N2, N3),
        ::element(element(N3, _, text, A3, _)),
        option(itemset(Set3), A3),
        Set1 \= Set3, % The only foreign item.
        ::gettext(S2, T2),
        string_length(T2, L2), L2 < 40,
        !.


    lines_mergable(element(_, _, text, _, S1),
                   element(N2, P2, text, A2, S2)) :-
        \+ ::par_start(element(N2, P2, text, A2, S2)),
        ::gettext(S1, T1),
        ::unterminated_sentence(T1), !.

    lines_mergable(element(_, _, text, _, _),
                   element(N2, P2, text, A2, S2)) :-
        \+ ::par_start(element(N2, P2, text, A2, S2)),
        ::gettext(S2, T2),
        ::cannot_start_sentence(T2), !.

    :- protected(unterminated_sentence/1).
    % :- mode(unterminated_sentence, Solutions).
    :- info(unterminated_sentence/1, [
        comment is 'Determine wether the sentence is not well terminted.'
    ]).

    unterminated_sentence(T) :-
        re_match("[-/+=–—]\s*$", T, []).
    unterminated_sentence(T) :-
        string_lower(T, L),
        re_match("url\s*:\s*$", L, []).
    unterminated_sentence(T) :-
        re_match("\s+[а-я]{1,3}\s+$", T, []).
    unterminated_sentence(T) :-
        re_match("%[a-zA-Z]{0,2}$", T, []).

    :- protected(cannot_start_sentence/1).
    % :- mode(cannot_start_sentence, Solutions).
    :- info(cannot_start_sentence/1, [
       comment is 'This sequence cannot start a sentence'
    ]).

    cannot_start_sentence(T) :-
        re_matchsub("^\s*[а-я]+\s*([):]?)", T, Dict, []),
        get_dict(1, Dict, ""), !.

    cannot_start_sentence(T) :-
        re_match("[a-zA-Z]{0,2}%", T, []).

    cannot_start_sentence(T) :-
        re_match("^https?://", T, []).

    :- protected(text_adjust/2).
    % :- mode(text_adjust, Solutions).
    :- info(text_adjust/2, [
        comment is 'Adjust text'
    ]).

    text_adjust(T, A) :-
        re_split("-\s+$", T, [A|_], []), !.

    text_adjust(A, B) :-
        ^^text_adjust(A, B).

    :- protected(list_item/1).
    % :- mode(list_item, Solutions).
    :- info(list_item/1, [
        comment is 'Options contain eiter nonempty ding or item.'
    ]).

    list_item(Attrs) :-
        option(ding(S), Attrs),
        S\="".
    list_item(Attrs) :-
        option(item(I), Attrs),
        I\="".


:- end_category.
