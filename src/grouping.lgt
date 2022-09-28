:- category(grouping).

    :- info([
        version is 1:0:0,
        author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
        date is 2022-09-28,
        comment is 'Grouping elements in HTML-like constructions'
    ]).

    :- use_module(lists, [member/2, append/3]).
	:- use_module(library(option), [option/2, option/3]).
    :- use_module(library(pcre), [re_match/2, re_match/3,
                                  re_matchsub/4, re_split/4]).

    :- protected(process_grouping/0).
    % :- mode(process_grouping, Solutions).
    :- info(process_grouping/0, [
        comment is 'Run grouping phase.'
    ]).

    process_grouping :-
        groping_items.

    :- protected(groping_items/0).
    % :- mode(groping_items, Solutions).
    :- info(groping_items/0, [
        comment is 'Grouping ttems in ol or ul lists'
    ]).

    groping_items :-
        ::range(text, Begin, End),
        groping_items(Begin, End, none).

    groping_items(N, End, none) :-
        N =< End,
        ::element(element(N, P, text, Attrs, S)),
        option(itemset(Set), Attrs), !,
        container(O, _, Cnt),
        option(O, Attrs), !,
        % ( Set=1067 -> debugger::trace; true),
        Q = element(N, P, text, [ group=Cnt | Attrs], [element(N, P, text, Attrs, S)]),
        ::replace(element(N, P, text, Attrs, S), Q),
        grouping0(End, Q, group(Set, Cnt, N)).
    groping_items(N, End, none) :-
        N =< End,
        ::element(element(N, _, text, Attrs, _)),
        \+ option(itemset(_), Attrs), !,
        ( ::next(N, N1) -> groping_items(N1, End, none); true).

    groping_items(_, _, _).

    grouping0(End, element(N, P, text, Attrs, S), group(Set, Cnt, N)) :-
        (
          ::next(N, N1),
          ::element(N1, P1, text, Attrs1, S1),
          option(itemset(Set), Attrs1) ->
            append(S, [element(N1, P1, text, Attrs1, S1)], SN),
            ::replace(element(N, P, text, Attrs, S), element(N, P, text, Attrs, SN)),
            ::remove(element(N1, P1, text, Attrs1, S1)),
            grouping0(End, element(N, P, text, Attrs, SN), group(Set, Cnt, N));
          ( ::next(N, N1) -> groping_items(N1, End, none) ; true)).


    container(ding(V), V, ul).
    container(item(V), V, ol).


:- end_category.
