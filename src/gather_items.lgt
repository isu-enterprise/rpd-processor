:- category(gather_items).

    :- info([
        version is 1:0:0,
        author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
        date is 2022-09-28,
        comment is 'Gather similar items in one bunch'
    ]).

    :- use_module(lists, [member/2]).
	:- use_module(library(option), [option/2, option/3]).
    :- use_module(library(pcre), [re_match/2, re_match/3,
                                  re_matchsub/4, re_split/4]).
    :- protected(process_item_gathering/0).
    % :- mode(process_item_gathering, Solutions).
    :- info(process_item_gathering/0, [
    comment is 'Run the process of item gathering'
    ]).

    process_item_gathering :-  % Move forward till breaking pattern within items or a section.
        ::range(text, Begin, End),
        gather_item(Begin, End, none).

    gather_item(N, End, itemset(ding(V), P, R)) :-
        N =< End,
        ::element(E, N),
        is_item(E, ding(V)), !,
        gather0(E, End, itemset(ding(V), P, R)).
    gather_item(N, End, itemset(item(V), P, R)) :-
        N =< End,
        ::element(E, N),
        is_item(E, item(V1)),
        V < V1,
        V1 < V + 10, !,
        gather0(E, End, itemset(item(V1), P, R)).
    gather_item(N, End, itemset(O, P, R)) :-
        N =< End,
        ::element(E, N),
        is_item(E, O1),
        O =.. [F1, V1],
        O1 =.. [F2, V2],
        (F1 \= F2; V1 \= V2 ),
        gather3(E, End, itemset(O, P, R), O1).
    gather_item(N, End, none) :-
        N =< End,
        ::element(E, N),
        is_item(E, O), !,
        gather0(E, End, itemset(O,N, [none])).
    gather_item(N, End, _) :-
        N =< End,
        ::element(E, N),
        ::section(E, _), !,
        gather1(E, End, none).
    gather_item(N, End, ItemSet) :-
        N =< End,
        ::element(E, N),
        gather1(E, End, ItemSet).
    gather_item(N, E, _) :-
        N > E.

    gather0(E, End, itemset(O, Set, L)):-
        E = element(N, P, text, Attrs, S),
        ::replace(E, element(N, P, text, [itemset=Set | Attrs], S)),
        ( ::next(N, N1) -> gather_item(N1, End, itemset(O, Set, L));
          true ).
    gather1(E, End, ItemSet):-
        E = element(N, _, text, _, _),
        ( ::next(N, N1) -> gather_item(N1, End, ItemSet);
          true ).
    gather3(E, End, itemset(O, P, L), O1) :-
        find_trend([itemset(O, P) | L], O1, ItemSet), !,
        gather0(E, End, ItemSet).
    gather3(E, End, itemset(O, P, L), O1) :-
        E = element(N, _, text, _, _),
        gather0(E, End, itemset(O1, N, [itemset(O, P)|L])).

    find_trend([itemset(ding(V), P) | R], ding(V), itemset(ding(V), P, R)) :-!.
    find_trend([itemset(item(V), P) | R], item(V1), itemset(ding(V), P, R)) :-
        V < V1,
        V1 < V + 10, !.
    find_trend([_ | T], O, ItemSet) :-
        find_trend(T, O, ItemSet).

    is_item(element(N, P, text, Attrs, S), O) :-
        item_option(O, _),
        option(O, Attrs),
        \+ ::section(element(N, P, text, Attrs, S), _).

    item_option(ding(V), V).
    item_option(item(V), V).

:- end_category.
