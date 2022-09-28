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

    gather_item(N, End, itemset(ding(V), P)) :-
        N =< End,
        ::element(E, N),
        is_item(E, ding(V)), !,
        gather0(E, End, itemset(ding(V), P)).
    gather_item(N, End, itemset(item(V), P)) :-
        N =< End,
        ::element(E, N),
        is_item(E, item(V1)),
        V =< V1, !,
        gather0(E, End, itemset(item(V1), P)).
    gather_item(N, End, itemset(O, _)) :-
        N =< End,
        ::element(E, N),
        is_item(E, O1),
        O =.. [F1, V1],
        O1 =.. [F2, V2],
        (F1 \= F2; V1 \= V2 ),
        gather0(E, End, itemset(O1, N)).
    gather_item(N, End, none) :-
        N =< End,
        ::element(E, N),
        is_item(E, O), !,
        gather0(E, End, itemset(O,N)).
    gather_item(N, End, _) :-
        N =< End,
        ::element(E, N),
        ::section(E, _), !,
        gather1(E, End, none).
    gather_item(N, End, _) :-
        N =< End,
        ::element(E, N),
        gather1(E, End, none).

    gather0(E, End, itemset(O, Set)):-
        E = element(N, P, text, Attrs, S),
        ::replace(E, element(N, P, text, [itemset=Set | Attrs], S)),
        ( ::next(N, N1) -> gather_item(N1, End, itemset(O, Set));
          true ).
    gather1(E, End, ItemSet):-
        E = element(N, _, text, _, _),
        ( ::next(N, N1) -> gather_item(N1, End, ItemSet);
          true ).

    is_item(element(_,_, text, Attrs, _), O) :-
        item_option(O, _),
        option(O, Attrs).

    item_option(ding(V), V).
    item_option(item(V), V) :-
        V\="", V\=''.

:- end_category.
