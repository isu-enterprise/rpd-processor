
:- category(text_features).

   :- info([
      version is 1:0:0,
         author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
         date is 2022-09-17,
         comment is 'Assigns fetures for lines by its content. Requires knowing text width! '
      ]).

    :- public(process_features/0).
    :- info(process_features/0, [
        comment is 'Process lines individually without context.'
    ]).

    :- use_module(lists, [member/2]).
	:- use_module(library(option), [option/2, option/3]).
    :- use_module(library(pcre), [re_match/2, re_match/3,
                                  re_matchsub/4, re_split/4]).

    process_features :-
        forall(::element(N, _, text, _, _), update_attrs(N)).

    update_attrs(N) :-
        starts_with_indent(N),
        fail.

    update_attrs(N) :-
        starts_field(N),
        fail.

    update_attrs(N) :-
        starts_with_numbering(N),
        fail.

    update_attrs(N) :-
        starts_with_ding(N),
        fail.

    update_attrs(N) :-
        paragraph_end(N),
        fail.

    update_attrs(_).

    starts_field(N) :-   % Цели: ... Задачи: ...
        ::element(N, P, T, A, S), !,
        ::gettext(S, Text),
        re_matchsub("^\\s*?([А-Я][а-яА-Я]{2,})\\s*(:|\\))", Text, Dict, []),
        % TODO задачи: цели: .... from a user set of fields
        % format("RE: ~w | ~w\n", [Dict, Text]),
        long_enough(Text, Dict),
        get_dict(1, Dict, ItemName),
        ItemName \= "",
        ::replace(element(N, P, T, A, S),
                  element(N, P, T,
                          [descr=ItemName, dict=Dict | A], S)),
        !.

    starts_with_numbering(N) :-
        ::element(N, P, T, A, S), !,
        ::gettext(S, Text),
        re_matchsub("^\\s*?(разд?е?л?|тема?)?\\.?\\s*((\\d{1,3}\\.?)+(\\.|:|\\)|\s)|[а-кА-К]\\))"/i, Text, Dict, []),
        % format("RE: ~w\n~w\n", [Dict, Text]),
        get_dict(2, Dict, Item),
        Item \= "",
        long_enough(Text, Dict),
        get_dict(1, Dict, ItemName),
        convert_item(Item, CItem),
        % format("FRM2:~w ~w\n",[Item, CItem]),
        ::replace(element(N, P, T, A, S),
                  element(N, P, T,
                          [item=CItem, itemName=ItemName, dict=Dict | A],
                          S)),
        !.

    starts_with_ding(N) :-
        ::element(N, P, T, A, S), !,
        ::gettext(S, Text),
        % format("\nDIN: ~w < ~w\n", [S,Text]),
        ::lstrip(Text, TS, " "),
        sub_string(TS, 0, 1, _, Ding),
        ::ding_symbol(Ding),
        ::replace(element(N, P, T, A, S), element(N, P, T, [ding=Ding | A], S)),
        !.

    starts_with_indent(N) :-
        ::element(N, P, T, A, S), !,
        option(left(L), A),
        ::element(P, _, _, AP, _),
        option(textleft(LP), AP),
        ::deviation(parindent, [I]),
        LP + I =< L,
        ::replace(element(N, P, T, A, S), element(N, P, T, [parindent=true | A], S)),
        !.

    paragraph_end(N) :-
        ::element(N, P, T, A, S), !,
        option(width(W), A),
        ::deviation(paragraph, [_, RD]),
        W < RD,
        ::replace(element(N, P, T, A, S),
            element(N, P, T, [ parend=true | A], S)),
        !.

    long_enough(S, Dict) :-
        get_dict(0, Dict, Matched), % Matching from the brgininning of string
        sub_string(S, 0, _, After, Matched),
        ::deviation(itemtextminlength, [Length]),
        After > Length.

    convert_item(Item, IntItem) :-
        number_string(IntItem, Item), !.

    convert_item(Item, Result) :-
        re_split("\\d+|[а-яА-я]", Item, L, []),
        filter0(L, List, skip),
        ( List = [Result] -> true;
          Result=List ).

    filter0([_], [], skip).
    filter0([_|T1], T2, skip) :-
        filter0(T1, T2, number).
    filter0([X|T1], [IX|T2], number) :-
        number_string(IX, X), !,
        filter0(T1, T2, skip).
    filter0([X|T1], [X|T2], number) :-
        filter0(T1, T2, skip).


    :- protected(ding_symbol/1).
    :- info(ding_symbol/1, [
        comment is 'List of Ding symbols used for bullet list bullets.'
    ]).

    ding_symbol("").
    ding_symbol("●").
    ding_symbol("-").
    ding_symbol("—").
    ding_symbol("–").

:- end_category.
