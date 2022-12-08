:- category(text_sections).

    :- info([
        version is 1:0:0,
        author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
        date is 2022-09-23,
        comment is 'Recognizes text sections and makes references from Attr list with numbers'
    ]).

    :- use_module(lists, [member/2]).
    :- use_module(library(option), [option/2, option/3]).
    :- use_module(library(pcre), [re_match/2, re_match/3,
                                  re_matchsub/4, re_split/4]).

    :- public(process_sections/0).
    :- info(process_sections/0, [
        comment is 'Does recognition of sections in the text'
    ]).

    process_sections :-
        numbered_sections, !,
        named_sections, !,
        % centered_sections, !,
        associate_paragraphs, !,
        true.

    :- protected(numbered_sections/0).
    :- info(numbered_sections/0, [
        comment is 'Process sections defined by numbers'
    ]).

    numbered_sections :-
        forall(::element(N, P, T, A, S),
               num_sec(element(N, P, T, A, S))).

    num_sec(element(N, P, T, A, S)) :-
        option(item(No), A),
        option(itemName(""), A),
        ::gettext(S, Text),
        string_lower(Text, LText),
        % ( N=690 -> debugger::trace; true),
        number_section0(No, Def, LText),
        ::replace(element(N, P, T, A, S),
            element(N, P, T, [ section=Def, property="dc:title" | A ], S)).

    num_sec(_).

    number_section0(N, Sec, Text) :-
        ::number_section(N, Sec, _Parent, Hints),
        ::check_hints(Text, Hints, _), !.

    % number_section0(N, Sec, Text) :-
    %     ::number_section([N], Sec, _, _),
    %     number_section0([N], Sec, Text).

    :- protected(number_section/4).
    :- info(number_section/4, [
        comment is 'Associate a pattern of a numbers to section symbol, parent and its description'
    ]).

    % number_section(1, introduction, none, [введен]).
    % number_section([1,X], subsection(X), introduction, [основн, результат]).

    :- protected(section_title/2).
    :- info(section_title/2, [
        comment is 'Converts an atom defining section into its title'
    ]).

    % section_title(introduction, 'Introduction to an article').
    % section_title(subsection(X), X).

    section_title(contents, "Содержание").

    :- protected(named_sections/0).
    :- info(named_sections/0, [
        comment is 'Process sections defined by substring order'
    ]).

    named_sections :-
        forall(::element(N, P, T, A, S),
               unum_sec(element(N, P, T, A, S))).

    unum_sec(element(N, P, T, A, S)) :-
        \+ ::section(element(N, P, T, A, S), _),
        \+ option(item(_), A),
        ::gettext(S, Text),
        string_lower(Text, LText),
        ::unnumbered_section(Sec, _Parent, Hints),
        ::check_hints(LText, Hints, _), !,
        % check_parent(N, Parent),          % Somewhere up there is a parent
        ::replace(element(N, P, T, A, S),
            element(N, P, T, [ section=Sec, property="dc:title" | A ], S)).

    unum_sec(_).

    check_parent(_, none).
    check_parent(N, Parent) :-
        N >= 1,
        ::prev(N, N0),
        ::element(N0, _, text, SA, _),   % TODO: Make local cash with dynamics parent_section(Sec, Number)
        option(section(Parent), SA), !.

    check_parent(N, Parent) :-
        N >= 1,
        ::prev(N, N0),
        check_parent(N0, Parent).


    :- protected(unnumbered_section/3).
    :- info(unnumbered_section/3, [
        comment is 'Section definition without numbers'
    ]).

    % unnumbered_section(contents, none, [содержание]).
    % unnumbered_section(contents, none, [оглавление]).

    :- protected(associate_paragraphs/0).
    :- info(associate_paragraphs/0, [
        comment is 'Make references from paragraphs to their sections'
    ]).

    associate_paragraphs :-
        ::range(text, Start, End),
        associate(Start, End, none).

    :- public(section/2).
    :- info(section/2, [
        comment is 'Is this element a section?'
    ]).

    section(element(_, _, text, A, _), Id) :-
        option(section(Id), A).

    associate(N, End, _) :-  % Parent section
        N =< End,
        ::element(N, P, text, A, S),
        ::section(element(N, P, text, A, S), Id), !,
        find_parent_section(Id, Parent),
        ::replace(element(N, P, text, A, S),
                  element(N, sp(Parent,P), text, A, S)),  % sp = section, page
        ::next(N,N1),
        associate(N1, End, N).

    associate(N, End, Parent) :-  % Parent section
        N =< End,
        ::element(N, P, text, A, S),
        \+ ::section(element(N,P,text,A,S), _), !,
        ::replace(element(N, P, text, A, S),
                  element(N, sp(Parent,P), text, A, S)),
        ::next(N, N1),
        associate(N1, End, Parent).

    associate(N, End, Parent) :-  % Parent section
        N =< End,
        ::element(N, _, page, _, _), !,
        N1 is N + 1,
        associate(N1, End, Parent).

    associate(N, E, P) :-
        N =< E,
        ::next(N, N1), !,
        associate(N1, E, P).

    associate(N, End, _) :-
        N > End.

    find_parent_section(Id, none) :-
        ::number_section(_, Id, none, _), !.
    find_parent_section(Id, none) :-
        ::unnumbered_section(Id, none, _), !.
    find_parent_section(Id, N) :-
        ::number_section(_, Id, Parent, _),
        find_parent0(Parent, N).
    find_parent_section(Id, N) :-
        ::unnumbered_section(Id, Parent, _),
        find_parent0(Parent, N).
    find_parent_section(Id, none) :-
        (::section_title(Id, Title) -> true;
         Title = "< title not found >"),
        format("% WARNING: Cannot find parent element for section '~w'~n%       ~w~n", [Id, Title]).
    find_parent0(PId, N) :-
        ::element(E),
        E = element(_,_, text, _, _),
        section(E, PId), !,
        E = element(N, _, text, _, _).
    find_parent0(PId, _) :-
        (::section_title(PId, Title) -> true;
         Title = "< title not found >"),
        format("% WARNING: Cannot find element for section '~w'~n%       ~w~n", [PId, Title]),
        fail.



:- end_category.
