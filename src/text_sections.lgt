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
        % named_sections, !,
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
        number_section0(No, Def, Text),
        ::replace(element(N, P, T, A, S),
            element(N, P, T, [ section=Def | A ], S)).

    num_sec(_).

    number_section0(N, Sec, Text) :-
        ::number_section(N, Sec, _, Hints), !,
        % hints_re(Hints, RegExp), !,
        string_lower(Text, LText), !,
        % re_matchsub(RegExp, LText, Dict, []), !,
        % debugger::trace,
        check_hints(LText, Hints),
        format("NS: ~w ~w\n", [LText, Hints]).

    number_section0(N, Sec) :-
        ::number_section([N], Sec), !.

    check_hints(_, []).
    check_hints(Text, [H|T]) :-
        sub_string(Text, _, _, After, H), !,
        sub_string(Text, _, After, 0, SubText), !,
        check_hints(SubText, T).

    % hints_re([Word], Word).
    % hints_re([Word|T], RE) :-
    %     hints_re(T, TRE),
    %     string_concat(Word, "|", Word1),
    %     string_concat(Word1, TRE, RE).

    :- protected(number_section/4).
    :- info(number_section/4, [
        comment is 'Associate a pattern of a numbers to section symbol, parent and its description'
    ]).

    % number_section(1, introduction, none, [введен]).
    % number_section([1,X], subsection(X), introduction, [основн, результат]).

    :- protected(section_tite/2).
    :- info(section_tite/2, [
        comment is 'Converts an atom defining section into its title'
    ]).

    % section_label(introduction, 'Introduction to an article').
    % section_label(subsection(X), X).

    :- protected(named_sections/0).
    :- info(named_sections/0, [
        comment is 'Process sections defined by substring order'
    ]).

    named_sections :-
        true.


    :- protected(associate_paragraphs/0).
    :- info(associate_paragraphs/0, [
        comment is 'Make references from paragraphs to their sections'
    ]).

    associate_paragraphs :-
        forall( section(E), associate(E)).

    :- public(section/1).
    :- info(section/1, [
        comment is 'Is this element a section?'
    ]).

    section(E) :-
        ::gen(N),
        ::element(N, P, T, A, S),
        E = element(N, P, T, A, S),
        is_section(E).

    is_section(element(N, P, T, A, S)) :-
        option(section(_), A).

    associate(E) :-
        E = element(Start, P, text, A, S),
        S1 is Start + 1,
        ::gen(S1, N),
        ::element(S1, Par, text, A2, S2),
        ( is_section(element(S1, Par, text, A2, S2) -> !, fail; true),
        ::replace(element(S1, Par, text, A2, S2),
                  element(S1, Par, text, [in_section=Start|A2], S2)),
        fail.

    associate(_).






:- end_category.
