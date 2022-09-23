:- category(text_sections).

    :- info([
        version is 1:0:0,
        author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
        date is 2022-09-23,
        comment is 'Recognizes text sections and makes references from Attr list with numbers'
    ]).

   :- use_module(lists, [member/2]).
   :- use_module(library(option), [option/2, option/3]).

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
        option(itemname(''), A),
        number_section0(No, Def),
        ::replace(element(N, P, T, A, S),
            element(N, P, T, [ section=Def | A ], S)).

    num_sec(_).

    number_section0(N, Sec) :-
        ::number_section(N, Sec), !.
    number_section0(N, Sec) :-
        ::number_section([N], Sec), !.





    :- protected(number_section/3).
    :- info(number_section/3, [
        comment is 'Associate a pattern of a numbers to section symbol, parent and its description'
    ]).

    % number_section(1, introduction, none).
    % number_section([1,X], subsection(X), introduction).

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

    associate_paragraphs. % TODO






:- end_category.
