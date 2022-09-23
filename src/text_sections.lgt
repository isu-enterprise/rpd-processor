:- category(text_sections).

    :- info([
        version is 1:0:0,
        author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
        date is 2022-09-23,
        comment is 'Recognizes text sections and makes references from Attr list with numbers'
    ]).


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
        forall(::element(E), num_sec(E)).

    :- protected(number_section/4).
    :- mode(number_section, Solutions).
    :- info(number_section/4, [
        comment is 'Associate a pattern of a numbers to section symbol, parent and its description'
    ]).

    % number_section(1, introduction, none, 'Introduction to an article').
    % number_section([1,X], subsection(X), introduction, 'Introduction to an article').


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








:- end_category.
