:- category(syllabus_page_one,
        extends(text_merge)).

    :- info([
        version is 1:0:0,
        author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
        date is 2022-09-20,
        comment is 'The category to analyze the first page of a syllabus.'
    ]).

    :- use_module(lists, [member/2, append/3]).
    :- use_module(library(option), [option/2, option/3,
                                    select_option/3, select_option/4,
                                    merge_options/3]).

    :- public(process_first_page/0).
    :- info(process_first_page/0, [
        comment is 'Runs the first page processing'
    ]).

    process_first_page :-
        find_first_page(E), !,
        process(E).

    process_first_page.

    process(E) :-
        simple_lines_merge_of_page(E).

    find_first_page(element(P, PP, Tag, PAttrs, PS)) :-
        ::gen(N),           % Run in order
        ::element(N, P, text, _, S),
        ::gettext(S, T),
        string_upper(T, UT),
        sub_string(UT, _, _, _, "МИНИСТЕРСТВО"), !,
        ::element(P, PP, Tag, PAttrs, PS).

    simple_lines_merge_of_page(element(P, _, page, _, _)) :-
        simple_lines_merge(P, text).


    :- protected(simple_lines_merge/2).
    :- info(simple_lines_merge/2, [
       comment is 'Merges two lines'
    ]).

    simple_lines_merge(Par, Tag) :-
       ::element(N, Par, Tag, Attrs, S),
       A = element(N, Par, Tag, Attrs, S),
       ::neighbor(A,B),
       % debugger::trace,
       lines_mergable(A,B),
       ::merge(A,B), !,
       simple_lines_merge(Par, Tag).

    simple_lines_merge(_,_).

    :- protected(lines_mergable/2).
    :- info(lines_mergable/2, [
        comment is 'Check wether lines are mergable'
    ]).


    lines_mergable(
           element(_, Par, Tag, Attrs1, _),
           element(_, Par, Tag, Attrs2, _)):-
       option(font(F), Attrs1),
       option(font(F), Attrs2).

:- end_category.
