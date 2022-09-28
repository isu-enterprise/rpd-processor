:- category(text_merge).

    :- info([
        version is 1:0:0,
        author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
        date is 2022-09-17,
        comment is 'Implements text merge procedures over line sets'
    ]).

    :- use_module(lists, [member/2, append/3]).
    :- use_module(library(option), [option/2, option/3,
                                   select_option/3, select_option/4,
                                   merge_options/3]).

    :- protected(process_merge/0).
    :- info(process_merge/0, [
        comment is 'Implement merge rules rules'
    ]).


    process_merge :-
        ::range(text, Start, End),
        simple_words_merge(Start, End, text),
        simple_lines_merge(Start, End, text),
        page_lines_merge(page),
        true.

    :- protected(process_runs_merge/0).
    % :- mode(process_runs_merge, Solutions).
    :- info(process_runs_merge/0, [
        comment is 'Process merge of runs in a line'
    ]).

    process_runs_merge :-
        ::range(text,  Start, End),
        simple_words_merge(Start, End, text).


    :- protected(simple_lines_merge/3).
    :- info(simple_lines_merge/3, [
       comment is 'Merges two lines'
    ]).

    simple_lines_merge(Start, End, Tag) :-
       repeat,
       simple_lines_merge(Start, End, Tag, Merged),
       format("% Lines merged: ~w\n", [Merged]),
       Merged == 0, !.
    simple_lines_merge(_, _, _).

    simple_lines_merge(N, End, Tag, Merged) :-
       N < End,
       ::element(N, P, Tag, Attrs, S), !,
       A = element(N, P, Tag, Attrs, S),
       lines_merge_next(N, End, A, Merged).
    simple_lines_merge(_, _, _, 0).

    lines_merge_next(N, End, A, Merged):-
       ::neighbor(A,B), !,
       A = element(N, _, Tag, _, _),
       ( ::lines_mergable(A,B) ->
           % format("M:\n~w\n~w\n", [A, B]),
           merge(A,B), !,
           simple_lines_merge(N, End, Tag, Merged2),
           Merged is Merged2 + 1
           ;
           B = element(N1, _, Tag, _, _),
           simple_lines_merge(N1, End, Tag, Merged) ).
    lines_merge_next(_, _, _, 0).


    :- protected(simple_words_merge/3).
    :- info(simple_words_merge/3, [
        comment is 'Merges two lines if condition is ok'
    ]).

    simple_words_merge(Start, End, Tag) :-
       repeat,
       simple_words_merge(Start, End, Tag, Merged),
       format("% Runs merged: ~w\n", [Merged]),
       Merged == 0, !.
    simple_words_merge(_, _, _).

    simple_words_merge(N, End, Tag, Merged) :-
       N < End,
       ::element(N, P, Tag, Attrs, S), !,
       A = element(N, P, Tag, Attrs, S),
       % (N = 240 -> debugger::trace; true),
       words_merge_next(N, End, A, Merged).
    simple_words_merge(_, _, _, 0).

    words_merge_next(N, End, A, Merged) :-
       ::neighbor(A,B), !,
       A = element(N, _, Tag, _, _),
       ( word_mergable(A,B) ->
             word_merge(A, B), !,
             simple_words_merge(N, End, Tag, Merged2),
             Merged is Merged2 + 1
         ;
             B = element(N1, _, Tag, _, _),
             simple_words_merge(N1, End, Tag, Merged) ).
    words_merge_next(_, _, _, 0).

    page_lines_merge(Tag) :-
       ::element(N, P, Tag, Attrs, S),
       A = element(N, P, Tag, Attrs, S),
       % format("NP1:\n~w\n~w\n", [A,B]),
       ::neighbor(A, B),
       % format("NP2:\n~w\n~w\n", [A,B]),
       last_line(A, B, EA),
       first_line(B, EB),
       ::lines_mergable(EA,EB),
       merge(EA,EB), !,
       page_lines_merge(Tag).

    page_lines_merge(_).

    first_line(element(PN, _, page, _, _), element(N, PN, text, A, S)) :-
       Start is PN + 1,
       ::gen(Start, N),
       ::element(N, PN, text, A, S), !.

    last_line(element(PN, _, page, _, _),
              element(P2N, _, page, _, _),
              element(N, PN, text, A, S)) :-
       End is P2N - 1,
       ::ngen(End, N),
       ::element(N, PN, text, A, S), !.

    neighbor1(element(N1, Par, Tag, Attrs1, S1), element(N2, Par, Tag, Attrs2, S2)):-
       ::gen(N1),
       ::element(N1, Par, Tag, Attrs1, S1),
       ::neighbor(element(N1, Par, Tag, Attrs1, S1), element(N2, Par, Tag, Attrs2, S2)).

    :- protected(merge/2).
    :- info(merge/2, [
        comment is 'Merges two lines'
    ]).


    merge(E1, E2) :-
       E1 = element(N1, Par, Tag, Attrs1, S1),
       E2 = element(_, Par, Tag, Attrs2, S2), !,
       merge_bbox_ver(Attrs1, Attrs2, MergedAttrs),
       ::remove(E2),
       append_bodies(S1, S2, MS),
       ::replace(E1,element(N1, Par, Tag, [ljoined=true | MergedAttrs], MS)),
       !.

    merge(E1, E2) :-
       E1 = element(N1, Par1, Tag, Attrs1, S1),
       E2 = element(_, Par2, Tag, Attrs2, S2),
       Par2 \= Par1,
       !,
       % debugger::trace,
       merge_bbox_page(Par1, Par2, Attrs1, Attrs2, MergedAttrs),  % TODO: Incorrect over pages
       ::remove(E2),
       append_bodies(S1, S2, MS),
       ::replace(E1,element(N1, Par1, Tag, [ljoined=true | MergedAttrs], MS)),
       % TODO: Add neighboring for joined line of different pages!

       % format("\nSIM: ~w\n", [element(N1, Par, Tag, MergedAttrs, MS)]),
       !.

    :- protected(word_merge/2).
    :- info(word_merge/2, [
        comment is 'Merges runs in a line'
    ]).

    word_merge(E1, E2) :-  % TODO: Riechest font (Bold, Italic) -> element in body
       E1 = element(N1, Par, Tag, Attrs1, _),
       E2 = element(_, Par, Tag, Attrs2, S2),
       select_option(font(_), Attrs1, AT11),
       select_option(font(F2), Attrs2, AT21),
       merge_bbox_hor(AT11, AT21, MergedAttrs),
       ::remove(E2),
       append_bodies(E1, S2, MS),
       ::replace(E1,element(N1, Par, Tag, [wjoined=true, font(F2) | MergedAttrs], MS)),
       !.

    :- protected(lines_mergable/2).
    :- info(lines_mergable/2, [
        comment is 'Defines wether lines can be merged in one.'
    ]).

    lines_mergable(element(N1, Par, Tag, Attrs1, S1), element(N2, Par, Tag, Attrs2, S2)):-
       \+ par_start(element(N2, Par, Tag, Attrs2, S2)),
       option(left(L1), Attrs1),
       option(left(L2), Attrs2),
       ::deviation(paragraph, [LeftD, _RightD]),
       (
         DR is abs(L1 - L2),
         DR =< LeftD
         ;
         ::element(Par, _, page, PAttrs, _),
         option(textleft(PTL), PAttrs),
         DR1 is abs(L2 - PTL),
         DR1 =< LeftD,
         par_start(element(N1, Par, Tag, Attrs1, S1)) ),
       option(font(F1), Attrs1),
       option(font(F2), Attrs2),
       ::equal_font(F1, F2),
       !.

    lines_mergable(element(_N1, Par1, Tag, Attrs1, _S1), element(N2, Par2, Tag, Attrs2, S2)):-
       Par1 \= Par2,
       \+ par_start(element(N2, Par2, Tag, Attrs2, S2)),
       option(left(L2), Attrs2),
       ::deviation(paragraph, [LeftD, _RightD]),
       ::element(Par2, _, page, PAttrs, _),
       option(textleft(PTL), PAttrs),
       DR1 is abs(L2 - PTL),
       DR1 =< LeftD,
       % par_start(element(N1, Par1, Tag, Attrs1, S1)),
       option(font(F1), Attrs1),
       option(font(F2), Attrs2),
       ::equal_font(F1, F2),
       !.

    :- protected(word_mergable/2).
    :- info(word_mergable/2, [
       comment is 'Check wether two elements can be mergable as a common line.'
    ]).

    word_mergable(element(_, Par, Tag, Attrs1, _), element(_, Par, Tag, Attrs2, _)):-
       option(left(L1), Attrs1),
       option(left(L2), Attrs2),
       option(width(W1), Attrs1),
       % option(width(W2), Attrs2),
       ::deviation(paragraph, [LeftD, _RightD]),
       R1 is L1 + W1,
       abs(L2 - R1) < LeftD,
       option(top(T1), Attrs1),
       option(height(H1), Attrs1),
       B1 is T1 + H1,
       option(top(T2), Attrs2),
       option(height(H2), Attrs2),
       B2 = T2 + H2,
       ( (T1 >= B2 ; T2 >= B1) -> fail; true ).

    :- protected(par_start/1).
    % :- mode(par_start, Solutions).
    :- info(par_start/1, [
        comment is 'Check if line is a paragraph start.'
    ]).

    par_start(element(_, _, _, A, _)) :-
        par_option(O),
        option(O, A),
        !.

    par_option(ident(_)).
    par_option(item(_)).
    par_option(ding(_)).
    par_option(parindent(_)).
    par_option(descr(_)).

    merge_bbox_ver(A1, A2, [left(L), top(To1), width(W), height(H) | T]) :-
        select_option(left(L1), A1, A11),
        select_option(left(L2), A2, A21),
        L is min(L1, L2),
        select_option(width(W1), A11, A12),
        select_option(width(W2), A21, A22),
        R1 is L1 + W1,
        R2 is L2 + W2,
        R is max(R1, R2),
        W is R - L,
        select_option(height(_), A12, A13),
        select_option(height(H2), A22, A23),
        select_option(top(To1), A13, T1),
        select_option(top(To2), A23, T2),
        H is To2 + H2 - To1,
        merge_options(T1, T2, T).

    merge_bbox_page(P1, P2, A1, A2, [left(L), top(To1), width(W), height(H) | T]) :-
        ::element(P1, _, page, PAttrs1, _),
        ::element(P2, _, page, PAttrs2, _),
        select_option(left(L1), A1, A11),
        select_option(left(L2), A2, A21),
        option(textleft(TL1), PAttrs1),
        option(textleft(TL2), PAttrs2),
        DTL is TL1 - TL2,
        L is min(L1, L2 + DTL),
        select_option(width(W1), A11, A12),
        select_option(width(W2), A21, A22),
        R1 is L1 + W1,
        R2 is L2 + DTL + W2,
        R is max(R1, R2),
        W is R - L,
        select_option(height(H1), A12, A13),
        select_option(height(H2), A22, A23),
        select_option(top(To1), A13, T1),
        select_option(top(_To2), A23, T2),
        H is H1 + H2,  % Me'sa  too lazy.
        merge_options(T1, T2, T).

    merge_bbox_hor(A1, A2, [left(L), top(To), width(W), height(H) | T]) :-
        select_option(left(L1), A1, A11),
        select_option(left(L2), A2, A21),
        L is min(L1, L2),
        select_option(width(W1), A11, A12),
        select_option(width(W2), A21, A22),
        R1 is L1 + W1,
        R2 is L2 + W2,
        R is max(R1, R2),
        W is R - L,
        select_option(height(H1), A12, A13),
        select_option(height(H2), A22, A23),
        H is max(H1, H2),
        select_option(top(To1), A13, T1),
        select_option(top(To2), A23, T2),
        To is min(To1, To2),
        merge_options(T1, T2, T).

    append_bodies(element(N, P, Tag, A, S),
                  S1,
                  [element(N, P, Tag, A, S) | L1]) :-
                     ( string(S1) -> L1 = [S1];
                       L1 = S1
                     ).
    append_bodies("", S, S) :- string(S), !.
    append_bodies(S, "", S) :- string(S), !.
    append_bodies(S1, S2, [S1, S2]) :-
       string(S1),
       string(S2), !.
    append_bodies(L1, S, L) :-
       string(S), !,
       append(L1, [S], L).
    append_bodies(S, L2, [S | L2]) :-
       string(S), !.
    append_bodies(L1, L2, L3) :-
       append(L1, L2, L3).

    :- protected(text_adjust/2).
    % :- mode(text_adjust, Solutions).
    :- info(text_adjust/2, [
        comment is 'Adjust a run of a text, e.g. remove hyphenations.'
    ]).

    text_adjust(T, T). % No adjust by default.

:- end_category.
