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

   :- private(process_merge/0).
   :- info(process_merge/0, [
       comment is 'Implement merge rules rules'
   ]).


    process_merge :-
        simple_word_merge(text),
        simple_lines_merge(text).

    simple_lines_merge(Tag) :-
       ::element(N, P, Tag, Attrs, S),
       A = element(N, P, Tag, Attrs, S),
       ::neighbor(A,B),
       ::lines_mergable(A,B),
       merge(A,B), !,
       simple_lines_merge(Tag).

    simple_lines_merge(_).

    simple_word_merge(Tag) :-
       ::element(N, P, Tag, Attrs, S),
       A = element(N, P, Tag, Attrs, S),
       ::neighbor(A,B),
       ::word_mergable(A,B),
       word_merge(A, B), !,
       simple_word_merge(Tag).

    simple_word_merge(_).

    neighbor1(element(N1, Par, Tag, Attrs1, S1), element(N2, Par, Tag, Attrs2, S2)):-
       ::gen(N1),
       ::element(N1, Par, Tag, Attrs1, S1),
       ::neighbor(element(N1, Par, Tag, Attrs1, S1), element(N2, Par, Tag, Attrs2, S2)).

    merge(E1, E2) :-
       E1 = element(N1, Par, Tag, Attrs1, S1),
       E2 = element(_, Par, Tag, Attrs2, S2),
       merge_bbox_ver(Attrs1, Attrs2, MergedAttrs),
       % debugger::trace,
       ::remove(E2),
       append_bodies(S1, S2, MS),
       ::replace(E1,element(N1, Par, Tag, [joined=true | MergedAttrs], MS)),
       % format("\nSIM: ~w\n", [element(N1, Par, Tag, MergedAttrs, MS)]),
       !.

    word_merge(E1, E2) :-  % TODO: Riechest font (Bold, Italic) -> element in body
       E1 = element(N1, Par, Tag, Attrs1, _),
       E2 = element(_, Par, Tag, Attrs2, S2),
       select_option(font(_), Attrs1, AT11),
       select_option(font(F2), Attrs2, AT21),
       merge_bbox_hor(AT11, AT21, MergedAttrs),
       ::remove(E2),
       append_bodies(E1, S2, MS),
       ::replace(E1,element(N1, Par, Tag, [joined=true, font(F2) | MergedAttrs], MS)),
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
         par_start(element(N1, Par, Tag, Attrs1, S1))

       ),
       option(font(F), Attrs1),
       option(font(F), Attrs2),
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
       ( (B1 >= B2, T1 < B2) ; ( T1 =< T2, B2 > T2) ).

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

:- end_category.
