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
       % simple_row_merge(text),
       simple_lines_merge(text).

    simple_lines_merge(Tag) :-
       forall(
          (
             neighbor1(element(N, Par, Tag, Attrs, S), B),
             lines_similar(element(N, Par, Tag, Attrs, S), B)
          ), merge(element(N, Par, Tag, Attrs, S), B)).

    neighbor1(element(N1, Par, Tag, Attrs1, S1), element(N2, Par, Tag, Attrs2, S2)):-
       ::gen(N1),
       ::element(N1, Par, Tag, Attrs1, S1),
%       debugger::trace,
       ::neighbor(element(N1, Par, Tag, Attrs1, S1), element(N2, Par, Tag, Attrs2, S2)).

    merge(E1, E2) :-
       E1 = element(N1, Par, Tag, Attrs1, S1),
       E2 = element(_, Par, Tag, Attrs2, S2),
       % debugger::trace,
       merge_bbox(Attrs1, Attrs2, MergedAttrs),
       ::remove(E1), ::remove(E2),
       append_bodies(S1, S2, MS),
       ::append(element(N1, Par, Tag, MergedAttrs, MS)),
       % format("\nSIM: ~w\n", [element(N1, Par, Tag, MergedAttrs, MS)]),
       !.

    lines_similar(element(_, Par, Tag, Attrs1, _), element(_, Par, Tag, Attrs2, _)):-
       option(left(L1), Attrs1),
       option(width(W1), Attrs1),
       option(left(L2), Attrs2),
       option(width(W2), Attrs1),
       ::deviation(paragraph, [LeftD, RightD]),
       abs(L1 - L2) =< LeftD,
       abs(L1 + W1 - (L2 + W2)) =< RightD,
       option(font(F), Attrs1),
       option(font(F), Attrs2),
       !.

   merge_bbox(A1, A2, [left(L), width(W), height(H) | T]) :-
       select_option(left(L1), A1, A11),
       select_option(left(L2), A2, A21),
       L is min(L1, L2),
       select_option(width(W1), A11, A12),
       select_option(width(W2), A21, A22),
       R1 is L1 + W1,
       R2 is L2 + W2,
       R is max(R1, R2),
       W is R - L,
       select_option(height(H1), A12, T1),
       select_option(height(H2), A22, A23),
       H is H1 + H2,
       select_option(top(_), A23, T2),
       merge_options(T1, T2, T).

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
