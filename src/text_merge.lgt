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
       simple_lines_merge(text),
       simple_lines_merge(text),
       simple_lines_merge(text),
       simple_lines_merge(text),
       simple_lines_merge(text),
       simple_lines_merge(text),
       simple_lines_merge(text),
       simple_lines_merge(text),
       simple_lines_merge(text),
       simple_lines_merge(text),
       simple_lines_merge(text),
       simple_lines_merge(text),
       simple_lines_merge(text),
       simple_lines_merge(text),
       simple_lines_merge(text),
       simple_lines_merge(text),
       simple_lines_merge(text),
       simple_lines_merge(text),
       simple_lines_merge(text),
       simple_lines_merge(text).


    % simple_lines_merge(Tag) :-
    %    atom(Tag),
    %    ::gen(N),
    %    ::element(N, P, T, A, S),
    %    simple_lines_merge(element(N, P, T, A, S)),
    %    fail.

    % simple_lines_merge(Tag) :-
    %    atom(Tag), !.

    % simple_lines_merge(E) :-
    %    ::neighbor(E, B),
    %    lines_similar(E, B), !,
    %    merge(E, B).

    % simple_lines_merge(_).

    simple_lines_merge(Tag) :-
       % debugger::trace,
       A = element(_, _, Tag, _, _),
       neighbor1(A,B),
       ::lines_mergable(A,B),
       merge(A,B),
       fail.

    simple_lines_merge(_).


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
       ::append(element(N1, Par, Tag, [joined=true | MergedAttrs], MS)),
       % format("\nSIM: ~w\n", [element(N1, Par, Tag, MergedAttrs, MS)]),
       !.

    :- private(lines_mergable/2).
    :- info(lines_mergable/2, [
        comment is 'Defines the similarity of lines.'
    ]).

    lines_mergable(element(_, Par, Tag, Attrs1, _), element(N2, Par, Tag, Attrs2, S2)):-
       \+ par_start(element(N2, Par, Tag, Attrs2, S2)),
       option(left(L1), Attrs1),
       option(width(W1), Attrs1),
       option(left(L2), Attrs2),
       option(width(W2), Attrs2),
       ::deviation(paragraph, [LeftD, RightD]),
       DR is abs(L1 - L2),
       DR =< LeftD,
       % DL is abs(L1 + W1 - (L2 + W2)),
       %DL =< RightD,
       (W1 =< W2; W1 =< W2 + RightD),
       % format("\nSIM:|~w| =< ~w\n", [DL, RightD]),
       option(font(F), Attrs1),
       option(font(F), Attrs2),
       !.

   par_start(element(_, _, _, A, _)) :-
       par_option(O),
       option(O, A),
       !.

   par_option(ident(_)).
   par_option(item(_)).
   par_option(ding(_)).
   par_option(parindent(_)).

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
