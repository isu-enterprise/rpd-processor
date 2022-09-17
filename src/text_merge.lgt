:- category(text_merge).

   :- info([
      version is 1:0:0,
      author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
      date is 2022-09-17,
      comment is 'Implements text merge procedures over line sets'
   ]).

    :- private(process_merge/0).
    :- info(process_merge/0, [
       comment is 'Implement merge rules rules'
    ]).

    process_merge.
    process_merge :-
       simple_merge(text).

    simple_merge(Tag) :-
       forall(
          neighbor1(element(N, Par, Tag, Attrs, S), B),
          merge(element(N, Par, Tag, Attrs, S), B)).

    neighbor1(element(N1, Par, Tag, Attrs1, S1), element(N2, Par, Tag, Attrs2, S2)):-
       ::gen(N1),
       ::element(N1, Par, Tag, Attrs1, S1),
       ::neighbor(element(N1, Par, Tag, Attrs1, S1), element(N2, Par, Tag, Attrs2, S2)).


:- end_category.
