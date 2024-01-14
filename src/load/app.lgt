

:- object(it_chair_2023_2024_b,
   extends(xlsx_load("/home/eugeneai/projects/code/rpd-processor/data/load-2023-2024/distr/каф-70-таб-2-3-5-бюджет-(2023-24).xlsx"))).
:- end_object.

:- object(it_chair_2023_2024_v,
   extends(xlsx_load("/home/eugeneai/projects/code/rpd-processor/data/load-2023-2024/distr/каф-70-таб-2-3-5-вб-(2023-24).xlsx"))).
:- end_object.

:- object(app).

   :- info([
      version is 1:0:0,
      author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
      date is 2024-01-14,
      comment is 'Application for testing loading professor loadings'
   ]).

   :- initialization((
        it_chair_2023_2024_b::load
      , it_chair_2023_2024_v::load
   )).

   :- protected(open_load/0).

:- end_object.
