:- category(syllabus_fonts,
            extends(fonts)).

    :- info([
        version is 1:0:0,
        author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
        date is 2022-09-25,
        comment is 'Defines font logics for ISU syllabi'
    ]).

    :- public(process_syllabus_fonts/0).
    % :- mode(process_syllabus_fonts(Arguments), Solutions).
    :- info(process_syllabus_fonts/0, [
        comment is 'Run processing of fonts'
    ]).

    process_syllabus_fonts :-
        ::process_fonts.

:- end_category.
