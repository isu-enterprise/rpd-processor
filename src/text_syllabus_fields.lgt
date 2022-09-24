:- category(text_syllabus_fields,
    extends(text_fields)).

    :- info([
        version is 1:0:0,
        author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
        date is 2022-09-24,
        comment is 'Recognition configuration for the ISU syllabus fields'
    ]).

	:- use_module(library(option), [option/2, option/3]).
    :- use_module(library(pcre), [re_match/2, re_match/3,
                                  re_matchsub/4, re_split/4]).

    :- public(process_syllabus_fields/0).
    :- info(process_syllabus_fields/0, [
        comment is 'Run the processing syllabus fields'
    ]).

    process_syllabus_fields :- ::process_fields.

    :- protected(field/4).
    :- info(field/4, [
        comment is 'Defines a field in a section with hints'
    ]).

    field(intermediate_control_type, content,  [форм, промежуточн, аттестац, ':'], strip(till("."))).
    field(intermediate_control_hours, content,  [объем, дисциплины, составл, ' '], ich).
    field(author, author,  [разработчик, ':'], author).

    :- protected(get_field_data/3).
    :- info(get_field_data/3, [
        comment is 'Get field data from a string'
    ]).

    get_field_data(Text, ich, [Z,H]) :-
        re_matchsub("^.*?(\\d+).+?ед.*?(\\d+).*?ч.*?$", Text, Dict, []),
        get_dict(1, Dict, Z),
        get_dict(2, Dict, H).
    get_field_data(Text, ich, [Z,H]) :-
        re_matchsub("^.*?(\\d+).+?ч.*?(\\d+).*?ед.*?$", Text, Dict, []),
        get_dict(1, Dict, H),
        get_dict(2, Dict, Z).

    get_field_data(Text, author, [Name, Affils]) :-
        re_matchsub("^\s*(.*?)\s*,\s*(.*?)\\.?$", Text, Dict, []),
        get_dict(1, Dict, Name),
        get_dict(2, Dict, Affils).

    get_field_data(Text, F, Data) :-
        ^^get_field_data(Text, F, Data).

    :- protected(field_title/2).
    :- info(field_title/2, [
        comment is 'Standardized name of a field'
    ]).

    field_title(intermediate_control_type, "Форма промежуточной аттестации:").
    field_title(intermediate_control_hours, "Объем дисциплины составляет ~w зачетных единиц, ~w час.").
    field_title(author, "Разработчик: ~w, ~w.").
:- end_category.
