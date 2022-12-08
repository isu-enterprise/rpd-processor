:- category(text_fields).

    :- info([
        version is 1:0:0,
        author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
        date is 2022-09-24,
        comment is 'Recognition of fields data, alike Name of person: .....'
    ]).

    :- use_module(lists, [member/2]).
    :- use_module(library(option), [option/2, option/3]).
    :- use_module(library(pcre), [re_match/2, re_match/3,
                                  re_matchsub/4, re_split/4]).

    :- public(process_fields/0).
    :- info(process_fields/0, [
        comment is 'Run the recognition process'
    ]).

    process_fields :-
        forall(::field(Id, Sec, Hints, Term),
               find_fields(Id, Sec, Hints, Term)).

% Protected interface
    :- protected(field/4).
    :- info(field/4, [
        comment is 'Defines a field in a section with hints'
    ]).

    :- protected(field_title/2).
    :- info(field_title/2, [
        comment is 'Standardized name of a field'
    ]).

    find_fields(I, S, H, T) :-
        ::range(_Start, End),
        forall(section(S, E), (find_data_sec(E, I, H, T, End); true)).

    section(Section,
            element(N, sp(Sec, P), text, A, S)) :-
        ::element(N, sp(Sec, P), text, A, S),
        option(section(Section), A).

    find_data_sec(element(Sec, sp(HSec, P), text, A, S),
        Name, Hints, Term, End) :-
        Sec =< End,
        % Section itself can contain data
        % format("SEC: ~w\n", [element(Sec, sp(HSec, P), text, A, S)]),
        find_data0(element(Sec, sp(HSec, P), text, A, S),
                   Name, Hints, Term),
        !,
        next(Sec, End, element(N1, sp(Sec, P1), text, A1, S1)), !,
        find_data(element(N1, sp(Sec, P1), text, A1, S1),
                  Name, Hints, Term, End).

    % find_data_sec(_, _, _, _, _).

    find_data(element(N, sp(Sec, P), text, A, S),
        Name, Hints, Term, End) :-
        N =< End,
        % format("INSEC: ~w\n", [element(N, sp(Sec, P), text, A, S)]),
        find_data0(element(N, sp(Sec, P), text, A, S),
                   Name, Hints, Term), !,
        next(N, End, element(N1, sp(Sec, P1), text, A1, S1)), !,
        find_data(element(N1, sp(Sec, P1), text, A1, S1),
                  Name, Hints, Term, End).

    % find_data(_, _, _, _, _).

    find_data0(element(N, sp(Sec, P), text, A, S),
                   Name, Hints, Term) :-
        ::gettext(S, Text),
        string_lower(Text, LText),
%        ( N = 194 -> debugger::trace; true),
        ::check_hints(LText, Hints, Rest), !,
        string_length(Text, LT),
        string_length(Rest, LR),
        Before is LT - LR,
        sub_string(Text, Before, LR, 0, ORest),
        ::get_field_data(ORest, Term, Data), !,
        ::element(N, _, text, _, _),
        ::replace(element(N, sp(Sec, P), text, A, S),
                  element(N, sp(Sec, P), text, [field=f(Name, Data) | A], S)).

    find_data0(_, _, _, _).

    :- protected(get_field_data/3).
    :- info(get_field_data/3, [
        comment is 'Get field data from a string'
    ]).

    get_field_data(Text, strip(F), Data) :-
        get_field_data(Text, F, D),
        re_matchsub("^\s*(.+?)\s*$", D, Dict, []),
        get_dict(1, Dict, Data).
    get_field_data(Text, till(Str), Data) :-
        sub_string(Text, Before, _, _, Str), !,
        sub_string(Text, 0, Before, _, Data).
    get_field_data(Text, till(_), Text).  % Till the end of line

    next(N, End, element(N2, sp(Sec, P), text, A, S)) :-
        N < End,
        ::next(N, N1),
        getel(N1, End, element(N2, sp(Sec, P), text, A, S)).

    getel(N, _, element(N, sp(Sec, P), text, A, S)) :-
        ::element(N, sp(Sec, P), text, A, S), !.
    getel(N, _, element(N, sp(Sec, _), text, _, _)) :-
        ::element(N, sp(Sec1, _), text, _, _),
        Sec1 \= Sec, !, fail.
    getel(N, End, element(N1, sp(Sec, P), text, A, S)) :-
        % \+ element ....
        next(N, End, element(N1, sp(Sec, P), text, A, S)).


:- end_category.
