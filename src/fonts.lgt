:- category(fonts).

    :- info([
        version is 1:0:0,
        author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
        date is 2022-09-25,
        comment is 'Deals with fonts, defines font logic.'
    ]).

    :- use_module(library(lists), [member/2, append/3]).
    :- use_module(library(option), [option/2, option/3, select_option/3]).
    :- use_module(library(pcre), [re_match/2, re_match/3,
                                  re_matchsub/4, re_split/4]).
    :- public(process_fonts/0).
    :- info(process_fonts/0, [
        comment is 'Runs processing of fonts'
    ]).

    process_fonts :-
        ::range(fontspec, Start, End),
        % debugger::trace,
        count_fonts(Start, End),
        true.

    :- protected(count_fonts/2).
    % :- mode(count_fonts, Solutions).
    :- info(count_fonts/2, [
        comment is 'Enumerate fonts, adding them to the database.'
    ]).

    count_fonts(N, End) :-
        N =< End,
        ::element(N, _, fontspec, Attrs, _), !,
        proc_font(N, Attrs),
        ( ::neighbor(element(N, _, fontspec, Attrs, _),
                     element(N2, _, fontspec, _, _)) ->
          count_fonts(N2, End) ; true ).

    proc_font(N, Attrs) :-
        select_option(id(Id), Attrs, A1),
        select_option(color(Color), A1, A2),
        select_option(size(Sz), A2, A3),
        select_option(family(Name), A3, A4), !,
        ::font_features(Name, Family, Bold, Italic, Other),
        append(Other, A4, FAttrs), !,
        ::add_font(Id, N, Sz, Family, Bold, Italic, FAttrs, Color).

    :- private(font_features/5).
    % :- mode(font_features/5, Solutions).
    :- info(font_features/5, [
        comment is 'Decode font name'
    ]).

    font_features(Name, Family, Bold, Italic, Other) :-
        re_matchsub("^.*?\\+(.*?),(.*)$", Name, Dict, []),
        get_dict(1, Dict, Family),
        get_dict(2, Dict, Series),
        string_lower(Series, LSeries),
        font_series(LSeries, Bold, Italic, Other).

    font_features(Name, Family, no, no, []) :-
        re_matchsub("^.*?\\+(.*?)$", Name, Dict, []),
        get_dict(1, Dict, Family), !.

    font_features(Name, Family, no, no, []) :-
        re_matchsub("^(.*?)(PS|MT)?.*$", Name, Dict, []),
        get_dict(1, Dict, Family).
    font_features(Name, _, _, _, _) :-
        format("%%%% ERROR: Cannot deal with font '~w'\n", [Name]),
        fail.

    :- protected(font_series/4).
    % :- mode(font_series, Solutions).
    :- info(font_series/4, [
        comment is 'Decode font series'
    ]).

    font_series(Str, Bold, Italic, Other) :-
        re_split("[,;:]", Str, Split, []),
        font_series0(Split, Bold, Italic, Other).

    font_series0([], no, no, []).
    font_series0(["bold" | T], yes, Italic, O) :- !,
        font_series0(T, _, Italic, O).
    font_series0(["italic" | T], Bold, yes, O) :- !,
        font_series0(T, Bold, _, O).
    font_series0([X | T], Bold, Italic, [X | O]) :-
        font_series0(T, Bold, Italic, O).


    :- protected(equal_font/2).
    % :- mode(equal_font, Solutions).
    :- info(equal_font/2, [
        comment is 'Are argunebts equal in sense of font definition?'
    ]).

    equal_font(A, A) :- !.
    equal_font(F1, F2) :-
        ::font(_, F1, Sz, Family, Bold, Italic, _, Color),
        ::font(_, F2, Sz, Family, Bold, Italic, _, Color), !.

:- end_category.
