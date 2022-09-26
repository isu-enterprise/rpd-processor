:- category(htmlize).

    :- info([
        version is 1:0:0,
        author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
        date is 2022-09-26,
        comment is 'Description'
    ]).

	:- use_module(library(sgml_write), [html_write/3]).
    :- use_module(library(option), [select_option/3, select_option/4,
                                    option/3, option/2]).

    :- public(htmlize/1).
    % :- mode(htmlize, Solutions).
    :- info(htmlize/1, [
        comment is 'Convert context to html.'
    ]).

    htmlize(FileName) :-
        string(FileName),
        htmlize(HTML),
        open(FileName, write, Stream, []),
        % write(HTML), nl,
        html_write(Stream, HTML, []),
        close(Stream).
    htmlize(HTML) :-
        var(HTML),
        ::range(text, Start, End),
        % debugger::trace,
        htmlize(Start, End, Body),
        HTML = element(html, [], [
            element(body, [], Body )
        ]).

    htmlize(N, End, [H | Prev]) :-
        N =< End,
        % (N=1030 -> debugger::trace ; true ),
        E = element(N, _, text, _, _),
        ::element(E), !,
        htmlize(E, H),
        ( ::neighborN(N, Q) -> htmlize(Q, End, Prev) ;
          Prev = [] ).

    htmlize([], []) :- !.
    htmlize(S, T) :-
        string(S), !,
        ::text_adjust(S, T), !.
    htmlize(A, T) :-
        atom(A),
        atom_string(A, S), !,
        ::text_adjust(S, T), !.
    htmlize([X, Y|T], HT) :-
        aors(X),
        aors(Y), !,
        htmlize(X, HX),
        htmlize(Y, HY),
        string_concat(HX, HY, H),
        htmlize([H|T], HT).
    % Here are special cases.
    htmlize(element(_, _, text, Attrs, S), H) :-
        aors(S), !,
        htmlize(element(_, _, text, Attrs, [S]), H).
%    htmlize(element(_, _, text, Attrs, S), element(span, [], L)) :- !,
    htmlize(element(_, _, text, Attrs, S), element(Tag, RAttrs, L)) :- !,
        convert_attrs(Attrs, FAttrs),
        option_tag(FAttrs, Tag, RAttrs), !,
        htmlize(S, L).

    htmlize([X|T], [HX | HT]) :-
        htmlize(X, HX),
        htmlize(T, HT).

    convert_attrs([], []).
    convert_attrs([A=B | T], [A=B1 | CT]) :-!,
        textify(B, B1),
        convert_attrs(T, CT).
    convert_attrs([F | T], [A=B1|CT]) :-
        F =.. [A,B], !,
        textify(B, B1),
        convert_attrs(T, CT).

    textify(B, B) :- aors(B), !.
    textify(B, B1) :- format(atom(B1), "~w", [B]).

    aors(A) :-
        atom(A).
    aors(A) :-
        string(A).

    option_tag(Attrs, Tag, R) :-
        ::text_name(Tag, A),
        F =.. [A, true],
        select_option(F, Attrs, R), !.
    option_tag(A, p, A).

:- end_category.
