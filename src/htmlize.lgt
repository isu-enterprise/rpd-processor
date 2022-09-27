:- category(htmlize).

    :- info([
        version is 1:0:0,
        author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
        date is 2022-09-26,
        comment is 'Description'
    ]).

	:- use_module(lists, [member/2]).
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

    htmlize(N, End, [RH | Prev]) :-
        N =< End,
        % (N=1030 -> debugger::trace ; true ),
        E = element(N, _, text, _, _),
        ::element(E), !,
        htmlize(E, H),
        refine_node(H,RH),
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
    htmlize(element(_, _, text, Attrs, S), element(Tag, FAttrs, L)) :- !,
        option_tag(Attrs, Tag, RAttrs), !,
        convert_attrs(RAttrs, FAttrs),
        htmlize(S, L).

    htmlize([X|T], [HX | HT]) :-
        htmlize(X, HX),
        htmlize(T, HT).

    convert_attrs([], []).
    convert_attrs([A=B | T], [A=B1 | CT]) :-
        html_attr(A), !,
        textify(B, B1),
        convert_attrs(T, CT).
    convert_attrs([F | T], [A=B1|CT]) :-
        F =.. [A,B],
        html_attr(A),
        !,
        textify(B, B1),
        convert_attrs(T, CT).
    convert_attrs([_|T], CT) :-
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

    :- protected(html_attr/1).
    % :- mode(html_attr, Solutions).
    :- info(html_attr/1, [
        comment is 'Is this atter OK to use in HTML for any purpose?'
    ]).

    html_attr(X) :-
        member(X, [href, value, name, id, class, style]).

    :- protected(refine_node/2).
    % :- mode(refine_node, Solutions).
    :- info(refine_node/2, [
        comment is 'Refines HTML node to be simple'
    ]).

    refine_node(element(Tag, Attrs, Nodes),
                element(Tag, Attrs, NNodes)) :-
        refine_nodes(Tag, Nodes, NNodes).

    refine_nodes(_, [], []) :- !.
    refine_nodes(_, S, S) :-
        aors(S), !.
    refine_nodes(Tag, element(Tag, Attrs, Nodes), element(span,Attrs, RNodes)) :- !,
        refine_nodes(Tag, Nodes, RNodes).
    refine_nodes(Tag, element(Tag1, Attrs, Nodes), element(Tag1,Attrs, RNodes)) :- !,
        Tag \= Tag1,
        refine_nodes(Tag1, Nodes, RNodes).
    refine_nodes(Tag, [X|T], [RX|RT]) :-
        refine_nodes(Tag, X, RX),
        refine_nodes(Tag, T, RT).


:- end_category.
