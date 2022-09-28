:- category(htmlize).

    :- info([
        version is 1:0:0,
        author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
        date is 2022-09-26,
        comment is 'Description'
    ]).

	:- use_module(lists, [member/2, flatten/2, append/3]).
	:- use_module(library(sgml_write), [html_write/3]).
    :- use_module(library(option), [select_option/3, select_option/4,
                                    option/3, option/2]).
    :- use_module(library(pcre), [re_match/2, re_match/3,
                                  re_matchsub/4, re_split/4]).

    :- public(htmlize/2).
    % :- mode(htmlize, Solutions).
    :- info(htmlize/2, [
        comment is 'Convert context to html and denote it as Document IRI.'
    ]).

    htmlize(FileName, Document) :-
        string(FileName),
        htmlize(HTML, Document),
        open(FileName, write, Stream, []),
        html_write(Stream, HTML, []),
        close(Stream).
    htmlize(HTML, Document) :-
        var(HTML),
        ::range(text, Start, End),
        BE = element(body, [property='oa:hasTarget', resource=ContentId, typeof='foaf:Content'], [] ),
        htmlize0(Start, End, Content, BE, BO),
        BO = element(body, BAttrs, _ ),
        flatten(Content, Body),
        format(atom(ContentId), "~w-content", [Document]),
        HTML = element(html, [resource=Document, typeof='foaf:Document'], [
            element(body, BAttrs, Body)
        ]).

    htmlize0(N, End, [RH | Rest], C, C) :-
        N =< End,
        E = element(N, _, text, _, _),
        ::element(E), !,
        htmlize0(E, H),
        refine_node(H,RH),
        ( ::neighborN(N, Q) -> htmlize0(Q, End, Rest, C, _) ;
          Rest = [] ).

    htmlize0([], []) :- !.
    htmlize0(S, T) :-
        string(S), !,
        ::text_adjust(S, T), !.
    htmlize0(A, T) :-
        atom(A),
        atom_string(A, S), !,
        ::text_adjust(S, T), !.
    htmlize0([X, Y|T], HT) :-
        aors(X),
        aors(Y), !,
        htmlize0(X, HX),
        htmlize0(Y, HY),
        string_concat(HX, HY, H),
        htmlize0([H|T], HT).
    % Here are special cases.
    htmlize0(element(_, _, text, Attrs, S), H) :-
        aors(S), !,
        htmlize0(element(_, _, text, Attrs, [S]), H).
    htmlize0(element(_, _, text, Attrs, S), element(Tag, FAttrs, L)) :- !,
        option_tag(Attrs, Tag, RAttrs), !,
        convert_attrs(RAttrs, FAttrs),
        htmlize0(S, L).
    htmlize0([X|T], [HX | HT]) :-
        htmlize0(X, HX),
        htmlize0(T, HT).

    convert_attrs([], []).
    convert_attrs([A=B | T], [A=B1 | CT]) :-
        ::allowed_attr(A), !,
        textify(B, B1),
        convert_attrs(T, CT).
    convert_attrs([F | T], [A=B1|CT]) :-
        F =.. [A,B],
        ::allowed_attr(A), !,
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
    option_tag(Attrs, Tag, R) :-
        ::attr_tag(_Tag1, F, V),
        select_option(F, Attrs, R),
        V \= "", !,
        ::attr_tag(Tag, F, V).

    option_tag(A, p, A).

    :- protected(attr_tag/3).
    % :- mode(attr_tag, Solutions).
    :- info(attr_tag/3, [
        comment is 'Selects a tag for HTML representation based on Attrs content'
    ]).

    attr_tag(h, section(Id), Id) :-
        var(Id).
    attr_tag(p, itemName(V), V) :-
        var(V).
    attr_tag(li, item(V), V).
    attr_tag(li, ding(V), V).
    attr_tag(HLevel, section(Id), Id) :-
        nonvar(Id), var(HLevel), !,
        calc_section_level(Id, Num),
        format(atom(HLevel), "h~w", [Num]).
    attr_tag(p, itemName(V), V) :-
        nonvar(V), !,
        V \= "".

    calc_section_level(nosection, 0) :-!, fail.
    calc_section_level(none, 0) :- !.
    calc_section_level(Id, N) :-
        ::number_section(_, Id, Parent, _), !,
        calc_section_level(Parent, M),
        N is M + 1.
    calc_section_level(Id, N) :-
        ::unnumbered_section(Id, Parent, _), !,
        calc_section_level(Parent, M),
        N is M + 1.
    calc_section_level(Id, _) :-
        format("% ERROR: No section info for section '~w'\n", [Id]),
        fail.

    :- protected(allowed_attr/1).
    :- info(allowed_attr/1, [
        comment is 'Is this atter OK to use in HTML for any purpose?'
    ]).

    allowed_attr(X) :-
        html_attr(X).
    allowed_attr(X) :-
        rdfa_attr(X).

    :- protected(html_attr/1).
    :- info(html_attr/1, [
        comment is 'Is this atter OK to use in HTML for any purpose?'
    ]).

    html_attr(X) :-
        member(X, [href, value, name, id, class, style]).

    :- protected(rdfa_attr/1).
    :- info(rdfa_attr/1, [
        comment is 'Defines a set of RDFa attributes allowed in HTML'
    ]).

    rdfa_attr(X) :-
        member(X, [resource, about, property, vocab,
                   typeof, refix, datatype, rel, rev, itemset]).

    :- protected(refine_node/2).
    % :- mode(refine_node, Solutions).
    :- info(refine_node/2, [
        comment is 'Refines HTML node to be simple'
    ]).

    refine_node(element(Tag, Attrs, Nodes),
                element(Tag, Attrs, N4)) :-
        refine_nodes(Tag, Nodes, NNodes),
        eliminate_span(NNodes, N3),
        remove_duplicate_nodes(N3, N4).

    refine_nodes(_, [], []) :- !.
    refine_nodes(_, S, S) :-
        aors(S), !.
    refine_nodes(Tag, element(Tag, Attrs, Nodes), element(span, Attrs, RNodes)) :- !,
        refine_nodes(Tag, Nodes, RNodes).
    refine_nodes(li, element(p, Attrs, Nodes), element(span, Attrs, RNodes)) :- !,
        refine_nodes(li, Nodes, RNodes).
    refine_nodes(H, element(_, Attrs, Nodes), element(span, Attrs, RNodes)) :-
        re_match("^[hH][1-9]$", H, []), !,
        refine_nodes(H, Nodes, RNodes).
    refine_nodes(Tag, element(Tag1, Attrs, Nodes), element(Tag1,Attrs, RNodes)) :- !,
        Tag \= Tag1,
        refine_nodes(Tag1, Nodes, RNodes).
    refine_nodes(Tag, [X|T], [RX|RT]) :-
        refine_nodes(Tag, X, RX),
        refine_nodes(Tag, T, RT).

    eliminate_span([], []) :-!.
    eliminate_span(element(span, _, L), FL) :- !,
        eliminate_span(L, EL),
        flatten(EL, FL).
    eliminate_span(element(Tag, Attrs, L), element(Tag, Attrs, FL)) :- !,
        eliminate_span(L, EL),
        flatten(EL, FL).
    eliminate_span([X|T], R) :- !,
        eliminate_span(X, EX),
        eliminate_span(T, ET),
        flatten([EX| ET], R).
    eliminate_span(X,X).

    remove_duplicate_nodes([], []) :- !.
    remove_duplicate_nodes([X, Y | T], R) :-
        X = element(Tag, Attrs1, L1),
        Y = element(Tag, Attrs2, L2),
        attrs_join(Attrs1, Attrs2, Attrs), !,
        append(L1, L2, L),
        remove_duplicate_nodes([element(Tag, Attrs, L)|T], R).
    remove_duplicate_nodes([X | T], [X | R]) :-
        remove_duplicate_nodes(T, R).

    :- protected(attrs_join/3).
    % :- mode(attrs_join, Solutions).
    :- info(attrs_join/3, [
        comment is 'Try to join attributes non-crontradictory'
    ]).

    attrs_join(A1, A2, A) :-
        append(A1, A2, A).

:- end_category.
