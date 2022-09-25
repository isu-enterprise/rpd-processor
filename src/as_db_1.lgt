:- object(as_db(_XML_)).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin',
		date is 2022-09-14,
		comment is 'Represents an XML document as a set of lines'
	]).

    :- use_module(lists, [member/2, append/3]).
    :- use_module(library(option), [select_option/3, select_option/4,
                                    option/3]).

	:- public(convert/0).
	:- info(convert/0, [
		comment is 'Converts _XML_ to a set of elements'
	]).

	convert :-
		_XML_::load(Term),
		convert(Term).

	:- public(convert/1).
	:- info(convert/1, [
		comment is 'Converts an XML term to set of elements'
	]).

	convert(Term) :-
		convert(Term, 1, L, none),
        Last = L - 1,
		asserta(range(1, Last)),
        count_neighbors(1, Last, []).

	:- public(convert/4).
	:- info(convert/4, [
		comment is 'Converts XML term to a set of elements, supplying ids.'
	]).

	convert([], N, N, _).

	convert([X|T], N, N2, Parent):-
		convert(X, N, N1, Parent),
		convert(T, N1, N2, Parent).

	convert(element(Name, Attrs, Terms), N, N2, Parent) :-
        convertattrs(Attrs, NAttrs),
        % format("CNV: <~w ~w>\n", [Name, NAttrs]),
%        debugger::trace,
		convert1(element(Name, NAttrs, Terms), Parent, N, RestTerms),
		N1 is N + 1,
		convert(RestTerms, N1, N2, N).

	convert1(element(text, Attrs, List), Parent, N, []) :- !,
        convert_el_list(List, CList),
		assertz(element(N, Parent, text, Attrs, CList)).

			% Defaults to just assert Name(N, Attrs, Parent)
	convert1(element(Name, Attrs, Terms), Parent, N, Terms) :-
%        debugger::trace,
		assertz(element(N, Parent, Name, Attrs, '')).

    convert_el_list([], []).
    convert_el_list([ element(Tag, Attrs, [Atom]) | T],
                    [ element(0, 0, text, [ Name = true | CAttrs ], Text) | CT ]) :- !,
        convertattrs(Attrs, CAttrs),
        text_name(Tag, Name),
        atom_string(Atom, Text),
        convert_el_list(T, CT).

    convert_el_list([Atom | T], [Text | CT]):-
        atom_string(Atom, Text),
        convert_el_list(T, CT).

    convertattrs([], []).
    convertattrs([K=V|T], [K=NV|T1]) :-
        convertattrs(T, T1),
        convertv(V, NV).

    convertv(V, V1) :-
        atom_string(V, S),
        number_string(V1, S), !.
    convertv(V,V).

	text_name(b,bold).
	text_name(text,text).
	text_name(i,italic).
	text_name(a,anchor).

    count_neighbors(N, Last, L) :-
        N =< Last, !,
        element(N, _, T, _, _),
        FP =.. [T, Prev],
        ::next(N, N1),
        FN =.. [T, N],
        ( select_option(FP, L, R) ->
           assertz(neighborN(Prev,N)) ; R = L ),
        count_neighbors(N1, Last, [FN | R]).

    count_neighbors(_, _, _).




    % count_neighbors(text, Par) :-
    %    P is Par + 1,
    %    gen(P, N),
    %    element(N, Par, text, A, S), !,  % The first one
    %    count_neighbors(element(N, Par, text, A, S)), !.

    % count_neighbors(page) :-
    %    gen(N),
    %    element(N, Par, page, A, S), !,  % The first one
    %    count_neighbors(element(N, Par, page, A, S)), !.

    % count_neighbors(element(N1, P, Tag, A1, S1)):-
    %    find_neighbor(element(N1, P, Tag, A1, S1), element(N2, P, Tag, A2, S2)), !,
    %    assertz(neighborN(N1,N2)), !,
    %    ( Tag = page ->
    %      % format("CN: ~w\n",[element(N1, P, Tag, A1, S1)]),
    %      % (N1=53 -> debugger::trace; true),
    %      count_neighbors(text, N1); true ),
    %    count_neighbors(element(N2, P, Tag, A2, S2)).

    % count_neighbors(element(N1, _, Tag, _, _)):-
    %    ( Tag = page -> count_neighbors(text, N1); true ).
    %    % ( Tag = page ->
    %    %   format("CN: ~w\n",[element(N1, P, Tag, A1, S1)]),
    %    %   count_neighbors(text, N1); true ).


	:- protected(element/5).
	:- info(element/5, [
		comment is 'Dynamic predicate represents an element of a PDF structure.'
	]).
    :- dynamic(element/5).


	:- protected(neighborN/2).
    :- info(neighborN/2, [
		comment is 'Dynamic prdicate defines that two element of the same kind are neighbors'
	]).
	:- dynamic(neighborN/2).


	:- protected(range/2).
    :- info(range/2, [
		comment is 'Dynamic predicate defines the range of id variation for the elements'
	]).
	:- dynamic(range/2).


	:- public(print/0).
	:- info(print/0, [
		comment is 'Print list of tags, used in debugging'
	]).

	printn(Tag, N) :-
		element(N, Parent, Tag, Attrs, String), !,
		format("\n~w-~w ~w ~w\n     '~w'\n", [N, Parent, Tag, Attrs, String]).
	printn(_, _).

	print :-
		range(Start, End),
		forall(gen(Start, End, N), printn(_, N)).

    :- public(gen/3).
    :- info(gen/3, [
       comment is 'Generates number from Start to End into N'
    ]).

	gen(Start, End, Start) :- Start =< End.
	gen(Start, End, N) :-
		Start =< End,
		Start1 is Start + 1,
		gen(Start1, End, N).

    :- public(ngen/3).
    :- info(ngen/3, [
       comment is 'Generates number from End downto Start into N'
    ]).

	ngen(Start, End, End) :- Start =< End.
	ngen(Start, End, N) :-
		Start =< End,
		End1 is End - 1,
		ngen(Start, End1, N).

    :- public(gen/2).
    :- info(gen/2, [
       comment is 'Generates number into N for all elements in order starting with Start'
    ]).

    gen(Start, N) :-
       range(_, End), !,
       gen(Start, End, N).

    :- public(gen/1).
    :- info(gen/1, [
       comment is 'Generates number into N for all elements in order'
    ]).

    gen(N) :-
       range(Start, End), !,
       gen(Start, End, N).

    :- public(ngen/2).
    :- info(ngen/2, [
       comment is 'Generates number into N for all elements in the negative order starting with End'
    ]).

    ngen(End, N) :-
       range(Start, _), !,
       ngen(Start, End, N).

    :- public(neighbor/2).
    :- info(neighbor/2, [
       comment is 'Two lines are neighbors and have same tag = text, page'
    ]).

    neighbor(element(N1, P, Tag, _, _),element(N2, P, Tag, A2, S2)):-
       neighborN(N1, N2),
       element(N2, P, Tag, A2, S2).

    :- protected(neighbor_num/2).
    :- info(neighbor_num/2, [
       comment is 'Neigbors defined as line numbers'
    ]).

    neighbor_num(A, B) :- neighborN(A,B).


    find_neighbor(element(N1, Par, Tag, _, _), element(N2, Par, Tag, Attrs2, S2)) :-
       range(_, End), N1 < End, !,
       Start is N1 + 1,
       gen(Start, End, N2),
       element(N2, Par, Tag, Attrs2, S2), !.

	:- public(print/1).
	:- info(print/1, [
		comment is 'Print all of tags, supplied as argument used in debugging'
	]).

	print(Tag) :-
		forall(gen(N), printn(Tag, N)).

	:- protected(replace/2).
	:- info(replace/2, [
		comment is 'Replaces a fact in object database'
	]).

	replace(A,B) :-
        % A = element(N, P, Tag, A, S),
        % B = element(N, P, Tag, A, S)
        % A = element(N, P, Na, Aa, S)
        % format("RETRACT ALL:~w-~w-~w ~w '~w'\n", [N, P, Na, Aa, S])),
		retract(A),
		asserta(B).   % Assuming all conversions are made from up to down

    :- public(remove/1).
    :- info(remove/1, [
       comment is 'Removes a database element.'
    ]).

    remove(element(N, P, Tag, A, S)) :-
        retract(element(N, P, Tag, A, S)),
        (
           neighborN(N0,N),
           neighborN(N ,N1) ->
           retract(neighborN(N0, N)),
           retract(neighborN(N, N1)),
           asserta(neighborN(N0, N1)) ;   % Element was located in a middle.
           (
             neighborN(N0, N) ->
             retract(neighborN(N0, N)) ;  % It was the last one
              (
                neighborN(N,N1) ->
                retract(neighborN(N, N1)); % The first one
                true                      % The only element
              )
           )
        )
        .

    :- public(prepend/1).
    :- info(prepend/1, [
        comment is 'AssertA a database element.'
    ]).

    prepend(A) :-
        asserta(A).

    :- public(append/1).
    :- info(append/1, [
        comment is 'AssertZ a database element.'
    ]).

    append(A) :-
        assertz(A).

    :- public(gettext/2).
    :- info(gettext/2, [
        comment is 'Joins all components of list or string in a string'
    ]).
    % TODO: Consider element/5. Else it faile.
    gettext([], "") :- !.
    gettext(element(_,_,text,_,S), T) :- !,
        gettext(S, T).
    gettext(A, S) :- atom(A), atom_string(A,S), !.
    gettext(S, S) :- string(S), !.
    gettext([X|T], S) :- !,
        gettext(X, XS),
        gettext(T, TS),
        string_concat(XS, TS, S).

    :- public(lstrip/3).
    :- info(lstrip/3, [
    comment is 'Eliminate spaces at the start of a string'
    ]).

    lstrip(S, SSS, Ch) :-
       sub_string(S,0,1,After,Ch),
       sub_string(S,1,After,0,SS), !,
       lstrip(SS,SSS,Ch).
    lstrip(S,S, _).

    :- public(print_as_text/1).
    :- info(print_as_text/1, [
        comment is 'Prints database as text with some marks'
    ]).

    print_as_text(Tag) :-
		forall(gen(N), print_as_text(Tag, N)).

    print_as_text(Tag, _N) :-
        element(_N, _P, Tag, _Attrs, S), !,
        % (N = 26 -> debugger::trace; true),
        gettext(S, Text),
        % Text = S,
        % format("~w-~w ~w ~w\n", [_N, _P, Text, _Attrs]).
        format("~w|\n", [Text]).

    print_as_text(_, _).

    :- protected(next/2).
    :- info(next/2, [
        comment is 'Get next number, possibly with neighbor stuff'
    ]).

    next(N, N1) :-
        ::neighbor_num(N, N1), !.
    next(N, N1) :-
        var(N1), !,
        N1 is N + 1.
    next(N, N1) :-
        var(N), !,
        N is N1 - 1.

    :- protected(prev/2).
    :- info(prev/2, [
        comment is 'Get previous number, pussibly with neighborhood'
    ]).

    prev(N, N0) :- next(N0, N).

    :- protected(check_hints/3).
    :- info(check_hints/3, [
        comment is 'Check wether a word list contained in a string. Returns the resto of the string.'
    ]).

    check_hints(S, [], S).
    check_hints(Text, [H|T], Rest) :-
        sub_string(Text, _, _, After, H), !,
        sub_string(Text, _, After, 0, SubText), !,
        check_hints(SubText, T, Rest).

:- end_object.
