:- object(as_db(_XML_)).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin',
		date is 2022-09-14,
		comment is 'Represents an XML document as a set of lines'
	]).

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
		convert(Term, 1, Last, none),
		asserta(range(1,Last)).

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
		convert1(element(Name, NAttrs, Terms), Parent, N, RestTerms),
		N1 is N + 1,
		convert(RestTerms, N1, N2, N).

	convert1(element(text, Attrs, [element(Tag, [], [String])]), Parent, N, []) :- !,
		text_name(Tag, ExtTag),
		assertz(element(N, Parent, text, [ExtTag=true|Attrs], String)).

	convert1(element(Name, Attrs, [String]), Parent, N, []) :-
		text_name(Name,ExtName), !,
		assertz(element(N, Parent, ExtName, Attrs, String)).

			% Defaults to just assert Name(N, Attrs, Parent)
	convert1(element(Name, Attrs, Terms), Parent, N, Terms) :-
		assertz(element(N, Parent, Name, Attrs, '')).

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
	text_name(i,itelic).

	:- public(element/5).
	:- protected(range/2).

	:- dynamic(element/5).
	:- dynamic(range/2).

	:- public(print/0).
	:- info(print/0, [
		comment is 'Print list of tags, used in debugging'
	]).

	print(N) :-
		::el(N, Parent, Name, Attrs, String), !,
		format("~w-~w-~w ~w '~w'\n", [N, Parent, Name, Attrs, String]).
	print(_).

	print :-
		range(Start, End),
		forall(gen(Start, End, N), print(N)).

	gen(Start, End, Start) :- Start =< End.
	gen(Start, End, N) :-
		Start =< End,
		Start1 is Start + 1,
		gen(Start1, End, N).

    :- public(el/5).
    :- info(el/5, [
        comment is 'Returns element contents. Stub'
    ]).

    el(N, P, Na, A, S) :-
          element(N, P, Na, A, S).



:- end_object.
