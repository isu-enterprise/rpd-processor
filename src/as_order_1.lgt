:- object(as_order(_XML_)).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin',
		date is 2022-09-14,
		comment is 'Represents an XML document as a set of lines'
	]).

	:- public(convert/1).
	:- info(convert/1, [
		comment is 'Converts an XML term to set of elements'
	]).

	convert(Term) :- 
		convert(Term, 1, Last, none),
		assert(range(1,Last)).	

	:- public(convert/4).
	:- info(convert/4, [
		comment is 'Converts XML term to a set of elements, supplying ids.'
	]).

	convert([], N, N, _).

	convert([X|T], N, N2, Parent):-
		convert(X, N, N1, Parent),
		convert(T, N1, N2, Parent).

	convert(element(Name, Attrs, Terms), N, N2, Parent) :-
		convert(Name, Attrs, Terms, Parent, N),
		N1 is N + 1,
		convert(Terms, N1, N2, N).

	convert(text, Attrs, [String], Parent, N) :-
		asserta(text(N, Parent, Attrs, String)).
	convert(b, Attrs, [String], Parent, N) :-
		asserta(bold(N, Parent, Attrs, String)).
	convert(i, Attrs, [String], Parent, N) :-
		asserta(italic(N, Parent, Attrs, String)).
		
	% Defaults to just assert Name(N, Attrs, Parent)
	convert(Name, Attrs, Terms, Parent, N) :-
		Atom =.. [Name, N, Parent, Attrs],
		asserta(Atom).

	:- public(print/0).
	:- info(print/0, [
		comment is 'Print list of tags, used in debugging'
	]).

	:- dynamic(range/2).
	:- dynamic(text/4).
	:- dynamic(bold/4).
	:- dynamic(italic/4).


	print :-
		for
		
		
:- end_object.
