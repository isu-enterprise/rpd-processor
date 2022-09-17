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

	convert1(element(text, Attrs, [element(Tag, [], [Atom])]), Parent, N, []) :- !,
		text_name(Tag, ExtTag), !,
        atom_string(Atom, String), !,
		assertz(element(N, Parent, text, [ExtTag=true|Attrs], String)).

	convert1(element(Name, Attrs, [Atom]), Parent, N, []) :-
		text_name(Name,ExtName), !,
        atom_string(Atom, String), !,
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

	printn(Tag, N) :-
		element(N, Parent, Tag, Attrs, String), !,
		format("~w-~w-~w ~w '~w'\n", [N, Parent, Tag, Attrs, String]).
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

    :- public(gen/1).
    :- info(gen/1, [
       comment is 'Generates number into N for all elements in order'
    ]).

    gen(N) :-
       range(Start, End), !,
       gen(Start, End, N).

    :- public(neighbor/2).
    :- info(neighbor/2, [
       comment is 'Two lines are neighbors and have same tag = text, page'
    ]).

    neighbor(element(N1, Par, Tag, Attrs1, S1), element(N2, Par, Tag, Attrs2, S2)) :-
       range(_, End), N1 < End, !,
       gen(N1, End, N2),
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
        % A = element(N, P, Na, Aa, S)
        % format("RETRACT ALL:~w-~w-~w ~w '~w'\n", [N, P, Na, Aa, S])),
		retract(A),
		asserta(B).   % Assuming all conversions are made from up to down

:- end_object.
