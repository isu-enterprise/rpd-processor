
:- object(text_attrib(_XML_),
	extends(as_db(_XML_))).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin',
		date is 2022-09-16,
		comment is 'Adds addidional attributes to text runs, e.g., centered'
	]).

	:- use_module(lists, [member/2]).
	:- use_module(library(option), 
		[option/2, option/3]).

	:- public(centered/2).
	:- info(centered/2, [
		comment is 'Adds a test attribute centered, if it is within its page.'
	]).

	centered(element(N, Parent, text, Attrs, String), element(N, Parent, text, [centered=true|Attrs], String)):-
		element(Parent, _, page, PAttrs, _),
		at_center(Attrs, PAttrs).
	
	at_center(As, PAs) :-
		option(left(ALeft), As),
		% option(left(PLeft), PAs),
		option(width(AWidth), As),
		option(width(PWidth), PAs),
		deviation(_, DC),!,
		PCenter is div(PWidth, 2), 
		ACenter is div(AWidth, 2) + ALeft,
		DCenter is abs(PCenter - ACenter), !,
		DCenter < DC.

	:- protected(deviation/2).
	:- info(deviation/2, [
		comment is 'Deviation parameter.'
	]).

	% +- deviation(width, center) center assuming stupid indent and shift on the page when printing.

	deviation(10, 50).

	:- public(process/0).
	:- info(process/0, [
		comment is 'Process all rules'
	]).

	process:-
		forall(::element(N,P,T,A,S), rule(element(N,P,T,A,S))).

	:- protected(rule/1).
	:- info(rule/1, [
		comment is 'Applies a processing rule'
	]).

	rule(El) :-
		centered(El, NEl), !,
		replace(El, NEl).

	rule(_).

	:- protected(replace/2).
	:- info(replace/2, [
		comment is 'Replaces a fact in database'
	]).

	replace(A,B) :-
		retractall(A),
		asserta(B).
		

:- end_object.
