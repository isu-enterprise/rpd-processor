:- object(xml_loader(_Filename_)).
	:- use_module(library(sgml), [load_xml/3]).
	:- meta_predicate(sgml:load_xml(*,*,*)).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin',
		date is 2022-09-14,
		comment is 'Loads XML upon object derivative definition.'
	]).

	:- public(load/1).
	:- info(load/1, [
		comment is 'Loads XML file. Argument is its structure'
	]).

	load(Term) :-
		load_xml(_Filename_, Term, [dtd(_)]).

	:- public(pprint/1).
	:- info(pprint/1, [
		comment is 'Pretty prints the argument'
	]).

	pprint(Term) :-
		print(Term).
	
	:- public(pprint/0).
	:- info(pprint/0, [
		comment is 'Loads and pprint the XML. Used for testing.'
	]).

	pprint :-
		load(Term), pprint(Term).



:- end_object.
