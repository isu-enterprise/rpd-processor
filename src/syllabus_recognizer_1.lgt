:- object(syllabus_recognizer(_XML_),
   extends(as_db(_XML_)),
   imports([text_attrib,
            text_features,
            text_merge,
            syllabus_page_one,
            text_syllabus_sections])).

   :- info([
      version is 1:0:0,
      author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
      date is 2022-09-17,
      comment is 'Complex of recognizing procedures run on a page and line sets'
   ]).

   :- use_module(lists, [member/2]).
   :- use_module(library(option), [option/2, option/3]).

   :- protected(deviation/2).
   :- info(deviation/2, [
      comment is 'Local definition of deviation coefficients'
   ]).

   deviation(attributes, [10, 50]).
   deviation(paragraph, [3, 10]).
   deviation(parindent, [28]).  % 1 cm = 28 pt
   deviation(itemtextminlength, [10]). % The length of a "minimal item text" in characters.

   :- public(process/0).
   :- info(process/0, [
       comment is 'Run all rules in an order'
   ]).

   process :-
      msg("Processing attrs"),
      ::process_attrs, !,
      msg("Gathering features"),
      ::process_features, !,
      msg("Merging lines into paragraphs"),
      ::process_merge, !,
      msg("Processing the titlepage"),
      ::process_first_page, !,
      msg("Findig sections"),
      ::process_syllabus_sections, !,
      msg("Finished"),
      true.

   msg(S) :-
      format("% ~w\n", [S]).


:- end_object.