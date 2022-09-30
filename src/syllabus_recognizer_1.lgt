:- object(syllabus_recognizer(_XML_, _HTML_File_Name_, _Document_IRI_),
          extends(as_db(_XML_)),
          imports([syllabus_fonts,
                   text_attrib,
                   degraded,
                   text_features,
                   % text_merge,
                   syllabus_merge,
                   text_syllabus_sections,
                   gather_items,
                   syllabus_page_one,
                   text_syllabus_fields,
                   grouping,
                   htmlize])).

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
   deviation(paragraph, [50, 10]).
   deviation(parindent, [28]).         % 1 cm = 28 pt
   deviation(itemtextminlength, [10]). % The length of a "minimal item text" in characters.

   :- public(process/0).
   :- info(process/0, [
       comment is 'Run all rules in an order'
   ]).
   process :-
      msg("Processing font definitions"),
      ::process_syllabus_fonts, !,
      msg("Processing attrs"),
      ::process_attrs, !,
      msg("Remove degraded artifacts"),
      ::process_degraded, !,
      msg("Merging runs in lines, Pass 0"),
      ::process_runs_merge, !,
      msg("Gathering features"),
      ::process_features, !,
      msg("Merging runs and lines into paragraphs, Pass 1"),
      ::process_merge, !,
      msg("Gathering features, Pass 2"),
      ::process_features, !,
      msg("Merging runs and lines into paragraphs, Pass 2"),
      ::process_merge, !,
      msg("Processing the titlepage"),
      ::process_first_page, !,
      msg("Findig sections"),
      ::process_syllabus_sections, !,
      msg("Gathering item sets"),
      ::process_item_gathering, !,
      msg("Merging runs and lines into paragraphs, Pass 3"),
      ::process_merge, !,
      msg("Findig fields"),
      ::process_syllabus_fields, !,
      msg("Grouping ul items"),
      ::process_grouping(ul), !,
      msg("Merging items into paragraphs, Pass 4"),
      ::process_merge, !,
      % msg("Grouping ol items"),
      % ::process_grouping(ol), !,
      % msg("Merging items into paragraphs, Pass 5"),
      % ::process_merge, !,
      msg("Write HTML of the result"),
      ::htmlize(_HTML_File_Name_, _Document_IRI_), !,
      msg("Finished"),
      true.
   msg(S) :-
      format("% ~w\n", [S]).

:- end_object.
