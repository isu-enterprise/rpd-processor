
:- object(test).

   :- info([
      version is 1:0:0,
      author is 'Evgeny Cherkashin',
      date is 2022-09-14,
      comment is 'Test and run predicates'
   ]).

   :- protected(xml_file_name/0).
   :- info(xml_file_name/0, [
      comment is 'Defines file name for esting'
   ]).

   xml_file_name("/home/eugeneai/projects/code/rpd-processor/data/xml/B1.O.30_Sistemy_iskusstvennogo_intellekta_4003.pdf.xml").

   :- public(run/0).
   :- info(run/0, [
      comment is 'Issues and informative message'
   ]).

   run:-format("Hello world\n").

   :- public(xml_test/0).
   :- info(xml_test/0, [
      comment is 'Just loads the xml and pprints it'
   ]).

   xml_test :-
      xml_file_name(Name),
      O = xml_loader(Name),
      O::pprint.

   :- public(db_test/0).
   :- info(db_test/0, [
      comment is 'Testing conversion to datbase.'
   ]).

   db_test :-
      xml_file_name(Name),
      XML = xml_loader(Name),
      O = as_db(XML),
      O::convert,
      O::print.

   :- public(attrib_test/0).
   :- info(attrib_test/0, [
      comment is 'Test of additional attribute setting.'
   ]).

   attrib_test :-
      xml_file_name(Name),
      XML = xml_loader(Name),
      O = text_attrib(XML),
      O::convert,
      O::process,
      O::print(_),
      true.

:- end_object.
