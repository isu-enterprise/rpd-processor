
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

   :- public(run/0).
   :- info(run/0, [
      comment is 'Issues and informative message'
   ]).

   run:-format("Syllabus PDF->RDFa converter\n").

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
      O::print,
      true.

   :- public(attrib_test/0).
   :- info(attrib_test/0, [
      comment is 'Test of additional attribute setting.'
   ]).

   attrib_test :-
      xml_file_name(Name),
      html_file_name(HTMLFN),
      XML = xml_loader(Name),
      O = syllabus_recognizer(XML, HTMLFN, 'http://irnok.net/syllabus/database/syllabus-1'),
      O::convert,
      O::process,
      O::print(_), nl,nl,
      % O::print_as_text(_),
      true.

   % xml_file_name("/home/eugeneai/projects/code/rpd-processor/data/xml/B1.O.30_Sistemy_iskusstvennogo_intellekta_4003.pdf.xml").
   % xml_file_name("/home/eugeneai/projects/code/rpd-processor/data/xml/2018B1.V.OD.4Rekursivno-logicheskoeprogrammirovanie_2766.pdf.xml").
   % xml_file_name("/home/eugeneai/projects/code/rpd-processor/data/xml/B1.V.06_Funktsionalnoe_programmirovanie_4001.pdf.xml").
   % xml_file_name("/home/eugeneai/projects/code/rpd-processor/data/xml/B1.O.20_Bazy_dannykh_4003.pdf.xml").
   xml_file_name("/home/eugeneai/projects/code/rpd-processor/data/xml/B1.V.10_Parallelnoe_programmirovanie_4001.pdf.xml").

   % xml_file_name("/home/eugeneai/projects/code/rpd-processor/data/xml/simplest.xml").
   % xml_file_name("/home/eugeneai/projects/code/rpd-processor/data/xml/simple.xml").
   % xml_file_name("/home/eugeneai/projects/code/rpd-processor/data/xml/russian.xml").
   % xml_file_name("/home/eugeneai/projects/code/rpd-processor/data/xml/ex1.xml").
   % xml_file_name("/home/eugeneai/projects/code/rpd-processor/data/xml/ex2.xml").
   % xml_file_name("/home/eugeneai/projects/code/rpd-processor/data/xml/ex3.xml").

   html_file_name("/home/eugeneai/projects/code/rpd-processor/data/result-db.html").

:- end_object.
