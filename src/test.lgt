
:- object(test).

   :- info([
      version is 1:0:0,
      author is 'Evgeny Cherkashin',
      date is 2022-09-14,
      comment is 'Test and run predicates'
   ]).

   :- public(run/0).
   :- info(run/0, [
      comment is 'Issues and informative message'
   ]).

   run:-format("Hello world\n").

   :- public(xml_test/0).
   :- info(xml_test/0, [
      comment is 'Just loads the xml and pprints it'
   ]).

   xml_test :- xml_loader("/home/eugeneai/projects/code/rpd-processor/data/xml/B1.O.30_Sistemy_iskusstvennogo_intellekta_4003.pdf.xml")::pprint.




:- end_object.
