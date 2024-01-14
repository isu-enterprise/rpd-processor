:- object(zip(_FileName_),
   imports(attributes)).
   % :- initialization(())
   :- use_module(library(zip),
      [ zip_open/4
      , zip_close/1
      , zipper_goto/2
      , zipper_open_current/3]).

   :- public(open/0).

   open:-
      zip_open(_FileName_, read, Zipper, []),
      ::set_attribute(zipper, Zipper).

   :- public(close/0).
   close:-
      ::current_attribute(zipper, Zipper),
      zip_close(Zipper),
      ::del_attribute(zipper).

   :- public(goto/1).
   goto(Where):-
      ::current_attribute(zipper, Zipper),
      zipper_goto(Zipper, Where).

   :- public(stream/2).
   stream(Stream, Options):-
      ::current_attribute(zipper, Zipper),
      zipper_open_current(Zipper, Stream, Options).

   :- use_module(library(zip), [zipper_members/2]).
   :- use_module(library(lists), [member/2]).
   :- public(list/0).
   list:-
      ::current_attribute(zipper, Zipper),
      zipper_members(Zipper, Things),
      format("Content of '~w' \n",[_FileName_]),
      forall(member(T,Things), format('~w\n', [T])).
:- end_object.

:- object(xlsx(_FileName_),
   extends(zip(_FileName_))).

   :- use_module(library(sgml), [load_xml_file/2]).
   :- use_module(library(xpath), [xpath/3]).

   :- op(400, fx, //).
   :- op(400, fx, /).
   :- op(200, fy, @).

   :- public(load/0).
   load :-
      ::open,
      ::parse.

   :- protected(parse/0).

   parse:-
      ::list,
      nl,
      forall(::workbook(Name, Item), format('~w:~w\n', [Name, Item])).

   :- protected(workbook/2).
   workbook(Name,Item):-
      ::goto(file('xl/workbook.xml')),
      ::stream(Stream, []),
      load_xml_file(Stream, Xml),
      xpath(Xml, //sheet, Item).


:- end_object.


:- object(xlsx_load(_FileName_),
   extends(xlsx(_FileName_))).
:- end_object.
