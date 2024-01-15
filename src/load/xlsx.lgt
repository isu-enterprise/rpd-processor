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


:- object(sheet(_Name_,_Id_, _XML_)).
   :- use_module(library(sgml), [load_xml_file/2]).
   :- use_module(library(xpath), [xpath/3]).
   :- use_module(library(option), [option/2]).
   :- use_module(library(lists), [member/2]).

   :- op(400, fx, //).
   :- op(400, fx, /).
   :- op(200, fy, @).

   :- public(dump/0).
   dump:-
      % format('Steet Id: ~w, Name:~w\n~w\n', [_Id_,_Name_,_XML_]).
      forall(::row(I,row(I,As,Cs)),
         format('Row:~w:~w\n', [I, As])).

   :- protected(parse/0).


   :- dynamic(row/2).
   :- public(row/2).

   :- public(load/0).
   load:-
         % debugger::trace,
      forall(
         xpath(_XML_, //row, element(row, Attrs, Cells)),
         load0(Attrs, Cells)).

   load0(Attrs, Cells):-
      option(r(Rs), Attrs),
      atom_number(Rs,R),
      assertz(row(R, row(R,Attrs,Cells))).


:- end_object.

:- object(workbook(_FileName_),
   extends(zip(_FileName_))).

   :- use_module(library(sgml), [load_xml_file/2]).
   :- use_module(library(xpath), [xpath/3]).
   :- use_module(library(option), [option/2]).
   :- use_module(library(lists), [member/2]).

   :- public(sheet/2).
   :- dynamic(sheet/2).

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
      forall(::sheet0(Name, sheet(Name, Id, XML)),
         (format('Sheet: ~w, ~w\n', [Name, Id]),
          Sheet = sheet(Name, Id, XML),
          assertz(sheet(Name, Sheet)),
          assertz(sheet(Id, Sheet)),
          Sheet::load)
      ).

   :- protected(sheet0/2).
   sheet0(Name, sheet(Name,Id, XML)):-
      ::goto(file('xl/workbook.xml')),
      ::goto(file('xl/workbook.xml')),
      ::stream(Stream, []),
      load_xml_file(Stream, Xml), !,
      close(Stream), !,
      xpath(Xml, //sheet, element(sheet, Attrs, [])),
      option(name(Name), Attrs),
      option(sheetId(Ids), Attrs),
      (atom_number(Ids, Id)->true; Id=Ids),
      format(atom(S),'xl/worksheets/sheet~w.xml', [Id]),
      ::goto(file(S)),
      ::stream(WStream, []),
      load_xml_file(WStream, XML),
      close(WStream).

:- end_object.


:- object(teachLoad(_FileName_),
   extends(workbook(_FileName_))).
:- end_object.
