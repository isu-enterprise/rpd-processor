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

:- object(row(_Number_, _Attrs_, _Cells_, _WB_)).
   :- use_module(library(sgml), [load_xml_file/2]).
   :- use_module(library(xpath), [xpath/3]).
   :- use_module(library(option), [option/2]).
   :- use_module(library(lists), [member/2]).

   :- op(400, fx, //).
   :- op(400, fx, /).
   :- op(200, fy, @).

:- end_object.

:- object(sheet(_Name_, _Id_, _XML_, _WB_)).
   :- use_module(library(sgml), [load_xml_file/2]).
   :- use_module(library(xpath), [xpath/3]).
   :- use_module(library(option), [option/2]).
   :- use_module(library(lists), [member/2]).

   :- op(400, fx, //).
   :- op(400, fx, /).
   :- op(200, fy, @).

   :- public(dump/0).
   dump:-
      format('Steet Id: ~w, Name:~w WB:~w\n', [_Id_,_Name_,_WB_]),
      % debugger::trace,
      self(Self),
      forall(_WB_::row(Self,row(I,As,Cs,_WB_)),
         format('Row:~w:~w\n~w\n', [I, As, Cs])).

   :- protected(parse/0).

   :- private(row_/2).

   :- public(row/2).
   row(Number, row(Number, Attrs, Cells, _WB_)):-
      self(Self),
      _WB_::row_(Self, row(Number, Attrs, Cells)),
      self(Self).
   row(Number, row(Number, Attrs, Cells, _WB_)):-
      self(Self),
      \+ _WB_::row_(Self, row(Number, Attrs, Cells)),!,
      xpath(_XML_, //row, element(row, Attrs, Cells)),
      option(r(NumberS),Attrs),
      atom_number(NumberS,Number),
      assertz(row_(Self, row(Number, Attrs, Cells))).

:- end_object.

:- object(workbook(_FileName_),
   extends(zip(_FileName_))).

   :- use_module(library(sgml), [load_xml_file/2]).
   :- use_module(library(xpath), [xpath/3]).
   :- use_module(library(option), [option/2]).
   :- use_module(library(lists), [member/2]).

   :- public(row_/2).
   :- dynamic(row_/2).  % Sheet->row.
   :- dynamic(sheet_/1).

   :- private(sheet_/1).

   :- op(400, fx, //).
   :- op(400, fx, /).
   :- op(200, fy, @).

   :- public(clear/0).
   clear:-
      retractall(row_(_,_)).
      retractall(sheet_(_)).

   :- public(load/0).
   load :-
      ::open,
      ::parse.

   :- protected(parse/0).
   parse :-
      ^^list, % TODO: Remove
      parseSheetList.

   list :-
      forall(::sheet_(sheet(Name,Id,File)), format('Sheet:~w | ~w | ~w\n',[Name, Id, File])).

   :- private(parseSheetList/0).
   parseSheetList:-
      forall(::sheet0(Name, sheet(Name, Id, File)),
         (Sheet = sheet(Name, Id, File),
          ::assertz(sheet_(Sheet))
          )
      ).

   :- protected(sheet0/2).
   sheet0(Name, sheet(Name,Id, file(File))):-
      ::goto(file('xl/workbook.xml')),
      ::stream(Stream, []),
      load_xml_file(Stream, Xml), !,
      close(Stream), !,
      xpath(Xml, //sheet, element(sheet, Attrs, [])),
      option(name(Name), Attrs),
      option(sheetId(Ids), Attrs),
      (atom_number(Ids, Id)->true; Id=Ids),
      format(atom(File),'xl/worksheets/sheet~w.xml', [Id]).

   :- public(sheet/2).
   sheet(Id,sheet(Name,Id,XML,WB)):-
      nonvar(Id),
      number(Id),!,
      ::sheet_(sheet(Name,Id,Data)),
      ::sheetLoad(sheet(Name,Id,Data),sheet(Name,Id,XML,WB)).
   sheet(Name,sheet(Name,Id,XML,WB)):-
      nonvar(Name), !,
      ::sheet_(sheet(Name,Id,Data)),
      ::sheetLoad(sheet(Name,Id,Data),sheet(Name,Id,XML,WB)).
   sheet(NameId,sheet(Name,Id,XML,WB)):-
      var(NameId), !,
      ::sheet_(sheet(Name,Id,Data)),
      NameId = Name-Id,
      ::sheetLoad(sheet(Name,Id,Data),sheet(Name,Id,XML,WB)).

   :- public(sheet/1).
   sheet(Sheet):-
      sheet(_, Sheet).

   :- protected(sheetLoad/2).
   sheetLoad(sheet(Name,Id,file(File)),
             sheet(Name,Id,XML,Self)):-!,
      retractall(sheet_(sheet(Name,Id,file(File)))),!,
      self(Self),
      ::goto(file(File)),
      ::stream(WStream, []),
      load_xml_file(WStream, XML),
      close(WStream),
      asserta(sheet_(sheet(Name,Id,xml(XML)))).

   sheetLoad(sheet(Name,Id,xml(XML)), sheet(Name,Id,XML,Self)):-
      self(Self).

:- end_object.


:- object(teachLoad(_FileName_),
   extends(workbook(_FileName_))).
:- end_object.