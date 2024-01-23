:- set_prolog_flag(re_compile, true).

:- use_module(library(semweb/rdf_db),
              [rdf_save/2]).

:- object(context(_Employee_), % Recodgnition context / sceraio parts.
   imports(attributes)).

   :- use_module(library(semweb/rdf11), [rdf/4,rdf_bnode/1,rdf_assert/4, rdf_is_iri/1,
                                         rdf_retractall/4, rdf_create_bnode/1
                                         ]).
   :- use_module(library(lists), [subtract/3, member/2, nth0/3, nth1/3]).
   :- use_module(library(option), [option/2]).

   :- public(graph/1).
   graph(IRI):-
      rdf_is_iri(IRI),
      ::clear, % Clear state.
      ::clear_attributes, % clear all attribute data.
      ::set_attribute(graph, IRI).

   :- public(asGraph/1).
   asGraph(Graph):-
      nonvar(Graph),
      ::graph(Graph),
      ::analyze.

   :- public(clear/0).
   clear:-!.

   :- public(analyze/0).
   analyze:-
     ::header(_HeaderRow, HeaderBottom),
     _Employee_::toUp(HeaderBottom, FieldTerm),
     _Employee_::toRowRef(FieldTerm, FieldTermRow),
     ::fields(1, FieldTermRow),
     _Employee_::toDown(HeaderBottom, TableTop),
     ::footer(TableTop, FooterTop),
     ::dump_attributes,
     _Employee_::toUp(FooterTop, TableBottom),
     _Employee_::buildHeaderStruct,
     _Employee_::headerStruct(HS),
     ::scanTable(HS, TableTop, TableBottom). % Including ends

   :- public(header/2).
   header(HRow, HRef):-
     _Employee_::headerRow(HRow),
     _Employee_::scanAfter(HRow,'итого:', R1),
     _Employee_::toRowRef(R1, HRef).

   :- public(footer/2).
   footer(HRef, FRef):-
     _Employee_::scanAfter(HRef, 'итого по', R1),
     _Employee_::toRowRef(R1,FRef).

   :- public(fields/2).
   fields(S,E):-
      forall(::cell(S,E, Cell),
             ::field(Cell,
                ['кафедра:'-chair/label,
                 'фио:'-teacher/full/name,
                 'должность:'-ext(teacher/position,[right,down]),
                 'размер ставки:'-teacher/rate/share])).

   :- public(field/2).
   field(_, []):-!.
   field(Cell, [SubAtom-ext(AttrName,Directions)|T]):-
      _Employee_::suffixInCell(SubAtom, Cell, _RightValue), !,
      forall(member(Dir, Directions),
         (
             %atom_concat(AttrName, '-', AttrNameDash),
             %atom_concat(AttrNameDash, Dir, AttrNameDir),
             AttrNameDir=AttrName-Dir,
             fieldValue(Cell, Dir, AttrNameDir))),
      field(Cell, T).

   field(Cell, [SubAtom-AttrName|T]):-
      _Employee_::suffixInCell(SubAtom, Cell, _Value),
      %debugger::trace,
      ::fieldValue(Cell, right, AttrName),
      field(Cell, T).
   field(Cell, [_|T]):-  % No refix found ...
      field(Cell, T).

   :- public(fieldValue/3).
   fieldValue(Cell, right, AttrName):-
      ::valueInCell(Cell, noprefix(Value1)),
      ( _Employee_::emptyValue(Value1) ->
        ::valueRightOf(Cell, noprefix(Value))
      ; Value = Value1 ), !,
      ::set_attribute(AttrName, Value-Cell).
   fieldValue(Cell, down, AttrName):-
      ::valueDownOf(Cell, Value),  % Ignore values to the right of the prefix
      ::set_attribute(AttrName, Value-Cell).

   :- public(scanTable/3). % table seems a keyword?
   scanTable(HeaderStruct, Top, Bottom):- % Scan from Top rowRef to Bottom one
      !.

   :- public(valueInCell/2).
   valueInCell(Cell, generic(Value)):-
      Cell::value(value(V1)), !,
      _Employee_::joinRuns(V1, V),
      _Employee_::normaizeSpace(V, Value).

   valueInCell(Cell, noprefix(Value)):-
      Cell::value(value([_|V1])), !,
      _Employee_::joinRuns(V1, V),
      _Employee_::normaizeSpace(V, Value).

   :- public(valueRightOf/2).
   valueRightOf(Cell, noprefix(Value)):- !, % find first value to the right of the current cell
      Cell::ref(Ref),
      _Employee_::toRight(Ref, RightRef),
      _Employee_::toRowRef(RightRef, RowRef),
      _Employee_::row(RowRef, Row),
      Row::cell(ref(RightRef), RightCell),
      ::valueInCell(RightCell, noprefix(JVN)),
      ( _Employee_::emptyValue(JVN) ->
        valueRightOf(RightCell, generic(Value)); Value = JVN).

   valueRightOf(Cell, generic(Value)):- % find first value to the right of the current cell
      Cell::ref(Ref),
      _Employee_::toRight(Ref, RightRef),
      _Employee_::toRowRef(RightRef, RowRef),
      _Employee_::row(RowRef, Row),
      Row::cell(ref(RightRef), RightCell),
      ::valueInCell(RightCell, generic(JVN)),
      ( _Employee_::emptyValue(JVN) ->
        valueRightOf(RightCell, generic(Value)); Value = JVN).

   :- public(valueDownOf/2).
   valueDownOf(Cell, Value):- % find first value to the right of the current cell
      Cell::ref(Ref),
      _Employee_::toDown(Ref, DownRef),
      _Employee_::toRowRef(DownRef, RowRef),
      _Employee_::row(RowRef, Row),
      Row::cell(ref(DownRef), DownCell),
      ::valueInCell(DownCell, generic(JVN)),
      ( _Employee_::emptyValue(JVN) ->
        valueDownOf(DownCell, Value); Value = JVN).

   :- public(cell/3).
   cell(StartRow, StartRow, Cell):- !,
      _Employee_::row(StartRow, Row),
      Row::cell(ref(_Ref), Cell),!.

   cell(StartRow, EndRow, Cell):-
      StartRow < EndRow,
      cell(StartRow, StartRow, Cell).

   cell(StartRow, EndRow, Cell):-
      StartRow < EndRow,
      SR is StartRow + 1,
      cell(SR, EndRow, Cell).

:- end_object.


:- object(employee(_FullName_, _Sheet_, _Load_)).
   :- use_module(library(lists), [subtract/3, member/2, nth0/3, nth1/3]).
   :- use_module(library(option), [option/2]).

   :- public(fullName/1).
   fullName(_FullName_):-
      _FullName_ \= undef, !.
   fullName(FullName):-
      ::field('ФИО:', FullName), !.

   :- public(suffixInCell/3).
   suffixInCell(Prefix, Cell, Value):-  % Prefix = the stripped first value of run list in a cell
      Cell::value(value([P|T])),
      ::normaizeSpace(P,PN),
      downcase_atom(PN, Prefix), !,
      ::joinRuns(T,Value1),
      ::normaizeSpace(Value1, Value).

   :- public(suffixOf/2).
   suffixOf(Prefix, Value):-
      _Sheet_::row(RowRef,Row),
      Row::cell(ref(Ref), Cell),
      suffixInCell(Prefix, Cell, Value2),
      (::emptyValue(Value2) -> ::rightOf(ref(Ref, RowRef), Value);
       Value=Value2).

   suffixOf(Prefix, Value):-
      _Sheet_::row(RowRef,Row),
      Row::cell(ref(Ref), Cell),
      Cell::value(value(Val)),
      ::joinRuns(Val, JV),
      sub_atom(JV, Begin, L, After, Prefix), !,
      Start is Begin + L,
      sub_atom(JV, Start, After, _, V),
      ::normaizeSpace(V,Value2),
      (::emptyValue(Value2) -> ::rightOf(ref(Ref, RowRef), Value);
       Value=Value2).

   :- public(emptyValue/1).
   emptyValue(''):-!.
   emptyValue(""):-!.
   emptyValue([]):-!.
   emptyValue(undef):-!.
   emptyValue(value(undef)):-!.

   :- public(rightOf/2).
   rightOf(ref(CellRef, RowRef), Value):- !,
      _Sheet_::row(RowRef, Row),
      % debugger::trace,
      ::toRight(CellRef, RightRef),
      % format('TR: ~w\n', [RightRef]),
      Row::cell(ref(RightRef), RightCell),
      % (RightRef = 'C7' -> debugger::trace; true),
      RightCell::value(value(RightValue)),
      % format('V: ~w:~w\n', [RightValue, RightCell]),
      ::joinRuns(RightValue,Value2),
      ::normaizeSpace(Value2, Value3),
      (::emptyValue(Value3) ->
       rightOf(ref(RightRef, RowRef), Value);
       Value=Value3).

   rightOf(prefix(Prefix), Value):-
      _Sheet_::row(RowRef,Row),
      Row::cell(ref(Ref), Cell),
      Cell::value(value(Value1)),
      ::normaizeSpace(Value1,Prefix), !,
      rightOf(ref(Ref, RowRef), Value).

   :- public(toRight/2).
   toRight(Ref, RightRef):-
      ::splitRef(Ref, Atom, Number),
      %debugger::trace,
      ::add(Atom, 1, Atom1),
      atom_concat(Atom1, Number, RightRef).
      % format('~w->~w\n',[Ref, RightRef]).

   :- public(toLeft/2).
   toLeft(Ref, LeftRef):-
      ::splitRef(Ref, Atom, Number),
      %debugger::trace,
      ::add(Atom, -1, Atom1),
      atom_concat(Atom1, Number, LeftRef).
      % format('~w->~w\n',[Ref, RightRef]).

   :- public(toUp/2).
   toUp(Ref, DownRef):-
      number(Ref), !,
      DownRef is Ref - 1.
   toUp(Ref, UpRef):-
      ::splitRef(Ref, Atom, Number),
      %debugger::trace,
      ::add(Number, -1, Number1),
      atom_concat(Atom, Number1, UpRef).
      % format('~w->~w\n',[Ref, RightRef]).

   :- public(toDown/2).
   toDown(Ref, DownRef):-
      number(Ref), !,
      DownRef is Ref + 1.
   toDown(Ref, DownRef):-
      ::splitRef(Ref, Atom, Number),
      ::add(Number, 1, Number1),
      atom_concat(Atom, Number1, DownRef).
      % format('~w->~w\n',[Ref, RightRef]).

   :- public(toRowRef/2).
   toRowRef(Ref, Ref):-
      number(Ref),!.
   toRowRef(Ref, RowRef):-
      splitRef(Ref, _, RowRef).

   :- public(scanAfter/3).
   scanAfter(StartRef, SubAtom, RowRef):-
      _Sheet_::row(StartRef, Row), !,
      % format('SCAN: ~w\n', [StartRef]),
      (::containsAll(row(Row), [SubAtom]) ->
         format('SCAN-FOUND: ~w\n', [StartRef]),
         Row::ref(RowRef);
         SR is StartRef + 1,
         % format('SCAN-NOTFOUND: ~w -> ~w\n', [StartRef, SR]),
         scanAfter(SR, SubAtom, RowRef)).

   :- public(ref/3).
   ref(Atom, Number, Ref):-
      atom_number(N, Number), !,
      atom_concat(Atom, N, Ref), !.

   :- protected(add/3).
   add(Number, V, Number1):-
      number(Number), !,
      N1 is Number + V,
      atom_number(Number1, N1).

   add(Atom, V, Atom1):-
      atom(Atom), !,
      atom_codes(Atom, Codes), !,
      codesNumber(1, Codes, Number), !,
      % debugger::trace,
      N1 is Number + V,
      codesNumber([], Codes1, N1), !,
      atom_codes(Atom1, Codes1), !.

   :- private(codesNumber/2).
   codesNumber(PV, [], PV):-!.
   codesNumber(PV, [X|T], N):-
      nonvar(X), !,
      NV is PV * 26 + (X-65),  % A = 0
      codesNumber(NV, T, N).
%   codesNumber([], [65], 0):-!.
%   codesNumber(T, T, 0):-!.
   codesNumber(T, T, N):-
      N = 1,!.
   codesNumber(T, [V|T], N):-
      N < 26, !,
      V is N + 63.
   codesNumber(T, R, N):-
      N>25, !,
      V is 65 + N mod 26,
      C is (N div 26),
      codesNumber([V|T], R, C).

   :- public(field/2).
   field(Name, Value):-
      suffixOf(Name, Value).
   field(Name, Value):-
      rightOf(prefix(Name), Value).

   :- use_module(library(pcre), [re_matchsub/4]).

   :- public(splitRef/3).
   splitRef(Ref, Alpha, Number):-
      re_matchsub("(?<alpha>[A-Z]+)(?<number>\\d+)"/x, Ref, Dict, []),
      get_dict(alpha, Dict, AlphaS),
      atom_string(Alpha, AlphaS),
      get_dict(number, Dict, NumberS),
      atom_string(NumberA, NumberS),
      atom_number(NumberA, Number).
%      format('MatchSub: ~w -> ~w\n',[Ref, Dict]), halt.

   :- public(normaizeSpace/2).
   normaizeSpace(S,NoSpace):-
      atomic(S), S\=[], !,
      normalize_space(atom(NoSpace), S).
   normaizeSpace([],[]):-!.
   normaizeSpace([X|T],[X1|T1]):-
      normaizeSpace(X,X1), !,
      normaizeSpace(T,T1).

   :- public(joinRuns/2).
   joinRuns(Run, Value):-
      atom(Run),!,
      ::normaizeSpace(Run, Value).
   joinRuns(Value, Value):-
      number(Value),!.
   joinRuns(value(undef), undef):-!.
   joinRuns(undef, undef):-!.
   joinRuns(Runs, Value):-
      atomic_list_concat(Runs, String),
      ::normaizeSpace(String, Value).

   :- public(headerRow/1).
   headerRow(Ref):-
      _Sheet_::row(Ref, Row),
      ::containsAll(row(Row), ['лекции','экзамены']).

   :- public(headerStruct/1). % list of header columns. A15-(Name, URI)
   headerStruct(HS):-
      self(Self),
      _Load_::headerStruct_(Self, HS).

   :- public(buildHeaderStruct/0).
   buildHeaderStruct:-
      ::headerRow(HR1), !,
      _Sheet_::row(HR1, R1),
      HR2 is HR1 + 1,
      _Sheet_::row(HR2, R2),
      ref('A', HR2, StartRef), !,
      ::bh(StartRef, [R1, R2], [], HS1), !,
      refineHierarchy(HS1,HSE),
      reduceHeaderStructure(HSE,HS),
      self(Self),
      % format('HS: ~w\n', [HS]),
      _Load_::assertz(headerStruct_(Self,HS)).

   :- protected(reduceHeaderStructure/2).
   reduceHeaderStructure([], []):-!.
   reduceHeaderStructure([Ref-L|T],
                         [Atom-RL|T1]):-
      splitRef(Ref, Atom, _),
      reduceList(L,RLS),
      ::normaizeSpace(RLS,RL),
      reduceHeaderStructure(T, T1).

   :- private(reduceList/2).
   reduceList([], []):-!.
   reduceList([_-V|T],[V|T1]):-
      reduceList(T,T1).

   :- private(refineHierarchy/2).
   refineHierarchy([],[]).
   refineHierarchy([Ref-[_-undef|TT]|T], R):-!,
      refineHierarchy([Ref-TT|T], R).
   refineHierarchy([Ref-[R1-Value,R2-undef|TT]|T], R):-!,
      upperValue(Ref, R2,T, Found),
      (Found = RN-ValueN ->
        refineHierarchy([Ref-[R1-Value,RN-ValueN|TT]|T], R);
        refineHierarchy([Ref-[R1-Value|TT]|T], R)).
   refineHierarchy([X|T], [X|T1]):-!,
      refineHierarchy(T, T1).

   :- protected(upperValue/4).
   upperValue(_, _, [], undef).
   upperValue(Ref, R, [RefL-L|T], Value):-
      toLeft(R, RL),
      toLeft(Ref, RefL), !,
      member(RL-V,L),
      (V = undef -> upperValue(RefL, RL, T, Value);
       Value=RL-V).
   upperValue(_, _, _, undef).

   :- protected(bh/4). % Forward processing
   bh(RefL, [RowH, RowL], Prev, Result):-
      RowL::cell(ref(RefL), CellL),
      toUp(RefL,RefH),
      RowH::cell(ref(RefH), CellH), !,
      CellH::value(value(VH)),
      CellL::value(value(VL)),
      %(RefL='AA15' -> debugger::trace;true),
      % debugger::trace,
      toRight(RefL, NewRef), !,
      % write([RefL, NewRef]), nl,
      bh(NewRef, [RowH, RowL], [RefL-[RefL-VL,RefH-VH]|Prev], Result).

   bh(_, _ , Prev, Prev).

   :- protected(containsAll/2).
   containsAll(_, []).
   containsAll(row(Row), Elems):-
      Row::cell(ref(_Ref),Cell),
      Cell::value(value(Val1)),
      subtract(Elems, [E], Els1),
      ::includes(Val1,E),
      containsAll(row(Row), Els1).

   :- public(includes/2).
   includes(undef, _):-!,false.
   includes(_, []):-!.
   includes(Value, Part):-
      atom(Value),!,
      downcase_atom(Value, V),
      downcase_atom(Part, P),
      ::normaizeSpace(P, PS),
      sub_atom(V, _, _, _, PS).
   includes([X|_], Part):-
      includes(X, Part),!.
   includes([_|T], Part):-
      includes(T, Part).

%   :- meta_predicate(semweb:rdf_db:rdf_save(*,*)).
%   :- use_module(library(semweb/turtle), []).
%   :- use_module(library(semweb/rdf_db), [rdf_save/2]).
   :- public(asGraph/1).
   asGraph(Graph):-
      % create_object(
      %    Context,
      %    [extends(context)], [], []),
      self(Self),
      Context = context(Self),
      Context::asGraph(Graph).

   :- public(row/2).
   row(Number, Row):-
      _Sheet_::row(Number, Row).

:- end_object.


:- object(teachLoad(_FileName_),
   extends(workbook(_FileName_))).

   load:-
      ^^load,
      load0.

   :- public(load/1).
   load(all):-
      ::load.

   load(fast):-
       ^^load.

   :- private(load0/0).
   load0:-
      self(Self),
      forall(
         (::sheet(Sheet),employee(undef,Sheet,Self)::fullName(FullName)),
         (  Sheet = sheet(Name, Id, _, _),
            format('Emp: ~w:~w:~w\n',[FullName,Name,Id]),
            ::assertz(employee_(employee(FullName,Sheet,Self))))
      ).
   :- dynamic(employee_/1).
   :- public(employee_/1).

   :- public(employee/2).
   employee(Id, employee(FullName, Sheet, Self)):-
      ::sheet(Id, Sheet),
      Sheet = sheet(_Name, Id, _Content, _WB),
      self(Self),
      % debugg98нек er::trace,
      employee(undef, Sheet, Self)::fullName(FullName).

   :- dynamic(headerStruct_/2).
   :- public(headerStruct_/2).

:- end_object.
