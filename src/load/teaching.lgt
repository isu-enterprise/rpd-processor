:- set_prolog_flag(re_compile, true).

:- object(employee(_FullName_, _Sheet_)).

   :- public(fullName/1).
   fullName(_FullName_):-
      _FullName_ \= undef, !.
   fullName(FullName):-
      ::field('ФИО:', FullName), !.

   :- public(suffixOf/2).
   suffixOf(Prefix, Value):-
      _Sheet_::row(RowRef,Row),
      Row::cell(ref(Ref), Cell),
      Cell::value(value([P|T])),
      ::normaizeSpace(P,Prefix), !,
      ::joinRuns(T,Value1),
      ::normaizeSpace(Value1, Value2),
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
       ::rightOf(ref(RightRef, RowRef), Value);
       Value=Value3).

   rightOf(prefix(Prefix), Value):-
      _Sheet_::row(RowRef,Row),
      Row::cell(ref(Ref), Cell),
      Cell::value(value(Value1)),
      ::normaizeSpace(Value1,Prefix), !,
      ::rightOf(ref(Ref, RowRef), Value).

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
   toUp(Ref, UpRef):-
      ::splitRef(Ref, Atom, Number),
      %debugger::trace,
      ::add(Number, -1, Number1),
      atom_concat(Atom, Number1, UpRef).
      % format('~w->~w\n',[Ref, RightRef]).

   :- public(toDown/2).
   toDown(Ref, DownRef):-
      ::splitRef(Ref, Atom, Number),
      %debugger::trace,
      ::add(Number, 1, Number1),
      atom_concat(Atom, Number1, DownRef).
      % format('~w->~w\n',[Ref, RightRef]).

   :- protected(add/3).
   add(Number, V, Number1):-
      number(Number), !,
      N1 is Number + V,
      atom_number(Number1, N1).

   add(Atom, V, Atom1):-
      atom(Atom), !,
      atom_codes(Atom, Codes), !,
      codesNumber(0, Codes, Number), !,
      % debugger::trace,
      N1 is Number + V,
      codesNumber([], Codes1, N1), !,
      atom_codes(Atom1, Codes1), !.

   :- private(codesNumber/2).
   codesNumber(PV, [], PV):-!.
   codesNumber(PV, [X|T], N):-
      nonvar(X), !,
      NV is PV * 26 + (X-65),
      codesNumber(NV, T, N).
   codesNumber([], [65], 0):-!.
   codesNumber(T, T, 0):-!.
   codesNumber(T, R, N):-
      N>0, !,
      V is 65 + N mod 26,
      C is N div 26,
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

   :- use_module(library(lists), [subtract/3, member/2]).

   :- protected(containsAll/2).
   containsAll(_, []).
   containsAll(row(Row), Elems):-
      Row::cell(ref(_Ref),Cell),
      Cell::value(value(Val1)),
      subtract(Elems, [E], Els1),
      ::includes(Val1,E),
      ::containsAll(row(Row), Els1).

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
      forall(
         (::sheet(Sheet),employee(undef,Sheet)::fullName(FullName)),
         (  Sheet = sheet(Name, Id, _, _),
            format('Emp: ~w:~w:~w\n',[FullName,Name,Id]),
            ::assertz(employee_(employee(FullName,Sheet))))
      ).
   :- dynamic(employee_/1).
   :- public(employee_/1).

   :- public(employee/2).
   employee(Id, employee(FullName, Sheet)):-
      ::sheet(Id, Sheet),
      Sheet = sheet(_Name, Id, _Content, _WB),
      % debugg98нек er::trace,
      employee(undef, Sheet)::fullName(FullName).

:- end_object.
