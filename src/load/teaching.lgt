:- set_prolog_flag(re_compile, true).

:- use_module(library(semweb/rdf_db),
              [rdf_save/2]).


:- category(rdf_namespaces).
   % :- op()
   :- public(namespace/2).

   namespace(sch, 'https://schema.org/').
   namespace(cnt, 'http://www.w3.org/2011/content#').
   namespace(dbr, 'http://dbpedia.org/resource/').
   namespace(bibo, 'http://purl.org/ontology/bibo/').
   namespace(brick,'https://brickschema.org/schema/Brick#').
   namespace(csvw,'http://www.w3.org/ns/csvw#').
   namespace(dc,'http://purl.org/dc/elements/1.1/').
   namespace(dcat,'http://purl.org/spar/datacite/').
   namespace(dcmitype,'http://purl.org/dc/dcmitype/').
   namespace(dcterms,'http://purl.org/dc/terms/').
   namespace(dcam,'http://purl.org/dc/dcam/').
   namespace(doap,'http://usefulinc.com/ns/doap#').
   namespace(foaf,'http://xmlns.com/foaf/0.1/').
   namespace(odrl2,'http://www.w3.org/ns/odrl/2/').
   namespace(org,'http://www.w3.org/ns/org#').
   namespace(owl,'http://www.w3.org/2002/07/owl#').
   namespace(prof,'http://www.w3.org/ns/dx/prof/').
   namespace(prov,'http://www.w3.org/ns/prov#').
   namespace(qb,'http://purl.org/linked-data/cube#').
   namespace(rdf,'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
   namespace(rdfs,'http://www.w3.org/2000/01/rdf-schema#').
   namespace(sdo,'https://schema.org/').
   namespace(sh,'http://www.w3.org/ns/shacl#').
   namespace(skos,'http://www.w3.org/2004/02/skos/core#').
   namespace(sosa,'http://www.w3.org/ns/sosa/').
   namespace(ssn,'http://www.w3.org/ns/ssn/').
   namespace(time,'http://www.w3.org/2006/time#').
   namespace(vann,'http://purl.org/vocab/vann/').
   namespace(void,'http://rdfs.org/ns/void#').
   namespace(wgs,'https://www.w3.org/2003/01/geo/wgs84_pos#').
   namespace(xsd,'http://www.w3.org/2001/XMLSchema#').

   :- use_module(library(semweb/rdf11), [rdf/4,rdf_bnode/1,rdf_assert/4, rdf_is_iri/1,
                                         rdf_retractall/4, rdf_create_bnode/1
                                         ]).
   :- public(global_id/2).
   global_id(Abbrev:Atom, IRI):-
      nonvar(Abbrev), nonvar(Atom),!,
      ::namespace(Abbrev, NS), !,
      atom_concat(NS, Atom, IRI).
   global_id(Abbrev:Atom, IRI):-
      nonvar(Atom), nonvar(IRI),!,
      sub_atom(IRI,_,L,0,Atom),
      sub_atom(IRI,0,L,_,NS),
      ::namespace(Abbrev, NS).
   global_id(Abbrev:Atom, IRI):-
      nonvar(Abbrev), nonvar(IRI),!,
      ::namespace(Abbrev, NS), !,
      sub_atom(IRI,0,L,A,NS),
      sub_atom(IRI,L,A,0,Atom).
   global_id(IRI,IRI):-
      rdf_is_iri(IRI),!.

   :- op(700, xfx, ~~).
   :- protected(equalResource/2).
   equalResource(A,B) :-
      global_id(A,URIA),
      global_id(B,URIA).

:- end_category.

:- category(isu_namespaces,
      extends(rdf_namespaces)).
   namespace(schema,'https://schema.org/').
   namespace(Abbrev, URI):-
      ^^namespace(Abbrev, URI). % Namespace would not be redefined during extension
   % local namespaces.
   namespace(it,'http://irnok.net/ontologies/database/isu/it-chair/employee#').
   namespace(wpdb,'http://irnok.net/ontologies/database/isu/workprog#').
   namespace(wpdd,'http://irnok.net/ontologies/isu/workprog#').
   namespace(idb, 'http://irnok.net/ontologies/database/isu/studplan#').
   namespace(idd, 'http://irnok.net/ontologies/isu/studplan#').

   namespace(libdb, 'http://irnok.net/ontologies/database/isu/library#').
   namespace(libdd, 'http://irnok.net/ontologies/isu/library#').
   namespace(bibframe, 'http://id.loc.gov/ontologies/bibframe/').
:- end_category.

% ------------------------------------------- recognition rule categories ---------------------------

:- category(employee_context_rules).

   :- protected(fieldSet/1).
   fieldSet([  'кафедра:'=chair/label
             , 'фио:'=teacher/full/name
             , 'индивидуальный расчёт учебной нагрузки преподавателя на '=load/years  % TODO: DOES NOT WORK
             , 'должность:'=ext(teacher/position,[right,down])
             , 'размер ставки:'=teacher/rate/share]).

   :- protected(encodeKD/2).
   %encodeKD([лаборат, занят, всего], [convert(::toInteger), pack(total/laboratory/hours)]).

   encodeKD([рецензирован, выпускн, работ, бакалавр, специалист, магистр],
      [convert(::toInteger), pack(reviewing/bach_spec_master/hours)]).
   encodeKD([руководств,   выпускн, работ, бакалавр, специалист, магистр],
      [convert(::toInteger), pack(supervision/bach_spec_master/hours)]).

   encodeKD([препод, код, дисципл, учебн, план], [exec(::ignored), convert(::undef)]).
   encodeKD([обзорн, лекц, консультац, государствен, экзам], [convert(::toInteger), pack(consulting/exams/hours)]).
   encodeKD([практ, семинар, занят, по, план], [convert(::toInteger), pack(practice/plan/hours)]).

   encodeKD([руководств, аспирант, соискател, стажер], [convert(::toInteger), pack(supervision/postgr_soiskat_stager/hours)]).
   encodeKD([экспертиз, диссер, исслед, rандидатск], [convert(::toInteger), pack(supervision/candidate/hours)]).
   encodeKD([экспертиз, диссер, исслед, докторск], [convert(::toInteger), pack(supervision/doctor/hours)]).
   encodeKD([организац, сопровожден, работ, совет], [convert(::toInteger), pack(dissconsil/organization/hours)]).
   encodeKD([проверк, реферат, аспирант, докторант], [convert(::toInteger), pack(checking/works/hours)]).
   encodeKD([государ, участ, гак, экзам], [convert(::toInteger), pack(exams/state/hours)]).
   encodeKD([практ, семинар, занят, всего], [convert(::toInteger), pack(practice/total/hours)]).
   encodeKD([лекц, по, план, всего], [convert(::toInteger), pack(lection/plan/hours)]).
   encodeKD([код, дисциплин, учебн, план], [convert(::stripSpaces), convert(::disciplineCode)]).

   encodeKD([рецензирован, диссертационн, исследован], [convert(::toInteger), pack(reviewing/dissertation/hours)]).
   encodeKD([руководств, диссертац, совет], [convert(::toInteger), pack(dissconcil/heading/hours)]).
   encodeKD([количеств, учебн, групп], [convert(::toInteger), pack(student/group/no)]).
   encodeKD([производ, педагог, практик], [convert(::toInteger), pack(practice/intership/hours)]).
   encodeKD([научн, докторант, консультац], [convert(::toInteger), pack(consulting/scientific/doctorant/hours)]).
   encodeKD([текущ, студент, консультац], [convert(::toInteger), pack(consulting/student/hours)]).
   encodeKD([участ, работ, гак], [convert(::toInteger), pack(scc/hours)]). % state certification commission

   encodeKD([вступительн, экзам], [convert(::toInteger), pack(exams/entrance/hours)]).
   encodeKD([руководств, магистрант], [convert(::toInteger), pack(supervision/magister/hours)]).
   encodeKD([рецензиров, выпускн], [convert(::toInteger), pack(reviewing/fqw/hours)]). % Final qualifying work
   encodeKD([руководств, выпускн], [convert(::toInteger), pack(supervision/fqw/hours)]).
   encodeKD([контингент, студент], [convert(::toInteger), pack(student/no)]).
   encodeKD([наименован, дисциплин], [convert(::stripSpaces), pack(discipline/name)]).
   encodeKD([кандидатск, экзам], [convert(::toInteger), pack(exams/candidate/hours)]).
   encodeKD([контрольн, работ], [convert(::toInteger), pack(control_work/hours)]).
   encodeKD([лаборат, занят], [convert(::toInteger), pack(laboratory/hours)]).
   encodeKD([курсов, экзам], [convert(::toInteger), pack(exams/course/hours)]).
   encodeKD([курсов, работ], [convert(::toInteger), pack(course_works/hours)]).
   encodeKD([занят, аспирант], [convert(::toInteger), pack(train/postgr/hours)]).
   encodeKD([учебн, практик], [convert(::toInteger), pack(practice/education/hours)]).
   encodeKD([курс, семестр], [convert(::discipName)]).  % Use exec(::writeLn) for debug write
   encodeKD([лекц, всего], [convert(::toInteger), pack(lection/total/hours)]).

   %encodeKD([практик], [convert(::toInteger), pack(practice/hours)]).
   encodeKD([зачет], [convert(::toInteger), pack(credit/hours)]).
   encodeKD([кср], [convert(::toInteger), pack(ksr/hours)]).
   encodeKD([всего], [convert(::toInteger), pack(total/hours)]).

   :- use_module(library(pcre), [re_matchsub/4]).
   :- protected(discipName/3).
   discipName(_, Atom, [course/no=AN,semester/no=BN]):-
      re_matchsub("(?<a>\\d+)\\s*[/]\\s*(?<b>\\d+)"/x, Atom, Dict, []),
      get_dict(a, Dict, AS),
      atom_string(A, AS),
      atom_number(A, AN),
      get_dict(b, Dict, BS),
      atom_string(B, BS),
      atom_number(B, BN),!.
   discipName(_, Atom, string=Atom).

   :- protected(disciplineCode/3).
   disciplineCode(_, Atom, discipline/code=Atom):-
      re_matchsub("([А-Я]+\\d*)([.][А-Я]*\\d*)*"/x, Atom, _Dict, []),!.
   disciplineCode(ColRef, Atom, undef):-
      format('WARNING: unrecognized course code: ~w at column ~w\n',[Atom, ColRef]).

   :- protected(generateFromFields/0).
   generateFromFields.


:- end_category.

% --------------------------------------------- recognition contextes ------------------------------------

:- object(context(_Employee_), % Recodgnition context / sceraio parts.
   imports([attributes, isu_namespaces])).

   :- op(700, xfx, ~~).
   % :- initialization(( ::global_id(rdf:typeOf, ID),
   %      ::equalResource(ID, rdf:typeOf),
   %      write('OK COMP\n') )).


   :- use_module(library(semweb/rdf11), [rdf/4,rdf_bnode/1,rdf_assert/4, rdf_is_iri/1,
                                         rdf_retractall/4, rdf_create_bnode/1
                                         ]).
   :- use_module(library(lists), [subtract/3, member/2, nth0/3, nth1/3]).
   :- use_module(library(option), [option/2]).

   :- public(graph/1).
   graph(IRIi):-
      ::global_id(IRIi,IRI),
      format('INFO: Constructing graph ~w\n',[IRI]),
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
     ::header(_HeaderRow, HeaderBottom), !,
     _Employee_::toUp(HeaderBottom, FieldTerm), !,
     _Employee_::toRowRef(FieldTerm, FieldTermRow),
     ::fields(1, FieldTermRow), !,
     ::dump_attributes, !,
     ::generateFromFields,
     _Employee_::toDown(HeaderBottom, TableTop),
     ::footer(TableTop, FooterTop), !,
     _Employee_::toUp(FooterTop, TableBottom),
     _Employee_::buildHeaderStruct, !,
     _Employee_::headerStruct(HS), !,
     ::scanTable(HS, TableTop, TableBottom).

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
      ::fieldSet(FS), !,
      forall(
             ::cell(S,E, Cell),
             ::field(Cell, FS)).

   :- public(field/2).
   field(_, []):-!.
   field(Cell, [SubAtom=ext(AttrName,Directions)|T]):-
      _Employee_::suffixInCell(SubAtom, Cell, _RightValue), !,
      forall(member(Dir, Directions),
         (
             %atom_concat(AttrName, '-', AttrNameDash),
             %atom_concat(AttrNameDash, Dir, AttrNameDir),
             AttrNameDir=(AttrName/Dir),
             fieldValue(Cell, Dir, AttrNameDir))),
      field(Cell, T).

   field(Cell, [SubAtom=AttrName|T]):-
      _Employee_::suffixInCell(SubAtom, Cell, _Value),
      ::fieldValue(Cell, right, AttrName),
      field(Cell, T).
   field(Cell, [_|T]):-  % No refix found ...
      field(Cell, T).

   :- public(fieldValue/3).
   % TODO Split value
   fieldValue(Cell, right, AttrName):-
      ::valueInCell(Cell, noprefix(Value1)),
      ( _Employee_::emptyValue(Value1) ->
        ::valueRightOf(Cell, noprefix(Value))
      ; Value = Value1 ), !,
      ::set_attribute(AttrName, Value=Cell).
   fieldValue(Cell, down, AttrName):-
      ::valueDownOf(Cell, Value),  % Ignore values to the right of the prefix
      ::set_attribute(AttrName, Value=Cell).

   :- public(scanTable/3). % table seems a keyword?
   scanTable(HeaderStruct, Top, Bottom):- % Scan from Top rowRef to Bottom one
      format('HS:~w\n',[HeaderStruct]),
      forall(
         ::rows(Top, Bottom, Row),
         (
            findall(NewValue,
               (
                  Row::cell(ref(Ref), Cell),
                  Cell::value(value(Value)),
                  \+ _Employee_::emptyValue(value(Value)),
                  % Cell::ref(Ref),
                  _Employee_::toColRef(Ref, ColRef),
                  member(ColRef=Definition, HeaderStruct), % Then
                  ::processStructuredValue(ColRef, Value, Definition, NewValue1),
                  \+ _Employee_::emptyValue(NewValue1),
                  (NewValue1=[_|_] -> member(NewValue, NewValue1); NewValue = NewValue1)
               ), Rels),
            Row::ref(RowRef),
            format('COLLECTED: ~w: ~w\n',[RowRef, Rels])
         )
      ),
      !.

   :- protected(processStructuredValue/4).
   processStructuredValue(ColRef, Value, Definition, NewValue):-
      ::encodeKD(Elems, Options),
      ::containsAll(Definition, Elems),
      !,
      ::interpreteOptions(ColRef, Value, Options, NewValue).
   processStructuredValue(ColRef, Value, Definition, ColRef=Value):-
      format("WARNING noproc: ~w = ~w (~w)\n",[ColRef, Value, Definition]).

   :- protected(interpreteOptions/4).
   interpreteOptions(_, Value, [], Value):-!.
   interpreteOptions(ColRef, Value, [Option|Os], NVal):-
      ::interpreteOption(ColRef, Value, Option, NewValue), !,
      interpreteOptions(ColRef, NewValue, Os, NVal).

   :- protected(interpreteOption/4).
   interpreteOption(ColRef, Value, convert(P), NewValue):-!,
      call(P, ColRef, Value, NewValue).
   interpreteOption(ColRef, Value, exec(P), Value):-!,
      call(P, ColRef, Value).
   interpreteOption(_, undef, pack(_), undef):-!.
   interpreteOption(_, Value, pack(Atom), Atom=Value):-!.

   :- protected(id/3).
   id(_, A,A).

   :- protected(undef/3).
   undef(_,_,undef).

   :- protected(toInteger/3).
   toInteger(_,Atom, Number):-
      atom(Atom), !,
      atom_number(Atom, Number).
   toInteger(_, Integer, Integer):-
      integer(Integer),!.

   :- protected(stripSpaces/3).
   stripSpaces(_, Value, NoSpaceValue):-!,
      _Employee_::normaizeSpace(Value, NoSpaceValue).

   :- protected(writeNl/2).
   writeNl(ColRef, Value):-
      format('DEBUG write: ~w = ~w\n',[ColRef, Value]).

   :- protected(ignored/2).
   ignored(ColRef, Value):-
      format('IGNORING: ~w = ~w\n',[ColRef, Value]).

   :- protected(containsAll/2).
   containsAll(_, []):-!.
   containsAll(L,Elems):-
      member(X,L),
      downcase_atom(X, XL),
      subtract(Elems, [E], Els),
      sub_atom(XL,_,_,_,E), !,
      containsAll(L,Els).

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

   :- public(rows/3).
   % rows(StartRow, StartRow, Row):-
   rows(StartRow, StartRow, Row):-!,
      _Employee_::row(StartRow, Row).
   rows(StartRow, EndRow, Row):-
      StartRow < EndRow,
      rows(StartRow, StartRow, Row).
   rows(StartRow, EndRow, Row):-
      StartRow < EndRow,
      SR is StartRow + 1,
      rows(SR, EndRow, Row).

   :- public(cell/3).
   cell(StartRow, EndRow, Cell):-
      ::rows(StartRow, EndRow, Row),
      % format('ROWS: ~w ~w ~n',[StartRow, EndRow]),
      Row::cell(ref(_Ref), Cell).

:- end_object.

% -----------------------------------------

:- object(employee_context(_Employee_),
      extends(context(_Employee_)),
      imports(employee_context_rules)).
:- end_object.

% -------------------------------------------------------------------------------------

:- object(employee(_FullName_, _Sheet_, _Load_)).
   :- use_module(library(lists), [subtract/3, member/2, nth0/3, nth1/3]).
   :- use_module(library(option), [option/2]).

   :- public(fullName/1).
   fullName(_FullName_):-
      _FullName_ \= undef, !.
   fullName(FullName):-
      ::field('фио:', FullName), !.

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
      ::add(Atom, 1, Atom1),
      atom_concat(Atom1, Number, RightRef).
      % format('~w->~w\n',[Ref, RightRef]).

   :- public(toLeft/2).
   toLeft(Ref, LeftRef):-
      ::splitRef(Ref, Atom, Number),
      ::add(Atom, -1, Atom1),
      atom_concat(Atom1, Number, LeftRef).
      % format('~w->~w\n',[Ref, RightRef]).

   :- public(toUp/2).
   toUp(Ref, DownRef):-
      number(Ref), !,
      DownRef is Ref - 1.
   toUp(Ref, UpRef):-
      ::splitRef(Ref, Atom, Number),
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

   :- public(toColRef/2).
   toColRef(Ref, Atom):-
      splitRef(Ref, Atom, _).

   :- public(scanAfter/3).
   scanAfter(StartRef, SubAtom, RowRef):-
      _Sheet_::row(StartRef, Row), !,
      (::containsAll(row(Row), [SubAtom]) ->
         format('SCAN-AFTER-FOUND: ~w\n', [StartRef]),
         Row::ref(RowRef);
         SR is StartRef + 1,
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
      N1 is Number + V,
      codesNumber([], Codes1, N1), !,
      atom_codes(Atom1, Codes1), !.

   :- private(codesNumber/2).
   codesNumber(PV, [], PV):-!.
   codesNumber(PV, [X|T], N):-
      nonvar(X), !,
      NV is PV * 26 + (X-65),  % A = 0
      codesNumber(NV, T, N).
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
      ::refineHierarchy(HS1,HSE),
      ::reduceHeaderStructure(HSE,HSS1),
      ::canonifyStrings(HSS1, HS),
      self(Self),
      % format('HS: ~w\n', [HS]),
      _Load_::assertz(headerStruct_(Self,HS)).

   :- protected(reduceHeaderStructure/2).
   reduceHeaderStructure([], []):-!.
   reduceHeaderStructure([Ref=L|T],
                         [Atom=RL|T1]):-
      splitRef(Ref, Atom, _),
      reduceList(L,RLS),
      ::normaizeSpace(RLS,RL),
      reduceHeaderStructure(T, T1).

   :- protected(canonifyStrings/2).
   canonifyStrings([], []):-!.
   canonifyStrings([Ref=V|T],[Ref=CV|CT]):-
      canonifyStrings(V, CV), !,
      canonifyStrings(T, CT).
   canonifyStrings([Atom|T], [CAtom|CT]):-
      downcase_atom(Atom, CAtom), !,
      canonifyStrings(T,CT).

   :- private(reduceList/2).
   reduceList([], []):-!.
   reduceList([_=V|T],[V|T1]):-
      reduceList(T,T1).

   :- private(refineHierarchy/2).
   refineHierarchy([],[]).
   refineHierarchy([Ref=[(_=undef)|TT]|T], R):-!,
      refineHierarchy([Ref=TT|T], R).
   refineHierarchy([Ref=[(R1=Value),(R2=undef)|TT]|T], R):-!,
      upperValue(Ref, R2,T, Found),
      (Found = (RN=ValueN) ->
        refineHierarchy([Ref=[(R1=Value),(RN=ValueN)|TT]|T], R);
        refineHierarchy([Ref=[(R1=Value)|TT]|T], R)).
   refineHierarchy([X|T], [X|T1]):-!,
      refineHierarchy(T, T1).

   :- protected(upperValue/4).
   upperValue(_, _, [], undef).
   upperValue(Ref, R, [RefL=L|T], Value):-
      toLeft(R, RL),
      toLeft(Ref, RefL), !,
      member(RL=V,L),
      (V = undef -> upperValue(RefL, RL, T, Value);
       Value=(RL=V)).
   upperValue(_, _, _, undef).

   :- protected(bh/4). % Forward processing
   bh(RefL, [RowH, RowL], Prev, Result):-
      RowL::cell(ref(RefL), CellL),
      toUp(RefL,RefH),
      RowH::cell(ref(RefH), CellH), !,
      CellH::value(value(VH)),
      CellL::value(value(VL)),
      %(RefL='AA15' -> debugger::trace;true),
      toRight(RefL, NewRef), !,
      % write([RefL, NewRef]), nl,
      bh(NewRef, [RowH, RowL], [RefL=[RefL=VL,RefH=VH]|Prev], Result).

   bh(_, _ , Prev, Prev).

   :- use_module(library(lists), [intersection/3]).
   :- protected(containsAll/2).
   containsAll(_, []).
   containsAll(row(Row), Elems):-
      Row::cell(ref(_Ref),Cell),
      Cell::value(value(Val1)),
      subtract(Elems, [E], Els1),
      ::includes(Val1,E),
      containsAll(row(Row), Els1).
   containsAll(list(L),Elems):-
      intersection(L,Elems,Elems).

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
      Context = employee_context(Self),
      Context::asGraph(Graph).

   :- public(row/2).
   row(Number, Row):-
      _Sheet_::row(Number, Row).

:- end_object.

% -------------------------------------------------------------

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
      employee(undef, Sheet, Self)::fullName(FullName).

   :- dynamic(headerStruct_/2).
   :- public(headerStruct_/2).

:- end_object.
