:- protocol(attributesp).

   :- public([
      set_attribute/2,
      current_attribute/2,
      del_attribute/1,
      clear_attributes/0,
      dump_attributes/0
      ]).
:- end_protocol.


:- category(attributes,
   implements(attributesp)).
   :- protected(attribute_/2).
   :- dynamic(attribute_/2).

   set_attribute(Name,Value):-
      ::retractall(attribute_(Name,_)),
      ::assertz(attribute_(Name,Value)).

   current_attribute(Name,Value):-
      ::attribute_(Name, Value).

   del_attribute(Name):-
      ::attribute_(Name,_),!,
      ::retractall(attribute_(Name,_)).

   clear_attributes:-
      ::retractall(attribute_(_,_)).

   dump_attributes:-
      forall(current_attribute(Name, Value),
         format('~w=|~w|\n',[Name, Value])).

:- end_category.


:- object(config,
   imports(attributes)).

   :- public([
      set/2,
      get/2,
      get/1
   ]).

   set(Name, Value):-
      ::set_attribute(Name,Value).

   get(Name, Value):-
      ::current_attribute(Name, Value).

   get(Name-Value):-
      ::get(Name, Value).

   % Name(Value).
:- end_object.
