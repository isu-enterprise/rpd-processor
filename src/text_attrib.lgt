:- category(text_attrib).

    :- info([
        version is 1:0:0,
        author is 'Evgeny Cherkashin',
        date is 2022-09-16,
        comment is 'Adds addidional attributes to text runs, e.g., centered'
    ]).

    :- use_module(library(lists), [member/2]).
    :- use_module(library(option), [option/2, option/3]).
    :- use_module(library(pcre), [re_match/2, re_match/3,
                                  re_matchsub/4, re_split/4]).

    :- public(centered/2).
    :- info(centered/2, [
        comment is 'Adds a test attribute centered, if it is within its page.'
    ]).

    centered(element(N, Parent, text, Attrs, String), element(N, Parent, text, [centered=true|Attrs], String)):-
        % format("el(~w,~w,~w,~w,~w)\n",[N, Parent, text, [Attrs,PAttrs], String]),
        % debugger::trace,
        ::element(Parent, _, page, PAttrs, _),
        at_center(Attrs, PAttrs).

    at_center(As, PAs) :-
        option(left(ALeft), As),
        option(width(AWidth), As),
        option(textleft(PLeft), PAs),
        option(textwidth(PWidth), PAs),
        ::deviation(attributes, [_, DC]),!,
        PCenter is div(PWidth, 2) + PLeft,
        ACenter is div(AWidth, 2) + ALeft,
        DCenter is abs(PCenter - ACenter), !,
        DCenter < DC.

    % :- protected(deviation/2).
    % :- info(deviation/2, [
    %     comment is 'Deviation parameter.'
    % ]).

    % +- deviation(width, center) center assuming stupid indent and shift on the page when printing.

    %%%% deviation(10, 50).  % DEFINED in the composed object

    :- public(process_attrs/0).
    :- info(process_attrs/0, [
        comment is 'Process all rules of denoted by subsets'
    ]).

    process_attrs :-
        process(textsize),
        process(center_text).

    :- protected(process/1).
    :- info(process/1, [
        comment is 'Process all rules of set defined by the argument'
    ]).

    process(textsize):-
        forall(::element(N,P,page,A,S), rule(textsize, element(N,P,page,A,S))).

    process(center_text):-
        forall(::element(N,P,text,A,S), rule(center_text, element(N,P,text,A,S))).
        %forall(::element(N,P,T,A,S), format("QQ~w-~w-~w ~w '~w'\n", [N, P, T, A, S])).

    :- protected(rule/2).
    :- info(rule/2, [
        comment is 'Applies a processing rule'
    ]).

    rule(textsize, element(Page, Par, page, PAttrs, PS)) :-
        findall(Lay,
            ( ::element(_, Page, text, Attrs, S), \+ ::degraded_element(S, Page), layout(Attrs, Lay)),
            List),
        bbox(List, lay(L,T,R,B)), !,
        W is R - L,
        H is B - T,
        ::replace(element(Page, Par, page, PAttrs, PS), element(Page, Par, page,
            [textleft=L, texttop=T, textwidth=W, textheight=H | PAttrs],
            PS)).

    rule(center_text, El) :-
        centered(El, NEl), !,
        ::replace(El, NEl).

    rule(_,_).

    layout(Attrs, lay(L,T,R,B)) :-
        option(left(L), Attrs),
        option(top(T), Attrs),
        option(width(W), Attrs),
        option(height(H), Attrs),
        R is L + W,
        B is T + H.

    bbox([Lay], Lay).
    bbox([lay(L,T,R,B) | Tail], lay(L2,T2,R2,B2)):-
        bbox(Tail, lay(L1,T1,R1,B1)),
        L2 is min(L1,L),
        T2 is min(T1,T),
        R2 is max(R1,R),
        B2 is max(B1,B).

    % :- public(print0/0).
    % :- info(print0/0, [
    %     comment is 'Prints content unordered. Used for testing'
    % ]).

    % print0 :-
    %        forall(::element(N, P, Na, A, S), format("~w-~w-~w ~w '~w'\n", [N, P, Na, A, S])).


:- end_category.
