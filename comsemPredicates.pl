﻿/*************************************************************************

    File: comsemPredicates.pl
    Copyright (C) 2004,2005,2006 Patrick Blackburn & Johan Bos

    This file is part of BB1, version 1.3 (November 2006).

    BB1 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB1 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB1; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(comsemPredicates,[
		appendLists/3,
		basicFormula/1,
		compose/3,
		concatStrings/2,
		executeCommand/1,
		infix/0,
		memberList/2,
		newFunctionCounter/1,
		prefix/0,
		printRepresentations/1,
		proveOnce/1,
		removeFirst/3,
		removeDuplicates/2,
		reverseList/2,
		selectFromList/3,
		simpleTerms/1,
		substitute/4,
		unionSets/3,
		variablesInTerm/2,

		sublist/2,
		subset/2,
		cartesianProduct/3,
		cartesianProduct2/3,
		cartesianUnion/3,
		cartesianIntersection/3,
		findAllOrderedN/3,
		diffList/3,
		massUnion/2,
		massIntersection/2,
		maxiSet/2,
		intersectionSets/3,
		splitSettoN/3,
		splitSettoN/4]).


/*========================================================================
   Appending two lists
========================================================================*/

appendLists([],List,List).
appendLists([X|Tail1],List,[X|Tail2]):- 
   appendLists(Tail1,List,Tail2).


/*========================================================================
   List membership
========================================================================*/

memberList(X,[X|_]).
memberList(X,[_|Tail]):- 
   memberList(X,Tail).

 
/*========================================================================
   Reversing Items in a List
========================================================================*/
 
reverseList(A,B):-
   reverseList(A,[],B).
 
reverseList([],L,L).
 
reverseList([X|L1],L2,L3):-
   reverseList(L1,[X|L2],L3).


/*========================================================================
   Selecting (i.e. removing) a member of a list
========================================================================*/

selectFromList(X,[X|L],L).
selectFromList(X,[Y|L1],[Y|L2]):-
   selectFromList(X,L1,L2).


/*========================================================================
   Removing first member of a list
========================================================================*/

removeFirst(X,[X|Tail],Tail) :- !.
removeFirst(X,[Head|Tail],[Head|NewTail]):-
   removeFirst(X,Tail,NewTail).


/*========================================================================
   Remove Duplicates
========================================================================*/

removeDuplicates([],[]).

removeDuplicates([X|L],Pruned):-
	memberList(Y,L), X==Y, !,
	removeDuplicates(L,Pruned).

removeDuplicates([X|L],[X|Pruned]):-
	removeDuplicates(L,Pruned).


/*========================================================================
   Union of two sets (disallowing unification of list elements)
========================================================================*/

unionSets([],L,L).

unionSets([X|L1],L2,L3):-
   memberList(Y,L2), 
   X==Y, !,
   unionSets(L1,L2,L3).

unionSets([X|L1],L2,[X|L3]):-
   unionSets(L1,L2,L3).


/*========================================================================
   Simple Terms
========================================================================*/

simpleTerms([]).

simpleTerms([X|Rest]):-
   simpleTerm(X),
   simpleTerms(Rest).

simpleTerm(T):-
   (
    var(T)
   ;   
    atomic(T)
   ;
    nonvar(T),
    functor(T,'$VAR',1) 
   ;
    nonvar(T),
    functor(T,fun,_)
   ).


/*========================================================================
   Compose predicate argument structure
========================================================================*/

compose(Term,Symbol,ArgList):-
    Term =.. [Symbol|ArgList].


/*========================================================================
   Basic Formula Syntax
========================================================================*/

basicFormula(F):-
   var(F), !, fail.

basicFormula(F):-
   compose(F,Symbol,Args),
   \+ memberList(Symbol,[not,and,imp,app,or,some,all,lam,eq]),
   simpleTerms(Args).
		

/*========================================================================
   Collect all occurrences of variables in Term to a difference list
========================================================================*/

variablesInTerm(Term,Var1-Var2):-
   compose(Term,_,Args),
   countVar(Args,Var1-Var2).

countVar([],Var-Var).
countVar([X|Rest],Var1-Var2):-
   var(X),!,
   countVar(Rest,[X|Var1]-Var2).
countVar([X|Rest],Var1-Var3):-
   variablesInTerm(X,Var1-Var2),
   countVar(Rest,Var2-Var3).


/*========================================================================
   Substitution Predicates
========================================================================*/

substitute(Term,Var,Exp,Result):- 
   Exp==Var, !, Result=Term.

substitute(_Term,_Var,Exp,Result):- 
   \+ compound(Exp), !, Result=Exp.

substitute(Term,Var,Formula,Result):-
   compose(Formula,Functor,[Exp,F]),
   memberList(Functor,[lam,all,some]), !, 
   (
    Exp==Var, !, 
    Result=Formula
   ; 
    substitute(Term,Var,F,R),
    compose(Result,Functor,[Exp,R])
   ).

substitute(Term,Var,Formula,Result):-
   compose(Formula,Functor,ArgList),
   substituteList(Term,Var,ArgList,ResultList),
   compose(Result,Functor,ResultList).

substituteList(_Term,_Var,[],[]).
substituteList(Term,Var,[Exp|Others],[Result|ResultOthers]):-
   substitute(Term,Var,Exp,Result),
   substituteList(Term,Var,Others,ResultOthers).


/*========================================================================
   Skolem Function Counter
========================================================================*/

:- dynamic(functionCounter/1).

functionCounter(1).

newFunctionCounter(N):-
   functionCounter(N), M is N+1,
   retract(functionCounter(N)),
   asserta(functionCounter(M)).


/*========================================================================
   Printing a set of representations
========================================================================*/

printRepresentations(Readings):-
   printRep(Readings,0).

printRep([],_):- nl.
printRep([Reading|OtherReadings],M):-
   N is M + 1, nl, write(N), tab(1), 
   \+ \+ (numbervars(Reading,0,_), print(Reading)),
   printRep(OtherReadings,N).


/*========================================================================
   Concatenate Strings
========================================================================*/

concatStrings(L,S):-
   concatStrings(L,[],S).

concatStrings([],Codes,String):- 
   name(String,Codes).

concatStrings([X|L],Codes1,String):-
   name(X,Codes2),
   appendLists(Codes1,Codes2,Codes3),
   concatStrings(L,Codes3,String).
   

/*========================================================================
   Prove a goal only once
========================================================================*/

proveOnce(Goal):- call(Goal), !.


/*========================================================================
   Prolog Dialect Detection
========================================================================*/

prologDialect(Dialect):-
   (
    predicate_property(version,Property),
    Property=built_in, !,
    Dialect=sicstus            % Probably Sicstus Prolog
   ;
    predicate_property(shell(_,_),Property),
    Property=interpreted, !,
    Dialect=swi                % Probably SWI Prolog
   ;
    Dialect=unknown
   ).


/*========================================================================
   Execute Operating System Command
========================================================================*/

executeCommand(A):-
   shell(A,_).
   

/*========================================================================
   Load Sicstus library if required
========================================================================*/

load_shell:-
   prologDialect(sicstus), !,
   use_module(library(system),[shell/2]).

load_shell.


/*========================================================================
   Load Shell
========================================================================*/

:- load_shell.


/*========================================================================
   Prefix/Infix Switch
========================================================================*/
  
:- dynamic bbmode/1.
 
bbmode(prefix).
 
infix:- retractall(bbmode(_)), assert(bbmode(infix)).
prefix:- retractall(bbmode(_)), assert(bbmode(prefix)).

 
/*========================================================================
   Portray B&B Syntax
========================================================================*/

%user:portray(E):- bbmode(prefix), !, write_term(E,[numbervars(true)]).

user:portray(not(F)):- bbmode(infix), !, write('~ '), print(F).
user:portray(and(F1,F2)):- bbmode(infix), !, write('('), print(F1), write(' & '), print(F2), write(')').
user:portray(imp(F1,F2)):- bbmode(infix), !, write('('), print(F1), write(' > '), print(F2), write(')').
user:portray(app(F1,F2)):- bbmode(infix), !, write('('), print(F1), write(' @ '), print(F2), write(')').
user:portray(or(F1,F2)):- bbmode(infix), !, write('('), print(F1), write(' v '), print(F2), write(')').
user:portray(some(X,F)):- bbmode(infix), !, write('some '), write_term(X,[numbervars(true)]), tab(1), print(F).
user:portray(all(X,F)):- bbmode(infix), !, write('all '), write_term(X,[numbervars(true)]), tab(1), print(F).
user:portray(lam(X,F)):- bbmode(infix), !, write('lam '), write_term(X,[numbervars(true)]), tab(1), print(F).
user:portray(eq(X,Y)):- bbmode(infix), !, write_term(X,[numbervars(true)]), write(' = '), write_term(Y,[numbervars(true)]).
user:portray(que(X,R,B)):- bbmode(infix), !, write('?'), write_term(X,[numbervars(true)]), write('['), print(R), write(','), print(B), write(']').
user:portray(model(A,B)):- printModel(model(A,B)).
user:portray(E):- write_term(E,[numbervars(true)]).


/*========================================================================
   Print Model
========================================================================*/

printModel(model(Dom,Ext)):-
   write('D='),write(Dom),nl,
   printExtensions(Ext).

printExtensions([]).

printExtensions([X|L]):-
   tab(2), write(X), nl,
   printExtensions(L).

/*========================================================================
   Extensions by Tibor szécsényi

   sublist(X,Y)		Az X lista elemei (ugyanolyan sorrendben) 
					megtaláhatóak-e Y-ban?
========================================================================*/

sublist([],_).
sublist([F|L1],[F|L2]):-
	sublist(L1,L2).
sublist([F1|L1],[F2|L2]):-
	F1\==F2,
	sublist([F1|L1],L2).


/*========================================================================
	subset(X,Y) : X < Y
========================================================================*/

subset([X|R],S) :- member(X,S), subset(R,S).
subset([],_).


/*========================================================================
	Descartes-szorzat
========================================================================*/


cartesianProduct(A,B,AxB):-
	findall([X,Y],(memberList(X,A),memberList(Y,B)),AxB).


/*========================================================================
	Descartes-szorzat : rendezett hármasokat csinál
========================================================================*/

cartesianProduct2(A,BxC,AxBxC):-
	findall([X|Y],(memberList(X,A),memberList(Y,BxC)),AxBxC).


/*========================================================================
	cartesianUnion : AxuB := { XuY | X eleme A-nak & Y eleme B-nek }
========================================================================*/

cartesianUnion(A,B,AxuB):-
	cartesianProduct(A,B,AxB),
	findall(X,(memberList([X1,X2],AxB),unionSets(X1,X2,X)),AxuB).


/*========================================================================
	cartesianIntersection : AxuB := { XuY | X eleme A-nak & Y eleme B-nek }
========================================================================*/

cartesianIntersection(A,B,AxiB):-
	cartesianProduct(A,B,AxB),
	findall(X,(memberList([X1,X2],AxB),intersectionSets(X1,X2,X)),AxiB).


/*========================================================================
	findAllOrderedN(A,N,AadN) : A,4 -> AxAxAxA
========================================================================*/

findAllOrderedN(A,2,ON):-
	cartesianProduct(A,A,ON).

findAllOrderedN(A,N,ON):-
	N > 2,
	N1 is N-1,
	findAllOrderedN(A,N1,ON1),
	cartesianProduct2(A,ON1,ON).


/*========================================================================
	diffList(L1,L2,L3) : L3 = L2\L1
========================================================================*/

diffList([],L,L).
diffList([X|Y],L1,L2):-
	selectFromList(X,L1,L3),
	diffList(Y,L3,L2).

massUnion([],[]).
massUnion([X|Y],Z):-
	massUnion(Y,Z1),
	unionSets(X,Z1,Z).

massIntersection([X],X).
massIntersection([X|Y],Z):-
	massIntersection(Y,Z1),
	findall(Q,(memberList(Q,X),memberList(Q,Z1)),Z).

/*========================================================================
	maxiSet(H,K) : H,K - set of sets; K<H & X,Y eK: X\<Y  
========================================================================*/

maxiSet([],[[]]).

maxiSet(H,K):-
	setof(X,(memberList(X,H),
		setof(Y,(memberList(Y,H),subset(X,Y)),[X])),
		K).

intersectionSets(H,K,HiK):-
	setof(X,(memberList(X,H),memberList(X,K)),HiK).

/*========================================================================
	splitSettoN(Set,N,Sets)
	
	splitSettoN(Set,3,[Sub1,Sub2,Sub3]) 
		: Sub1 U Sub2 U Sub3 = Set
		: Sub1 ^ Sub2 = [] ...
========================================================================*/

splitSettoN(Set,1,[Set]):-!.
splitSettoN(Set,N,[Sub|Sets]):-
	sublist(Sub,Set),
	diffList(Sub,Set,Sub2),
	N2 is N - 1,
	splitSettoN(Sub2,N2,Sets).


/*========================================================================
	splitSettoN(Set,N,Sets,L)
	
	splitSettoN(Set,3,[Sub1,Sub2,Sub3],1) 
		: Sub1 U Sub2 U Sub3 = Set
		: Sub1 ^ Sub2 = [] ...
		: |Sub1| >= 1 ...
========================================================================*/

splitSettoN(Set,1,[Set],_):-!.
splitSettoN(Set,N,[Sub|Sets],L):-
	sublist(Sub,Set),
	length(Sub,SubL),
	SubL >= L,
	diffList(Sub,Set,Sub2),
	N2 is N - 1,
	splitSettoN(Sub2,N2,Sets,L).

