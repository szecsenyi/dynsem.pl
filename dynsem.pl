/* 
dinamikus model: model(ModelID,D,Preds,Names,I).
	ModelID: ID
	D: [a,b,c,d]
	Preds: [[name1,0],[pred1,1],[pred2,2]]
	Names: [f(0,name1,a), ...]
	I: [index(1,i1,[f(2,pred2,[[a,b],[c,d]]), ...]), index(2,i2,[f(2,pred2,[[a,a],[b,b]]), ...]), ...]
 */

:- module(dynsem,[
			actualModelName/1,
			changeActualModel/1,
			changeEchoStatus/0,
			createModel/1, createModel/3, createModel/4,
			dropModel/1,
			combineModels/3,
			and/1, and/2,
			eval/3, eval/2,
			addNewName/3, addNewName/2,
			addNewPredicate/4, addNewPredicate/3, addNewPredicate/2,
			addNewOrdering/3, addNewOrdering/2, addNewOrdering/1,
			addNewClassifier/4, addNewClassifier/3, addNewClassifier/2,
			writeWorlds/1, writeWorlds/0,
			writeWorldNumber/1, writeWorldNumber/0,
			writeWorldsHTML/0,
			writeWorld/2, writeWorld/1,
			writeVocabulary/1, writeVocabulary/0
			]).


:- use_module(comsemPredicates,[
			memberList/2,
			appendLists/3,
			sublist/2,
			subset/2,
			selectFromList/3,
			unionSets/3,
			splitSettoN/3,splitSettoN/4
			]).

%:- use_module(inqsemModel,[exmodel/4]).

:- use_module(modelChecker3,[evaluate/3,lambdaX/4]). 

:- dynamic actualModelName/1.
:- dynamic actualModel/2.
% :- dynamic actualOrderedNs/3.
:- dynamic echoWorldNumber/1.

/*========================================================================
   Info
========================================================================*/

info:-
   format('~n> ------------------------------------------------------------------------- <',[]),
   format('~n> dynsem.pl,                                                                <',[]),
   format('~n>                                                                           <',[]),
   format('~n> ?- createModel(Name).          - a megadott nevû modell betöltése         <',[]),
   format('~n> ?- createModel(Name,D,N).      - a megadott nevû modell létrehozása       <',[]),
   format('~n>               D : az individuumok halmaza, pl: [j,p,r]                    <',[]),
   format('~n>               N : nevek és denotátumok, pl: [[john,j],[paul,p],[ringo,r]] <',[]),
   format('~n> ?- dropModel(Name).            - a megadott nevû modell törlése           <',[]),
   format('~n> ?- combineModels(Name1, Name2, Name3). - két model kombinálása egy újba   <',[]),
   format('~n> ?- addNewName(model:ModelName?,Name,Ind).                                 <',[]),
   format('~n>               - új név bevezetése a megadott denotátummal                 <',[]),
   format('~n> ?- addNewPredicate(model:ModelName?,PredName,N,Scope?).                   <',[]),
   format('~n>               - új n-argumentumú predikátum bevezetése a modellbe         <',[]),
   format('~n> ?- addNewOrdering(model:ModelName?,PredName,Scope?).                      <',[]),
   format('~n>               - új lineáris rendezés bevezetése a modellbe                <',[]),
   format('~n> ?- addNewClassifier(model:ModelName?,RelName,ClassNamesScope?).           <',[]),
   format('~n>               - új ekvivalenciareláció bevezetése a modellbe              <',[]),
   format('~n>                 az ekvivalenciaosztályok megadásával                      <',[]),
   format('~n>                 pl: addNewClassifier(gender,[f,m]).                    <',[]),
   format('~n> ?- and(model:ModelName?,Formula).                                         <',[]),
   format('~n>               - formula kiértékelése a modellben, a modell szûkítése      <',[]),
   format('~n> ?- eval(model:ModelName?,Formula,Ind).                                    <',[]),
   format('~n>               - formula kiértékelése a modellben                          <',[]),
   format('~n>                                                                           <',[]),
   format('~n> ?- writeWorlds(ModelName?).      - az érvényes világok kiíratása          <',[]),
   format('~n> ?- writeWorldNumber(ModelName?). - az érvényes világok számának kiíratása <',[]),
   format('~n> ?- writeVocabulary(ModelName?).  - a használható predikátumok és nevek    <',[]),
   format('~n>                                                                           <',[]),
   format('~n> ?- info.                   - prints this information                      <',[]),
   format('~n> ------------------------------------------------------------------------- <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.

/********************************************************
* A dinamikus predikátumok alapértelmezett beállításai
********************************************************/
echoWorldNumber(on).

/********************************************************
* A környezeti változók változtatása
********************************************************/

changeActualModel(ModelName):-
	actualModel(ModelName,_),
	retractall(actualModelName(_)),
	assert(actualModelName(ModelName)).

changeEchoStatus:-
	echoWorldNumber(on),
	retract(echoWorldNumber(on)),
	assert(echoWorldNumber(off)),
	write('Echo is off now'), nl, !.

changeEchoStatus:-
	echoWorldNumber(off),
	retract(echoWorldNumber(off)),
	assert(echoWorldNumber(on)),
	write('Echo is on now'), nl, !.
	
/**********************************
* Modell létrehozása
**********************************/

% megadott nevû modell beolvasása a modellfájlból 
% createModel(ModelName):-
	% exmodel(ModelName,D,P,N),
	% createModel(ModelName,D,P,N).

% createModel(ModelName, D, N)
%  D : az individuumok halmaza, pl: [j,p,r]
%  N : nevek és denotátumok, pl: [[john,j],[paul,p],[ringo,r]]
createModel(ModelName,D,N):-
	createPredsAndNames(D,N,P,N1),
	createModel(ModelName,D,P,N1).

createModel(ModelName,D,P,N):-
	buildModel(ModelName,D,P,N), % kiszámolja az I lehetséges világokat
	tryRetract(actualModelName(_)),
	assert(actualModelName(ModelName)), % Ez lesz az új aktív modell
	writeWorldNumberIfMust(ModelName),!.

% createPredsAndNames(D,N0,P,N)
% A D individuumhalmaz elemeibõl és az N0 név-ind rendezett párokból
% modellben értelmezhetõ neveket és predikátumokat készít: N és P
createPredsAndNames([],[],[],[]).
createPredsAndNames([],[[Name,Ind]|T1], [[Name,0]|T2], [f(0,Name,Ind)|T3]):-
	createPredsAndNames(T1,T2,T3).
createPredsAndNames([D|Ds],N,[[D,0]|T2],[f(0,D,D)|T3]):-
	createPredsAndNames(Ds,N,T2,T3).

% dropModel: felejtsük el ezt a modellt - felszabadítja a memóriát 
dropModel(ModelName):-
	tryRetract(actualModel(ModelName,_)).

% combineModels: két létezõ modell kombinációját veszi
combineModels(Name1, Name2, Name3):-
	actualModel(Name1,model(Name1,D1,P1,N1,I1)),
	actualModel(Name2,model(Name2,D2,P2,N2,I2)),
	% ellenõrizzük, hogy P1 és P2 nem tartalmaz azonos 1-2-3 argumentumú predikátumot
	findall(Pred,(memberList([Pred,1],P1),memberList([Pred,1],P2)),[]),
	findall(Pred,(memberList([Pred,2],P1),memberList([Pred,2],P2)),[]),
	findall(Pred,(memberList([Pred,2],P1),memberList([Pred,2],P2)),[]),
	% a mindkét modellben szereplõ neveknek ugyanolyan referenciájúaknak kell lenni
	findall(
		Name,
		(	memberList(f(0,Name,X),N1),
			memberList(f(0,Name,Y),N2),
			X \== Y),
		[]),
	% ha megfelelnek a feltételnek, egyesíthetünk
	unionSets(P1,P2,P3),
	unionSets(D1,D2,D3),
	unionSets(N1,N2,N3), 
	combineWorlds(I1,I2,I3),
	tryRetract(actualModel(Name3,_)),
	assert(actualModel(Name3, model(Name3,D3,P3,N3,I3))),
	writeWorldNumberIfMust(Name3).

/* combineWorlds(IndexIn1, IndexIn2, IndexOut).
	a két indexhalmaz descartes-unióját adja vissza */
combineWorlds(IndexIn1,IndexIn2,IndexOut):-
	findall(
		index(_,_,L), 
		(memberList(index(_,_,F1),IndexIn1), memberList(index(_,_,F2),IndexIn2), appendLists(F1,F2,L)), 
		IndexTMP),
	reIndex(IndexTMP, IndexOut,0).

reIndex([],[],_).
reIndex([index(_,_,Fs)|RestIndexIn],[index(Counter,ID,Fs)|RestIndexOut],Counter):-
	newID(ID,Counter),
	C2 is Counter + 1,
	reIndex(RestIndexIn, RestIndexOut, C2).

/*******************************************************
* A már meglevõ modellek változtatása
*   - addNewName: új név hozzáadása
*   - addNewPredicate: új predikátum hozzáadása
*   - addNewClassifier: új osztályozó hozzáadása (reláció és osztályok)
*   - addNewOrdering: új lineáris rendezés hozzáadása
*******************************************************/

addNewName(NewName,Ind):-
	actualModelName(ModelName),
	addNewName(model:ModelName,NewName,Ind).

addNewName(model:ModelName,NewName,Ind):-
	isThereSuchModel(ModelName),
	actualModel(ModelName,model(ModelName,D,P,N,I)),
	\+ memberList([NewName,0],P), % nincs még ilyen név
	memberList(Ind,D),
	retract(actualModel(ModelName,_)),
	assert(actualModel(ModelName,model(ModelName,D,[[NewName,0]|P],[f(0,NewName,Ind)|N],I))),
	writeWorldNumberIfMust(ModelName).

%%%%%%%%%%%%%%%
addNewPredicate(PredName,ArgNum):-
	actualModelName(ModelName),
	actualModel(ModelName,model(_,D,_,_,_)),
	addNewPredicate(model:ModelName,PredName,ArgNum,D).
	
addNewPredicate(model:ModelName,PredName,ArgNum):-
	actualModel(ModelName,model(_,D,_,_,_)),
	addNewPredicate(model:ModelName,PredName,ArgNum,D).

addNewPredicate(PredName,ArgNum,Scope):-
	atom(PredName),
	actualModelName(ModelName),
	addNewPredicate(model:ModelName,PredName,ArgNum,Scope).

% Scope: a predikátum ezen individummhalmaz elemein értelmezett
addNewPredicate(model:ModelName,PredName,ArgNum,Scope):-
	isThereSuchModel(ModelName),
	integer(ArgNum),
	ArgNum > 0,
	ArgNum < 4,
	actualModel(ModelName,Model),
	Model = model(ModelName,D,P,N,_),
	\+memberList([PredName,ArgNum],P),
	scope(ArgNum,Model,Scope,Scope2),
	predicates_1(Model,ArgNum,PredName,Scope2,I,0,_),
	retract(actualModel(ModelName,_)),
	assert(actualModel(ModelName,model(ModelName,D,[[PredName,ArgNum]|P],N,I))),
	writeWorldNumberIfMust(ModelName).

predicates_1(model(_,_,_,_,[]),_,_,_,[],Idnum,Idnum).
predicates_1(model(ModelName,D,P,N,[I|RestI]),ArgNum,PredName,Scope,Indices,IdnumIn,IdnumOut):-
	predicates_1(model(ModelName,D,P,N,RestI),ArgNum,PredName,Scope,I0,IdnumIn,IdnumTMP),
	scope2(ArgNum,model(ModelName,D,P,N,[I]),Scope,Scope2),
	findAllPossPredMean(Scope2,ArgNum,PredName,PredMeans),
	addNewIndices(I,I0,Indices,PredMeans,IdnumTMP,IdnumOut).

addNewIndices(_,I,I,[],Idnum,Idnum).
addNewIndices(index(_,_,Fs),Iin,[index(Idnum,ID,[Pred|Fs])|Iout],[Pred|Preds],Idnum,IdnumOut):-
	IdnumTMP is Idnum + 1,
	newID(ID, Idnum),
	addNewIndices(index(_,_,Fs),Iin,Iout,Preds,IdnumTMP,IdnumOut).
	
%%%%%%%%%%%%%%%%%%
addNewOrdering(PredName):-
	actualModelName(ModelName),
	actualModel(model(ModelName,D,_,_,_)),
	addNewOrdering(model:ModelName,PredName,D).

addNewOrdering(model:ModelName, PredName):-
	actualModel(ModelName,model(ModelName,D,_,_,_)),
	addNewOrdering(model:ModelName,PredName,D).

addNewOrdering(PredName, Scope):-
	atom(PredName),
	actualModelName(ModelName),
	addNewOrdering(model:ModelName,PredName,Scope).

addNewOrdering(model:ModelName,PredName,Scope):-
	isThereSuchModel(ModelName),
	actualModel(ModelName,Model),
	Model = model(ModelName,D,P,N,_),
	\+memberList([PredName,2],P),
	scope(1,Model,Scope,Scope2),
	orderings_1(Model,PredName,Scope2,I,0,_),
	retract(actualModel(ModelName,_)),
	assert(actualModel(ModelName,model(ModelName,D,[[PredName,2]|P],N,I))),
	writeWorldNumberIfMust(ModelName),!.

orderings_1(model(_,_,_,_,[]),_,_,[],Idnum,Idnum).
orderings_1(model(ModelName,D,P,N,[I|RestI]),PredName,Scope,Indices,IdNumIn,IdNumOut):-
	orderings_1(model(ModelName,D,P,N,RestI),PredName,Scope,I0,IdNumIn,IdNumTMP),
	scope2(1,model(ModelName,D,P,N,[I]),Scope,[Scope2]),
	findall(f(2,PredName,L),possOrdering(Scope2,L),PredMeans),
	addNewIndices(I,I0,Indices,PredMeans,IdNumTMP,IdNumOut).

%%%%%%%%%%%%%%%%%%%%
addNewClassifier(RelName,ClassNames):-
	actualModelName(ModelName),
	actualModel(ModelName,model(ModelName,D,_,_,_)),
	addNewClassifier(model:ModelName,RelName,ClassNames,D).

addNewClassifier(model:ModelName,RelName,ClassNames):-
	actualModel(ModelName,model(ModelName,D,_,_,_)),
	addNewClassifier(model:ModelName,RelName,ClassNames,D).

addNewClassifier(RelName,ClassNames,Scope):-
	atom(RelName),
	actualModelName(ModelName),
	addNewClassifier(model:ModelName,RelName,ClassNames,Scope).

addNewClassifier(model:ModelName,RelName,ClassNames,Scope):-
	isThereSuchModel(ModelName),
	actualModel(ModelName,Model),
	Model = model(ModelName,D,P,N,_),
	scope(1,Model,Scope,Scope2),
	classifiers_1(Model,Scope2,RelName,ClassNames,I,0,_),
	classifiers_5(P,P2,ClassNames),
	retract(actualModel(ModelName,_)),
	assert(actualModel(ModelName,model(ModelName,D,[[RelName,2]|P2],N,I))),
	writeWorldNumberIfMust(ModelName),!.
	
classifiers_1(model(_,_,_,_,[]),_,_,_,[],Idnum,Idnum).	
classifiers_1(model(ModelName,D,P,N,[I|RestI]),Scope,RelName,ClassNames,Indices,IdNumIn,IdNumOut):-
	classifiers_1(model(ModelName,D,P,N,RestI),Scope,RelName,ClassNames,I0,IdNumIn,IdNumTMP),
	length(ClassNames,ClassNum),
	scope2(1,model(ModelName,D,P,N,[I]),Scope,[Scope2]),
	findall(Classes,splitSettoN(Scope2,ClassNum,Classes),AllClasses),
	classifiers_2(I0,Indices,I,AllClasses,RelName,ClassNames,IdNumTMP,IdNumOut).

classifiers_2(Is,Is,_,[],_,_,IdNum,IdNum).
classifiers_2(IndicesIn,IndicesOut,I,[Classes|RestClasses],RelName,ClassNames,IdNumIn,IdNumOut):-
	classifiers_2(IndicesIn,IndicesTMP,I,RestClasses,RelName,ClassNames,IdNumIn,IdNumTMP),
	I = index(_,_,F0),
	classifiers_3(F0,F1,Classes,ClassNames),
	classifiers_4(F1,F2,Classes,RelName,[]),
	IdNumOut is IdNumTMP + 1,
	newID(ID,IdNumOut),
	IndicesOut = [index(IdNumOut,ID,F2)|IndicesTMP].

classifiers_3(Fs,Fs,[],[]).
classifiers_3(FIn,FOut,[Class|Classes],[Name|Names]):-
	classifiers_3(FIn,FTMP,Classes,Names),
	FOut = [f(1,Name,Class)|FTMP].

classifiers_4(Fs,[f(2,RelName,Tuples)|Fs],[],RelName,Tuples).
classifiers_4(FIn,FOut,[Class|Classes],RelName,T0):-
	findall([X,Y],(memberList(X,Class),memberList(Y,Class)),T1),
	append(T0,T1,Tuples),
	classifiers_4(FIn,FOut,Classes,RelName,Tuples).

classifiers_5(P,P,[]).
classifiers_5(P,[[ClassName,2]|P2],[ClassName|ClassNames]):-
	classifiers_5(P,P2,ClassNames).

/*************************************************************
* A modell szûkítése egy új állítás megadásával
*************************************************************/

and(Formula):-
	actualModelName(ModelName),
	and(ModelName,Formula).

and(ModelName,Formula):-
	actualModel(ModelName,model(Name,D,P,N,I)),
	evaluate(Formula,model(Name,D,P,N,I),I2),
	retract(actualModel(ModelName,_)),
	assert(actualModel(ModelName,model(Name,D,P,N,I2))),
	writeWorldNumberIfMust(ModelName).

eval(Formula,Ind):-
	actualModelName(ModelName),
	eval(ModelName,Formula,Ind).
	
eval(ModelName,Formula,Ind):-
	actualModel(ModelName,Model),
	evaluate(Formula,Model,Ind).

/*************************************************
* segédpredikátumok
*************************************************/

scope(1,_,l_x(X,Form),[l_x(X,Form)]):-
	!.

scope(1,model(_,D,_,_,_),Scope,[Scope]):-
	subset(Scope,D).

scope(2,Model,[S1In,S2In],[S1Out,S2Out]):-
	scope(1,Model,S1In,[S1Out]),
	scope(1,Model,S2In,[S2Out]),
	!.
scope(2,Model,ScopeIn,[ScopeOut,ScopeOut]):-
	scope(1,Model,ScopeIn,[ScopeOut]).

scope(3,Model,[S1In,S2In,S3In],[S1Out,S2Out,S3Out]):-
	scope(1,Model,S1In,[S1Out]),
	scope(1,Model,S2In,[S2Out]),
	scope(1,Model,S3In,[S3Out]),
	!.
scope(3,Model,ScopeIn,[ScopeOut,ScopeOut,ScopeOut]):-
	scope(1,Model,ScopeIn,[ScopeOut]).

scope2(1,Model,[l_x(X,Form)],[ScopeOut]):-
	lambdaX(X,Form,Model,ScopeOut),
	!.
scope2(1,_,Scope,Scope).

scope2(2,Model,[S1In,S2In],[S1Out,S2Out]):-
	scope2(1,Model,[S1In],[S1Out]),
	scope2(1,Model,[S2In],[S2Out]).

scope2(3,Model,[S1In,S2In,S3In],[S1Out,S2Out,S3Out]):-
	scope2(1,Model,[S1In],[S1Out]),
	scope2(1,Model,[S2In],[S2Out]),
	scope2(1,Model,[S3In],[S3Out]).

% Ellenõrzi, hogy van-e ilyen nevû modell
isThereSuchModel(ModelName):- 
	actualModel(ModelName,_),!. % van
isThereSuchModel(_):-
	false. % nincs

newID(NewID, Idnum):-
	atom_chars(Idnum,IdnumList),
	atom_chars(NewID,[i|IdnumList]).

findAllPossPredMean([Scope],1,PredName,PredMeans):-
	findall(f(1,PredName,L),sublist(L,Scope),PredMeans).
findAllPossPredMean([Scope1,Scope2],2,PredName,PredMeans):-
	tuples(Scope1,Scope2,Tuples),
	findall(f(2,PredName,L),sublist(L,Tuples),PredMeans).
findAllPossPredMean([Scope1,Scope2,Scope3],3,PredName,PredMeans):-
	triples(Scope1,Scope2,Scope3,Triples),
	findall(f(3,PredName,L),sublist(L,Triples),PredMeans).

/* possOrdering: megadja egy lineáris rendezését D-nek */
possOrdering([],[]).
possOrdering(D,List):-
	selectFromList(X,D,D2),
	findall([X,Y],memberList(Y,D2),ListTMP1),
	possOrdering(D2,ListTMP2),
	appendLists(ListTMP1,ListTMP2,List).

/* buildModel: a modell összes predikátumához hozzárendel 
 * egy interpretárciót az összes lehetséges módon:
 * létrehozza az I lehetséges világokat */
buildModel(ModelName,D,[],N):-
	\+ actualModel(ModelName,_),
	assert(actualModel(ModelName,model(ModelName,D,[[ModelName,1]],N,[index(0,i0,[f(1,ModelName,D)])]))).
buildModel(ModelName,D,[[_,0]|Preds],N):-
	buildModel(ModelName,D,Preds,N).
buildModel(ModelName,D,[[PredName,ArgNum]|Preds],N):-
	ArgNum\==0,
	buildModel(ModelName,D,Preds,N),
	addNewPredicate(model:ModelName,PredName,ArgNum,D).

tryRetract(X):-
	retractall(X),!.

tryRetract(_).

tuples(D,Tuples):-
	findall([X,Y],(memberList(X,D),memberList(Y,D)),Tuples).
tuples(D1,D2,Tuples):-
	findall([X,Y],(memberList(X,D1),memberList(Y,D2)),Tuples).

triples(D,Triples):-
	findall([X,Y,Z],(memberList(X,D),memberList(Y,D),memberList(Z,D)),Triples).
triples(D1,D2,D3,Triples):-
	findall([X,Y,Z],(memberList(X,D1),memberList(Y,D2),memberList(Z,D3)),Triples).
	
/************************************************************
* ezek itten a lekérdezést, képernyõre kiírást csinálják - javítani
************************************************************/

writeList([]).

writeList([H|T]):-
	writeList(T),
	write(H),nl.

writeWorlds:-
	actualModelName(ModelName),
	writeWorlds(ModelName).

writeWorlds(ModelName):-
	actualModel(ModelName,model(_,_,_,Names,I)),
	length(I,N),
	writeWorldNumber_(ModelName,N),
	writeNames_(Names),
	writeWorlds_(I).

writeWorldNumberIfMust(_):-
	echoWorldNumber(off),!.

writeWorldNumberIfMust(ModelName):-
	echoWorldNumber(on),!,
	writeWorldNumber(ModelName).

writeWorldNumber:-
	actualModelName(ModelName),
	writeWorldNumber(ModelName).

writeWorldNumber(ModelName):-
	actualModel(ModelName,model(_,_,_,_,I)),
	length(I,N),
	writeWorldNumber_(ModelName, N).
	
writeWorldNumber_(ModelName,0):-
	write('There is no any world in model '), write(ModelName), write(', sorry.'), nl,!.
writeWorldNumber_(ModelName,1):-
	write('There is only 1 world in model '), write(ModelName), write('.'), nl,!.
writeWorldNumber_(ModelName,N):-
	write('There are '),write(N),write(' worlds in model '), write(ModelName), write('.'), nl.

writeWorlds_([]):-!.

writeWorlds_([index(_,I,Preds)|T]):-
	writeWorlds_(T),
	write('index('),
	write(I),
	write('): '),
	writePreds(Preds),
	nl.

writeWorld(ID):-
	actualModelName(ModelName),
	writeWorld(ModelName,ID).

writeWorld(ModelName,ID):-
	actualModel(ModelName,model(_,_,_,_,I)),
	memberList(index(_,ID,Preds),I),
	write(ID),
	write(': {'),
	write(Preds),
	write('}'),
	nl.

writePreds([]).

writePreds([f(_,P,D)|T]):-
	write(P),
	write('='),
	write(D),
	write(', '),
	writePreds(T).

writeVocabulary:-
	actualModelName(ModelName),
	writeVocabulary(ModelName).

writeVocabulary(ModelName):-
	actualModel(ModelName,model(_,_,P,_,_)),
	writeList(P).

writeNames_([]).

writeNames_([f(0,Name,Ind)|Names]):-
	write(Name), write(' --> '), write(Ind), nl,
	writeNames_(Names).
	

/*******************************
* HTML output NEM MÛKÖDIK!!!
*******************************/
writeWorldsHTML:-
	actualModel(_,model(_,_,_,_,I)),
	length(I,Il),
	length(PU,PUl),
	write('There are '),write(Il),write(' worlds, in which '),write(PUl),writeln(' are valid.'),
	write('<table border="1">'),
	writeWorldsHTML(I,PU),
	write('</table>'),
	nl.

writeWorldsHTML([],_).

writeWorldsHTML([index(_,I,Preds)|T],P):-
	writeWorldsHTML(T,P),
	whatColor(I,P,Color),
	write('<tr '),
	write(Color),
	write('><td>index: <a name="'),
	write(I),
	write('">'),
	write(I),
	write('</a></td><td>'),
	writePredsHTML(Preds),
	write('</td></tr>').

whatColor(I,P,'bgcolor="#FAEBD7"'):-
	memberList(I,P),!.

whatColor(_,_,'bgcolor="#FF9999"').

writePredsHTML([]).

writePredsHTML([f(_,P,D)|T]):-
	write(P),
	write('='),
	write(D),
	write('<br>'),
	writePredsHTML(T).

writePossibilitiesHTML:-
	actualPossibilities(P),
	length(P,PLength),
	writeNumberOfPossibilities(PLength),
	writePossibilitiesHTML(P),
	nl.

writePossibilitiesHTML([]).

writePossibilitiesHTML([P|R]):-
	write('{'),
	writeListHTML(P),
	write('}<br>'),
	writePossibilitiesHTML(R).

writeListHTML([]).

writeListHTML([H|T]):-
	writeListHTML(T),
	write('<a href="#'),
	write(H),
	write('">'),
	write(H),
	write('</a>, ').

:-createModel(null,[a],[]).
