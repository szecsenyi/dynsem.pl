/* 
dinamikus model: model(ModelID,D,Preds,Names,I).
	ModelID: ID
	D: [a,b,c,d]
	Preds: [[name1,0],[pred1,1],[pred2,2]]
	Names: [f(0,name1,a), ...]
	I: [index(1,i1,[f(2,pred2,[[a,b],[c,d]]), ...]), index(2,i2,[f(2,pred2,[[a,a],[b,b]]), ...]), ...]
 */

:- op(1105, xfy, &).
:- op(900, fy, ~).
:- op(500, fy, some).
:- op(500, fy, all).
:- op(500, fy, iota).
:- op(600, xfy, :).
:- op(1105, xfy, ->).

:- use_module(comsemPredicates,[
			memberList/2,
			appendLists/3,
			sublist/2,
			subset/2,
			selectFromList/3,
			unionSets/3,
			splitSettoN/3,splitSettoN/4
			]).

:- use_module(modelChecker3,[evaluate/3,lambdaX/4]). 

:- consult(library(sgml)).

:- use_module(model2xml, [model2xml/2, xml2model/2]).

:- use_module(model2tsv, [saveModelTsv_/2, loadModelTsv_/2]).

:- dynamic actualModelName/1.
:- dynamic actualModel/2.
:- dynamic echoWorldNumber/1.
:- dynamic displayMessages/1, logFileName/1.
:- dynamic messages/1.

/*========================================================================
   Info
========================================================================*/

info:-
	format('~n> -------------------------------------------------------------------------------------- <',[]),
	format('~n> dynsem.pl,                                                                             <',[]),
	format('~n>                                                                                        <',[]),
	format('~n> ?- createModel(ModelName,D,N).                                                         <',[]),
	format('~n>                         - új modell létrehozása az individuumok és a nevek megadásával <',[]),
	format('~n> ?- changeActualModel(ModelName).          - váltás a megadott nevű modellre            <',[]),
	format('~n> ?- dropModel(ModelName?).                 - a megadott nevű modell törlése             <',[]),
	format('~n> ?- combineModels(Name1, Name2, Name3).    - két modell kombinálása Name3-ként          <',[]),
	format('~n> ?- copyModel(ModelName1?,ModelName2).     - modell másolása új néven                   <',[]),
	format('~n> ?- renameModel(ModelName1?,ModelName2).   - modell átnevezése                          <',[]),
	format('~n> ?- models.                                - kilistázza a modellek neveit               <',[]),
	format('~n>                                                                                        <',[]),
	format('~n> ?- addNewName(Name,Ind).                  - új név hozzáadása a modellhez              <',[]),
	format('~n> ?- addNewPredicate(PredName,N,Scope?).    - új n-argumentumú predikátum bevezetése     <',[]),
	format('~n> ?- addNewOrdering(RelName,Scope?).        - új lineáris rendezés bevezetése            <',[]),
	format('~n> ?- addNewClassifier(RelName,ClassNames,Scope?,noempty?).                               <',[]),
	format('~n>                                           - új ekvivalenciareláció bevezetése          <',[]),
	format('~n> ?- deletePredicate(PredName).             - predikátum törlése az aktuális modellből   <',[]),
	format('~n> ?- renamePredicate(OldName,NewName).      - predikátum átnevezése                      <',[]),
	format('~n>                                                                                        <',[]),
	format('~n> ?- eval(Formula,-Ind).                    - formula kiértékelése                       <',[]),
	format('~n> ?- writeIndices(Ind).                     - a kiértékelés eredményének megjelenítése   <',[]),
	format('~n> ?- and(Formula).                        - a modell szűkítése a formula kiértékelésével <',[]),
	format('~n>                                                                                        <',[]),
	format('~n> ?- writeWorld(ModelName?,Ind).            - az Ind azonosítójú világ megjelenítése     <',[]),
	format('~n> ?- writeWorlds(ModelName?).               - az érvényes világok kilistázása            <',[]),
	format('~n> ?- writeWorldNumber(ModelName?).          - az érvényes világok számának kiíratása     <',[]),
	format('~n> ?- writeVocabulary(ModelName?).           - a használható predikátumok és nevek        <',[]),
	format('~n>                                                                                        <',[]),
	format('~n> ?- saveModel(FileName,ModelName?).        - modell lemezre írása prolog predikátumként <',[]),
	format('~n> ?- loadModel(FileName).              - modell beolvasása prolog predikátum formátumból <',[]),
	format('~n> ?- saveModelXml(FileName,ModelName?).     - modell lemezre írása XML formátumban       <',[]),
	format('~n> ?- loadModelXml(FileName).                - modell beolvasása XML formátumból          <',[]),
	format('~n> ?- saveModelTsv(FileName,ModelName?).     - modell lemezre írása TSV formátumban       <',[]),
	format('~n> ?- loadModelTsv(FileName).                - modell beolvasása TSV formátumból          <',[]),
	format('~n>                                                                                        <',[]),
	format('~n> ?- info.                                  - prints this information                    <',[]),
	format('~n> -------------------------------------------------------------------------------------- <',[]),
	format('~n~n',[]).

:- encoding(utf8).
% :- info.

/********************************************************
* A dinamikus predikátumok alapértelmezett beállításai
********************************************************/
echoWorldNumber(on).
messages([]).
displayMessages(on).
logFileName('').

/********************************************************
* A környezeti változók változtatása
********************************************************/

changeActualModel(ModelName):-
	actualModel(ModelName,_),
	retractall(actualModelName(_)),
	assert(actualModelName(ModelName)),
	addMessage(['Actual model is ',ModelName,'.']).

silent:-
	retract(echoWorldNumber(_)),
	assert(echoWorldNumber(off)),
	retract(displayMessages(_)),
	assert(displayMessages(off)).

logFile(LogFileName):-
	retract(logFileName(_)),
	assert(logFileName(LogFileName)).
	
/**********************************
* Modell létrehozása
**********************************/

createModel(ModelName,D,N):-
	createPredsAndNames(D,N,P,N1),
	createModel(ModelName,D,P,N1).

createModel(ModelName,D,P,N):-
	buildModel(ModelName,D,P,N), % kiszámolja az I lehetséges világokat
	tryRetract(actualModelName(_)),
	assert(actualModelName(ModelName)), % Ez lesz az új aktív modell
	addMessage(['Model ',ModelName,' has been created.']),
	writeWorldNumberIfMust(ModelName),!.

% createPredsAndNames(D,N0,P,N)
% A D individuumhalmaz elemeiből és az N0 név-ind rendezett párokból
% modellben értelmezhető neveket és predikátumokat készít: N és P
createPredsAndNames([],[],[],[]).
createPredsAndNames([],[[Name,Ind]|T1], [[Name,0]|T2], [f(0,Name,Ind)|T3]):-
	createPredsAndNames(T1,T2,T3).
createPredsAndNames([D|Ds],N,[[D,0]|T2],[f(0,D,D)|T3]):-
	createPredsAndNames(Ds,N,T2,T3).

% dropModel: felejtsük el ezt a modellt - felszabadítja a memóriát 
dropModel(ModelName):-
	tryRetract(actualModel(ModelName,_)),
	addMessage(['Model ',ModelName,' has been dropped.']),
	chooseNewActModel(ModelName).

chooseNewActModel(ModelName):-
	actualModelName(AMD),
	AMD \== ModelName,
	!.
chooseNewActModel(_):-
	actualModel(ModelName,_), !,
	selectModel(ModelName).
chooseNewActModel(_):-
	retract(actualModelName(_)).

% combineModels: két létező modell kombinációját veszi
combineModels(Name1, Name2, Name3):-
	actualModel(Name1,model(Name1,D1,P1,N1,I1)),
	actualModel(Name2,model(Name2,D2,P2,N2,I2)),
	% ellenőrizzük, hogy P1 és P2 nem tartalmaz azonos 1-2-3 argumentumú predikátumot
	findall(Pred,(memberList([Pred,1],P1),memberList([Pred,1],P2)),[]),
	findall(Pred,(memberList([Pred,2],P1),memberList([Pred,2],P2)),[]),
	findall(Pred,(memberList([Pred,2],P1),memberList([Pred,2],P2)),[]),
	% a mindkét modellben szereplő neveknek ugyanolyan referenciájúaknak kell lenni
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
	addUniverse(f(1,Name3,D3),I3,I4),
	tryRetract(actualModel(Name3,_)),
	assert(actualModel(Name3, model(Name3,D3,[[Name3,1]|P3],N3,I4))),
	addMessage(['Model ',Name1,' and ',Name2,' is combined to model ',Name3,'.']),
	changeActualModel(Name3),
	writeWorldNumberIfMust(Name3).
	
addUniverse(f(ArgNum,Name,D),I,I2):-
	I = [index(_,_,F)|_],
	memberList(f(ArgNum,Name,_),F),!,
	addUniverse2(f(ArgNum,Name,D),I,I2).
addUniverse(f(ArgNum,Name,D),I,I2):-
	addUniverse1(f(ArgNum,Name,D),I,I2).

addUniverse1(_,[],[]).
addUniverse1(U,[index(N,ID,Preds)|R],[index(N,ID,[U|Preds])|R1]):-
	addUniverse(U,R,R1).
addUniverse2(_,[],[]).
addUniverse2(U,[I,R],[I1|R1]):-
	addUniverse3(U,I,I1),
	addUniverse2(U,R,R1).
addUniverse3(_,[],[]).
addUniverse3(f(N,P,D),[f(N1,P1,D1)|R1],[f(N2,P2,D2)|R2]):-
	(
		N = N1,
		P = P1,
		N2 = N,
		P2 = P,
		D2 = D,!
		;
		N2 = N1,
		P2 = P1,
		D2 = D1
	),
	addUniverse3(f(N,P,D),R1,R2).

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

saveModel(FileName):-
	actualModelName(ModelName),
	saveModel(FileName,ModelName).

saveModel(FileName,ModelName):-
	actualModel(ModelName,Model),
	open(FileName, write, File),
	write(File, Model),
	write(File,'.'),
	close(File),
	addMessage(['Model ',ModelName,' has been saved.']).

loadModel(FileName):-
	open(FileName, read, File),
	read(File, Model),
	close(File),
	Model = model(ModelName,_,_,_,_),
	loadError(ModelName),
	assert(actualModel(ModelName,Model)),
	addMessage(['Model ',ModelName,' is loaded from file.']),
	changeActualModel(ModelName).
	
loadError(ModelName):-
	actualModel(ModelName,_),
	!,
	addMessage(['ERROR: model ',ModelName,' is exists already.']),
	fail.
loadError(_).

copyModel(NewName):-
	actualModelName(OldName),
	actualModel(OldName,Model),
	copyModel(Model,NewName).

copyModel(OldName,NewName):-
	atom(OldName),
	loadError(NewName),
	actualModel(OldName,Model),
	copyModel(Model,NewName),
	addMessage(['Model ',OldName,' has been copied as ',NewName,'.']),
	changeActualModel(NewName).

copyModel(Model,NewName):-
	Model = model(ModelName,D,_,N,_),
	renamePredicate1(ModelName,1,NewName,Model,Model1),
	Model1 = model(ModelName,D,P1,N,I1),
	assert(actualModel(NewName, model(NewName,D,P1,N,I1))).

renameModel(NewName):-
	actualModelName(OldName),
	renameModel(OldName,NewName).

renameModel(OldName,NewName):-
	copyModel(OldName,NewName),
	retract(actualModel(OldName,_)),
	addMessage(['Model ',OldName,' has been renamed as ',NewName,'.']),	
	changeActualModel(NewName).

models:-
	findall(ModelName, actualModel(ModelName,_), Names),
	writeModelNames(Names),!.
models:-
	write('There is not any model yet.'),
	nl.

writeModelNames([]).
writeModelNames([F|R]):-
	write(F), nl,
	writeModelNames(R).

/*******************************************************
* A már meglevő modellek változtatása
*   - addNewName: új név hozzáadása
*   - addNewPredicate: új predikátum hozzáadása
*   - addNewClassifier: új osztályozó hozzáadása (reláció és osztályok)
*   - addNewOrdering: új lineáris rendezés hozzáadása
*******************************************************/

addNewName(NewName,Ind):-
	actualModelName(ModelName),
	addNewName(ModelName,NewName,Ind).

addNewName(ModelName,NewName,Ind):-
	isThereSuchModel(ModelName),
	actualModel(ModelName,model(ModelName,D,P,N,I)),
	\+ memberList(f(0,NewName,_),P), % nincs még ilyen név
	memberList(Ind,D),
	retract(actualModel(ModelName,_)),
	assert(actualModel(ModelName,model(ModelName,D,P,[f(0,NewName,Ind)|N],I))),
	addMessage(['Name ',NewName,' has been added to model ',ModelName,'. It refers to ',Ind,'.']).

%%%%%%%%%%%%%%%
addNewPredicate(PredName,ArgNum):-
	actualModelName(ModelName),
	actualModel(ModelName,model(_,D,_,_,_)),
	addNewPredicate(ModelName,PredName,ArgNum,D).
	
addNewPredicate(PredName,ArgNum,Scope):-
	atom(PredName),
	actualModelName(ModelName),
	addNewPredicate(ModelName,PredName,ArgNum,Scope).

% Scope: a predikátum ezen individummhalmaz elemein értelmezett
addNewPredicate(ModelName,PredName,ArgNum,Scope):-
	isThereSuchModel(ModelName),
	integer(ArgNum),
	ArgNum > 0,
	ArgNum < 4,
	actualModel(ModelName,Model),
	Model = model(ModelName,D,P,N,_),
	\+memberList([PredName,ArgNum],P),
	i2p(Scope,Scope1),
	scope(ArgNum,Model,Scope1,Scope2),
	predicates_1(Model,ArgNum,PredName,Scope2,I,0,_),
	retract(actualModel(ModelName,_)),
	assert(actualModel(ModelName,model(ModelName,D,[[PredName,ArgNum]|P],N,I))),
	addMessage(['Predicate ',PredName,' is added to model ',ModelName,'.']),
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
	actualModel(ModelName,model(ModelName,D,_,_,_)),
	addNewOrdering(ModelName,PredName,D).

addNewOrdering(PredName, Scope):-
	atom(PredName),
	actualModelName(ModelName),
	addNewOrdering(ModelName,PredName,Scope).

addNewOrdering(ModelName,PredName,Scope):-
	isThereSuchModel(ModelName),
	actualModel(ModelName,Model),
	Model = model(ModelName,D,P,N,_),
	\+memberList([PredName,2],P),
	i2p(Scope,Scope1),
	scope(1,Model,Scope1,Scope2),
	orderings_1(Model,PredName,Scope2,I,0,_),
	retract(actualModel(ModelName,_)),
	assert(actualModel(ModelName,model(ModelName,D,[[PredName,2]|P],N,I))),
	addMessage(['Predicate ',PredName,' is added to model ',ModelName,'.']),
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
	addNewClassifier(ModelName,RelName,ClassNames,D,empty).

addNewClassifier(RelName,ClassNames,noempty):-
	actualModelName(ModelName),
	actualModel(ModelName,model(ModelName,D,_,_,_)),
	addNewClassifier(ModelName,RelName,ClassNames,D,noempty).

addNewClassifier(RelName,ClassNames,Scope):-
	atom(RelName),
	actualModelName(ModelName),
	addNewClassifier(ModelName,RelName,ClassNames,Scope,empty).

addNewClassifier(RelName,ClassNames,Scope,noempty):-
	atom(RelName),
	actualModelName(ModelName),
	addNewClassifier(ModelName,RelName,ClassNames,Scope,noempty).

addNewClassifier(ModelName,RelName,ClassNames,Scope,Empty):-
	isThereSuchModel(ModelName),
	actualModel(ModelName,Model),
	Model = model(ModelName,D,P,N,_), % ez az eddigi modell, a világokat újraszámolja
	i2p(Scope,Scope1),
	scope(1,Model,Scope1,Scope2),
	classifiers_1(Model,Scope2,RelName,ClassNames,I,0,_,Empty), 
		% itt generálja az új világokat (I), 0-tól kezdi a világok számozását
	classifiers_5(P,P2,ClassNames), % az osztálynevekből a predikátumnevek listáját hozza létre
	retract(actualModel(ModelName,_)),
	assert(actualModel(ModelName,model(ModelName,D,[[RelName,2]|P2],N,I))),
	addMessage(['Predicate ',RelName,' is added to model ',ModelName,'.']),
	writeWorldNumberIfMust(ModelName),!.
	
classifiers_1(model(_,_,_,_,[]),_,_,_,[],Idnum,Idnum,_).	
% sorraveszi a korábbi modell világait: I
% ezek mindegyikét megsokszorozza a lehetséges osztályozások szerint
classifiers_1(model(ModelName,D,P,N,[I|RestI]),Scope,RelName,ClassNames,Indices,IdNumIn,IdNumOut,Empty):-
	classifiers_1(model(ModelName,D,P,N,RestI),Scope,RelName,ClassNames,I0,IdNumIn,IdNumTMP,Empty),
	length(ClassNames,ClassNum), % ennyi osztályt keres
	scope2(1,model(ModelName,D,P,N,[I]),Scope,[Scope2]), 
		% megkeresi, hogy az adott világban melyik individuumhalmaz felel meg a scope-nak
	findall(Classes,splitSettoN(Scope2,ClassNum,Classes),AllClasses),
		% a scope individuumhalmazt minden lehetséges módon szétvágja ClassNum számú részhalmazra
	emptyCheck(AllClasses, AllClasses2,Empty),
	classifiers_1b(I0,Indices,I,AllClasses2,RelName,ClassNames,IdNumTMP,IdNumOut).

classifiers_1b(I0,I0,_,[],_,_,IdNum,IdNum).

classifiers_1b(I0,Indices,I,[F|R],RelName,ClassNames,IdNumTMP,IdNumOut):-
	classifiers_2(I0,Indices,I,[F|R],RelName,ClassNames,IdNumTMP,IdNumOut).
	
emptyCheck(Classes,Classes,empty).
emptyCheck([],[],noempty).
emptyCheck([F|R],[F|R1],noempty):-
	noEmptySets(F,ok),
	!,
	emptyCheck(R,R1,noempty).
emptyCheck([_|R],R1,noempty):-
	emptyCheck(R,R1,noempty).
noEmptySets([],ok).
noEmptySets([[]|_],empty).
noEmptySets([[_|_]|R],OK):-
	noEmptySets(R,OK).

classifiers_2(Is,Is,_,[],_,_,IdNum,IdNum).
classifiers_2(IndicesIn,IndicesOut,I,[Classes|RestClasses],RelName,ClassNames,IdNumIn,IdNumOut):-
	classifiers_2(IndicesIn,IndicesTMP,I,RestClasses,RelName,ClassNames,IdNumIn,IdNumTMP),
	I = index(_,_,F0),
	classifiers_3(F0,F1,Classes,ClassNames),
	classifiers_4(F1,F2,Classes,RelName,[]),
	IdNumOut is IdNumTMP + 1,
	newID(ID,IdNumOut),
	IndicesOut = [index(IdNumOut,ID,F2)|IndicesTMP].

% classifiers_3(In, Out, Indclasses, Classnames).
classifiers_3(Fs,Fs,[],[]).
classifiers_3(FIn,FOut,[Class|Classes],[Name|Names]):-
	classifiers_3(FIn,FTMP,Classes,Names),
	FOut = [f(1,Name,Class)|FTMP].

% A RelName ekvivalenciareláció (kétargumentumú) denotációját (Tuples) adja az Fs-hez 
% classifiers_4(In, Out, Indclasses, Relname, []).
classifiers_4(Fs,[f(2,RelName,Tuples)|Fs],[],RelName,Tuples).
classifiers_4(FIn,FOut,[Class|Classes],RelName,T0):-
	findall([X,Y],(memberList(X,Class),memberList(Y,Class)),T1),
	append(T0,T1,Tuples),
	classifiers_4(FIn,FOut,Classes,RelName,Tuples).

% cl_5(A,B,C): A=[[p1,1],[p2,1]...] C=[p8, p9] --> B=[[p1,1],[p2,1]...[p8,1],[p9,1]]
classifiers_5(P,P,[]).
classifiers_5(P,[[ClassName,1]|P2],[ClassName|ClassNames]):-
	classifiers_5(P,P2,ClassNames).

deletePredicate(PredName):-
	actualModelName(ModelName),
	actualModel(ModelName, model(_,_,P,_,_)),
	memberList([PredName,ArgNum],P),!, % csak az elsőt keressük
	deletePredicate(PredName,ArgNum).
	

deletePredicate(PredName,ArgNum):-
	actualModelName(ModelName),
	actualModel(ModelName, model(ModelName,D,P,N,I)),
	memberList([PredName,ArgNum],P), % tényleg van ilyen predikátum?
	deletePredicate2(f(ArgNum,PredName,_),I,I1),
	dropDuplicateIndices(I1,I2),
	deletePredicate4([PredName,ArgNum],P,P1),
	retract(actualModel(ModelName,_)),
	assert(actualModel(ModelName,model(ModelName,D,P1,N,I2))),
	addMessage(['Predicate ',PredName,' is deleted from model ',ModelName,'.']),
	writeWorldNumberIfMust(ModelName).

deletePredicate2(_,[],[]).
deletePredicate2(f(ArgNum,PredName,_),[index(N,ID,Ps)|R],[index(N,ID,Ps1)|R1]):-
	deletePredicate2(f(ArgNum,PredName,_),R,R1),
	deletePredicate3(f(ArgNum,PredName,_),Ps,Ps1).

deletePredicate3(_,[],[]).
deletePredicate3(Pred,[Pred|R],R1):-
	deletePredicate3(Pred,R,R1),!.
deletePredicate3(Pred,[P|R],[P|R1]):-
	deletePredicate3(Pred,R,R1).

deletePredicate4(_,[],[]).
deletePredicate4(Pred,[Pred|R],R1):-
	deletePredicate4(Pred,R,R1),!.
deletePredicate4(Pred,[Pred1|R],[Pred1|R1]):-
	deletePredicate4(Pred,R,R1).

renamePredicate(OldName,NewName):-
	actualModelName(ModelName),
	actualModel(ModelName, model(_,_,P,_,_)),
	memberList([OldName,ArgNum],P),!, % csak az elsőt keressük
	renamePredicate(OldName,ArgNum,NewName).

renamePredicate(OldName,ArgNum,NewName):-
	actualModelName(ModelName),
	actualModel(ModelName, Model),
	renamePredicate1(OldName,ArgNum,NewName,Model,Model1),
	retract(actualModel(ModelName,_)),
	assert(actualModel(ModelName,Model1)),
	addMessage(['Predicate ',OldName,' has been renamed to ',NewName,' in model ',ModelName,'.']),
	writeWorldNumberIfMust(ModelName).

renamePredicate1(OldName,ArgNum,NewName,Model,Model1):-
	Model = model(ModelName,D,P,N,I),
	selectFromList([OldName,ArgNum],P,P1),!,
	renamePredicate2(OldName,ArgNum,NewName,I,I2),
	Model1 = model(ModelName,D,[[NewName,ArgNum]|P1],N,I2).
	
renamePredicate2(_,_,_,[],[]).
renamePredicate2(OldName,ArgNum,NewName,I,I2):-
	I = [index(Num,ID,F)|R],
	selectFromList(f(ArgNum,OldName,Den),F,F2),!,
	renamePredicate2(OldName,ArgNum,NewName,R,R2),
	I2 = [index(Num,ID,[f(ArgNum,NewName,Den)|F2])|R2].

/*************************************************************
* A modell szűkítése egy új állítás megadásával
*************************************************************/

and(Formula):-
	actualModelName(ModelName),
	and(ModelName,Formula).

and(ModelName,Formula0):-
	actualModel(ModelName,model(Name,D,P,N,I)),
	validityCheck(ModelName,Formula0,Formula,t),
	evaluate(Formula,model(Name,D,P,N,I),I2),
	retract(actualModel(ModelName,_)),
	assert(actualModel(ModelName,model(Name,D,P,N,I2))),
	term_to_atom(Formula0,Fatom),
	addMessage(['"',Fatom,'" has been asserted in model ',ModelName,'.']),
	writeWorldNumberIfMust(ModelName).

eval(Formula,Ind):-
	actualModelName(ModelName),
	eval(ModelName,Formula,Ind).
	
eval(ModelName,Formula0,Ind):-
	actualModel(ModelName,Model),
	i2p(Formula0,Formula),
	evaluate(Formula,Model,Ind),
	addMessage(['"',Formula0,'" has been evaluated in model ',ModelName,'.']).

writeIndices([]):-nl.
writeIndices([index(_,IName,_)]):-
	write(IName),nl.
writeIndices([index(_,IName,_)|[F|R]]):-
	write(IName),
	write(','),
	writeIndices([F|R]).

/*************************************************
* segédpredikátumok
*************************************************/

dropDuplicateIndices(ModelName):-
	actualModel(ModelName,model(Name,D,P,N,I)),
	dropDuplicateIndices(I,I2),
	retract(actualModel(ModelName,_)),
	assert(actualModel(ModelName,model(Name,D,P,N,I2))).

dropDuplicateIndices([],[]).
dropDuplicateIndices([I|RestI],[I|RestI3]):-
	dropSimilarIndices(I,RestI,RestI2),
	dropDuplicateIndices(RestI2,RestI3).

dropSimilarIndices(_,[],[]).
dropSimilarIndices(index(N,ID,W),[index(_,_,W)|R],R1):-
	dropSimilarIndices(index(N,ID,W),R,R1),
	!.
dropSimilarIndices(index(N,ID,W),[I|R],[I|R1]):-
	dropSimilarIndices(index(N,ID,W),R,R1).

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

% Ellenőrzi, hogy van-e ilyen nevű modell
isThereSuchModel(ModelName):- 
	actualModel(ModelName,_),!. % van
isThereSuchModel(ModelName):-
	addMessage(['ERROR: there is no model named ',ModelName,'.']),
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
* ezek itten a lekérdezést, képernyőre kiírást csinálják    *
************************************************************/
atomsConcat([],'').
atomsConcat([F|R],Result):-
	term_to_atom(F,F1),
	term_to_atom(F2, F1),
	atomsConcat(R,R0),
	atom_concat(F2,R0,Result).

addMessage(NewMsgList):-
	messages(Msg),
	atomsConcat(NewMsgList, NewMsg),
	writeOut(NewMsg),
	writeLog(NewMsg),
	retract(messages(_)),
	assert(messages([NewMsg|Msg])).

writeOut(Msg):-
	displayMessages(on),!,
	write(Msg),
	nl.
writeOut(_).

writeLog(_):-
	logFileName(''),
	!.
writeLog(Msg):-
	logFileName(LogFile),!,
	open(LogFile,append,Log),
	write(Log,Msg),
	nl(Log),
	close(Log).
	

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
	writeWorldNumber(ModelName,N),
	writeNames_(Names),
	writeWorlds_(I).

writeWorldNumberIfMust(ModelName):-
	echoWorldNumber(on),!,
	writeWorldNumber(ModelName).
writeWorldNumberIfMust(_).

writeWorldNumber:-
	actualModelName(ModelName),
	writeWorldNumber(ModelName).

writeWorldNumber(ModelName):-
	actualModel(ModelName,model(_,_,_,_,I)),
	length(I,N),
	writeWorldNumber(ModelName, N).
	
writeWorldNumber(ModelName,0):-
	addMessage(['There is not any world in model ', ModelName, '.']),!.
writeWorldNumber(ModelName,1):-
	addMessage(['There is only 1 world in model ', ModelName, '.']),!.
writeWorldNumber(ModelName,N):-
	atom_number(N1,N),
	addMessage(['There are ',N1,' worlds in model ', ModelName, '.']).

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
	memberList(index(N,ID,Preds),I),
	writeWorlds_([index(N,ID,Preds)]).

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
* convert infix formula to prefix
*******************************/

i2p(X,X) :- var(X), !.
i2p([],[]).
i2p([F|R], [F1|R1]) :- 
	i2p(F,F1),
	i2p(R,R1).
i2p(X : A, l_x(X,A1)) :- 
	var(X),
	!,
	i2p(A,A1).
i2p(some X : A, some(X,A1)) :- 
	var(X),
	!,
	i2p(A,A1).
i2p(all X : A, all(X,A1)) :- 
	var(X),
	!,
	i2p(A,A1).
i2p(iota X : A, iota(X,A1)) :- 
	var(X),
	!,
	i2p(A,A1).
i2p(~ A, not(A1)) :- !, i2p(A, A1).
i2p(A & B, and(A1,B1)) :- !, i2p(A, A1), i2p(B, B1).
i2p(A | B, or(A1,B1)) :- !, i2p(A, A1), i2p(B, B1).
i2p(A -> B, imp(A1,B1)) :- !, i2p(A, A1), i2p(B, B1).
i2p(A = B, eq(A1,B1)) :- !, i2p(A, A1), i2p(B, B1).
i2p(X,X1) :- 
	X =.. [Pred, A, B], !,
	i2p(A,A1),
	i2p(B,B1),
	X1 =.. [Pred, A1, B1].
i2p(X,X1) :- 
	X =.. [Pred, A], !,
	i2p(A,A1),
	X1 =.. [Pred, A1].
i2p(X,X).

syntaxCheck0(F,Cat):-syntaxCheck(0,0,F,Cat).
syntaxCheck(F,Cat):-
	actualModelName(ModelName),
	actualModel(ModelName,model(_,_,P,N,_)),
	syntaxCheck(N,P,F,Cat).
syntaxCheck(ModelName,F,Cat):-
	actualModel(ModelName,model(_,_,P,N,_)),
	syntaxCheck(N,P,F,Cat).
syntaxCheck(N,P,F,t):- 
	\+ var(F),
	F = not(X), !,
	syntaxCheck(N,P,X,t).
syntaxCheck(N,P,F,t):- 
	\+ var(F),
	(
		F = and(X,Y);
		F = or(X,Y);
		F = imp(X,Y)
	), !,
	syntaxCheck(N,P,X,t),
	syntaxCheck(N,P,Y,t).
syntaxCheck(N,P,F,t):- 
	\+ var(F),
	F = eq(X,Y),!,
	syntaxCheck(N,P,X,e),
	syntaxCheck(N,P,Y,e).
syntaxCheck(N,P,F,t):- 
	\+ var(F),
	(
		F = some(X,Y);
		F = all(X,Y)
	), !,
	syntaxCheck(N,P,X,v),
	syntaxCheck(N,P,Y,t).
syntaxCheck(N,P,F,e):- 
	\+ var(F),
	F = iota(X,Y), !,
	syntaxCheck(N,P,X,v),
	syntaxCheck(N,P,Y,t).
syntaxCheck(N,P,F,l):- 
	\+ var(F),
	F = l_x(X,Y), !,
	syntaxCheck(N,P,X,v),
	syntaxCheck(N,P,Y,t).
syntaxCheck(_,_,X,v):-var(X).
syntaxCheck(_,_,X,e):-var(X).
syntaxCheck(N,_,X,e):- 
	\+ var(X),
	memberList(f(0,X,_),N).
syntaxCheck(N,P,F,t):-
	\+ var(F),
	F =.. [Pred,Arg],
	memberList([Pred,1],P),
	syntaxCheck(N,P,Arg,e).
syntaxCheck(N,P,F,t):-
	\+ var(F),
	F =.. [Pred,Arg1,Arg2],
	memberList([Pred,2],P),
	syntaxCheck(N,P,Arg1,e),
	syntaxCheck(N,P,Arg2,e).
syntaxCheck(N,P,F,t):-
	\+ var(F),
	F =.. [Pred,Arg1,Arg2,Arg3],
	memberList([Pred,3],P),
	syntaxCheck(N,P,Arg1,e),
	syntaxCheck(N,P,Arg2,e),
	syntaxCheck(N,P,Arg3,e).
syntaxCheck(0,0,X,e):-atom(X).
syntaxCheck(0,0,F,t):-
	\+ var(F),
	F =.. [Pred,Arg],
	atom(Pred),
	syntaxCheck(0,0,Arg,e).
syntaxCheck(0,0,F,t):-
	\+ var(F),
	F =.. [Pred,Arg1,Arg2],
	atom(Pred),
	syntaxCheck(0,0,Arg1,e),
	syntaxCheck(0,0,Arg2,e).
syntaxCheck(0,0,F,t):-
	\+ var(F),
	F =.. [Pred,Arg1,Arg2,Arg3],
	atom(Pred),
	syntaxCheck(0,0,Arg1,e),
	syntaxCheck(0,0,Arg2,e),
	syntaxCheck(0,0,Arg3,e).

validityCheck(ModelName,F,F1,ValidCat):-
	i2p(F,F1),
	syntaxCheck(ModelName,F1,ValidCat),!.
validityCheck(ModelName,F,_,_):-
	term_to_atom(F,F1),
	addMessage(['ERROR: "',F1,'" is not a well-formed formula in model ',ModelName,'!']),
	fail.

saveModelXml(FileName):-
	actualModelName(ModelName),
	saveModelXml(FileName, ModelName).

saveModelXml(FileName,ModelName):-
	actualModel(ModelName,Model),
	model2xml(Model,XML),
	open(FileName, write, File),
	xml_write(File,XML,[indent(1)]),
	close(File).

loadModelXml(FileName):-
	load_xml(FileName, XML, [space(remove)]),
	xml2model(XML, Model),
	Model = model(ModelName,_,_,_,_),
	loadError(ModelName),
	assert(actualModel(ModelName,Model)),
	changeActualModel(ModelName).

saveModelTsv(FileName):-
	actualModelName(ModelName),
	saveModelTsv(FileName,ModelName).

saveModelTsv(FileName,ModelName):-
	atom(ModelName),
	actualModel(ModelName,Model),
	saveModelTsv_(FileName,Model).

loadModelTsv(FileName):-
	loadModelTsv_(FileName,Model),
	Model = model(ModelName,_,_,_,_),
	loadError(ModelName),
	assert(actualModel(ModelName,Model)),
	changeActualModel(ModelName).

writeOK:-
	write('OK'),
	nl.
