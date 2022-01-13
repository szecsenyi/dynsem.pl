:- module(model2tsv,[
		saveModelTsv_/2,
		loadModelTsv_/2]).

loadModelTsv_(FileName,Model):-
	open(FileName,read,S1),getModelName(S1,ModelName),close(S1),!,
	open(FileName,read,S2),getInds(S2,D,DC),close(S2),!,
	open(FileName,read,S3),getNames(S3,N,DC),close(S3),!,
	open(FileName,read,S4),getPredNames(S4,P),close(S4),!,
	open(FileName,read,S5),getWorlds(S5,P,DC,I),close(S5),!,
	Model = model(ModelName,D,P,N,I).

saveModelTsv_(FileName,Model):-
	Model = model(ModelName,D,P,N,I),
	indCode(D,Inds,97), % 97 -> a
	open(FileName,write,S),
	% S = current_output,
	writeX(S,['#model:',ModelName,'\n']),
	write(S,'#individuums:'), writeInds(S,Inds),
	write(S,'#names:'), writeNames(S,N,Inds),
	write(S,'ID'), writePredNames(S,P),
	writeWorlds(S,I,Inds,P),
	close(S).

/*********************************************************/

getModelName(S,ModelName):-
	readLine(S,L),
	(
		split_atom(L,':',' ',['#model',ModelName|_])
		;
		getModelName(S,ModelName)
	).
split_atom(L,Sep,Pad,Atoms):-
	split_string(L,Sep,Pad,Strings),
	str2atom(Strings,Atoms).
str2atom([],[]).
str2atom([S|SR],[A|AR]):-
	atom_string(A,S),
	str2atom(SR,AR).

getInds(S,D,DC):-
	readLine(S,L),
	(
		split_atom(L,'\t',' ',['#individuums:'|D2]),
		getIndsAux(D2,D,DC)
		;
		getInds(S,D,DC)
	).
getIndsAux([],[],[]).
getIndsAux([F|R],[D|DR],[[D,C]|DCR]):-
	split_atom(F,'=','',[D,C]),!,
	getIndsAux(R,DR,DCR).
getIndsAux([_|R],D,DC):-
	getIndsAux(R,D,DC).

getNames(S,N,DC):-
	readLine(S,L),
	(
		split_atom(L,'\t',' ',['#names:'|N2]),
		getNamesAux(N2,N,DC)
		;
		getNames(S,N,DC)
	).
getNamesAux([],[],_).
getNamesAux([F|R],[f(0,Name,D)|NR],DC):-
	split_atom(F,'=','',[Name,C]),!,
	member([D,C],DC),
	getNamesAux(R,NR,DC).
getNamesAux([_|R],N,DC):-
	getNamesAux(R,N,DC).

getPredNames(S,P):-
	readLine(S,L),
	(
		split_atom(L,'\t',' ',['ID'|P2]),
		getPredNamesAux(P2,P)
		;
		getPredNames(S,P)
	).
getPredNamesAux([],[]).
getPredNamesAux([F|R],[[PredName,ArgNum]|PR]):-
	split_atom(F,'/','',[PredName,ANstr]),
	atom_number(ANstr,ArgNum),!,
	getPredNamesAux(R,PR).
getPredNamesAux([_|R],PR):-
	getPredNamesAux(R,PR).

getWorlds(S,_,_,[]):-
	at_end_of_stream(S).
getWorlds(S,P,DC,I):-
	readLine(S,L),
	(
		split_atom(L,'\t',' ',[IDstr|W]),
		atom_number(IDstr,IDNum),!,
		getPredDs(W,P,DC,Den),
		atom_concat(i,IDstr,ID),
		getWorlds(S,P,DC,IR),
		I = [index(IDNum,ID,Den)|IR]
	;
		getWorlds(S,P,DC,I)
	).

getPredDs([],[],_,[]).
getPredDs([F|R],[[PredName,ArgNum]|PR],DC,[f(ArgNum,PredName,Ds)|DR]):-
	split_atom(F,'|','',Cs),
	getPredDsAux(ArgNum,Cs,DC,Ds),
	getPredDs(R,PR,DC,DR).
getPredDsAux(_,[''],_,[]).
getPredDsAux(_,[],_,[]).
getPredDsAux(1,[C|CR],DC,[D|DR]):-
	member([D,C],DC),
	getPredDsAux(1,CR,DC,DR).
getPredDsAux(2,[CC|CR],DC,[[D1,D2]|DR]):-
	atom_chars(CC,[C1,C2]),
	member([D1,C1],DC),
	member([D2,C2],DC),
	getPredDsAux(2,CR,DC,DR).
getPredDsAux(3,[CCC|CR],DC,[[D1,D2,D3]|DR]):-
	atom_chars(CCC,[C1,C2,C3]),
	member([D1,C1],DC),
	member([D2,C2],DC),
	member([D3,C3],DC),
	getPredDsAux(3,CR,DC,DR).

/***************************************************/
writeInds(S,[]):- nl(S).
writeInds(S,[[D,C]|R]):-
	writeX(S,['\t',D,'=',C]),
	writeInds(S,R).

writeNames(S,[],_):- nl(S).
writeNames(S,[F|R],Inds):-
	F = f(0,Name,D),
	member([D,C],Inds),!,
	writeX(S,['\t',Name,'=',C]),
	writeNames(S,R,Inds).

writePredNames(S,[]):- nl(S).
writePredNames(S,[[PredName,ArgNum]|R]):-
	writeX(S,['\t',PredName,'/',ArgNum]),
	writePredNames(S,R).

writeWorlds(_,[],_,_).
writeWorlds(S,[F|R],Inds,P):-
	F = index(Num,_,Preds),
	write(S,Num),
	writePreds(S,Preds,Inds,P),
	writeWorlds(S,R,Inds,P).

writePreds(S,[],_,[]):- nl(S).
writePreds(S,Preds,Inds,[FP|RP]):-
	FP = [PredName,ArgNum],
	select(f(ArgNum,PredName,D),Preds,Preds2),
	write(S,'\t'),
	writeDen(S,ArgNum,D,Inds),
	writePreds(S,Preds2,Inds,RP).

writeDen(_,_,[],_).
writeDen(S,ArgNum,[F],Inds):-
	writeD(S,ArgNum,F,Inds).
writeDen(S,ArgNum,[F1,F2|R],Inds):-
	writeD(S,ArgNum,F1,Inds),
	write(S,'|'),
	writeDen(S,ArgNum,[F2|R],Inds).

writeD(S,1,D,Inds):-
	member([D,C],Inds),
	write(S,C).
writeD(S,2,[D1,D2],Inds):-
	member([D1,C1],Inds),
	member([D2,C2],Inds),
	writeX(S,[C1,C2]).
writeD(S,3,[D1,D2,D3],Inds):-
	member([D1,C1],Inds),
	member([D2,C2],Inds),
	member([D3,C3],Inds),
	writeX(S,[C1,C2,C3]).

writeX(_,[]).
writeX(S,[F|R]):-
	write(S,F),
	writeX(S,R).

indCode([],[],_).
indCode([F|R],[[F,C1]|R1],C):-
	atom_codes(C1,[C]),
	CNext is C + 1,
	indCode(R,R1,CNext).
	
readLine(F,L):-
	get_code(F,C),
	checkCharAndReadRest(C,L0,F),
	atom_codes(L,L0).

checkCharAndReadRest(10, [], _):- !.
checkCharAndReadRest(-1, [], _):- !. 
checkCharAndReadRest(end_of_file, [], _):- !. 

checkCharAndReadRest(Char,[Char|Chars],S):-
    get_code(S,NextChar),
    checkCharAndReadRest(NextChar,Chars,S).
