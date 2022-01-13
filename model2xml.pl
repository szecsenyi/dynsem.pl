:- module(model2xml,[
		model2xml/2,
		xml2model/2]).


model2xml(model(ModelName,D,P,N,I), XML):-
	XML = [element(model, [n = ModelName], [
		element(inds, [], XD),
		element(preds, [], XP),
		element(names, [], XN),
		element(worlds, [], XW)])],
	getXmlInds(D,XD),
	getXmlPreds(P,XP),
	getXmlNames(N,XN),
	getXmlWorlds(I,XW).
	
getXmlInds([],[]).
getXmlInds([I|R],[element(i,[i = I],[])|X]):- 
	getXmlInds(R,X).

getXmlPreds([],[]).
getXmlPreds([[PredName, Arity]|R],[element(p,[n = PredName, a = Arity], [])|XML]):- 
	getXmlPreds(R,XML).

getXmlNames([],[]).
getXmlNames([f(0,Name,Ind)|R],[element(n,[n = Name, i = Ind],[])|XML]):- 
	getXmlNames(R,XML).

getXmlWorlds([],[]).
getXmlWorlds([index(Num,ID,Preds)|R],[element(w,[n = ID, c = Num],XmlPreds)|XML]):- 
	getXmlPreds2(Preds, XmlPreds),
	getXmlWorlds(R,XML).
	
getXmlPreds2([],[]).
getXmlPreds2([f(Arity,PredName,V)|R],[element(p,[n = PredName, a = Arity], XV)|XML]):- 
	getXmlPreds3(V,XV),
	getXmlPreds2(R,XML).

getXmlPreds3([],[]).
getXmlPreds3([Ind|R],[element(d, [i1 = Ind], [])|XML]):- 
	atom(Ind),
	getXmlPreds3(R,XML).
getXmlPreds3([[I1,I2]|R],[element(d, [i1 = I1, i2 = I2], [])|XML]):- 
	getXmlPreds3(R,XML).
getXmlPreds3([[I1,I2,I3]|R],[element(d, [i1 = I1, i2 = I2, i3 = I3], [])|XML]):- 
	getXmlPreds3(R,XML).

/************************************************/

xml2model(XML,Model):-
	getFirstElement(XML, model, Atts, ModelParts),
	getAV(n, Atts, ModelName),
	getFirstElement(ModelParts, inds, _, XInds),
	getIndsXML(XInds, Inds),
	getFirstElement(ModelParts, preds, _, XPreds),
	getPredsXML(XPreds, Preds),
	getFirstElement(ModelParts, names, _, XNames),
	getNamesXML(XNames, Names),
	getFirstElement(ModelParts, worlds, _, XWorlds),
	getWorldsXML(XWorlds, Worlds),
	!,
	Model = model(ModelName, Inds, Preds, Names, Worlds).

getIndsXML([],[]).
getIndsXML([XML|R], [V|Inds]):-
	XML = element(i,A,_),
	getAV(i,A,V),
	!,
	getIndsXML(R,Inds).
getIndsXML([_|R], Inds):-
	getIndsXML(R,Inds).

getPredsXML([],[]).
getPredsXML([XML|R], [[Pred, Arity]|Preds]):-
	XML = element(p,Atts,_),
	getAV(n,Atts,Pred),
	getAV(a,Atts,Arity0),
	atom_number(Arity0, Arity),
	!,
	getPredsXML(R,Preds).
getPredsXML([_|R], Preds):-
	getPredsXML(R,Preds).

getNamesXML([],[]).
getNamesXML([XML|R], [f(0,Name,Ind)|Names]):-
	XML = element(n,Atts,_),
	getAV(n, Atts, Name),
	getAV(i, Atts, Ind),
	!,
	getNamesXML(R,Names).
getNamesXML([_|R], Names):-
	getNamesXML(R,Names).


getWorldsXML([],[]).
getWorldsXML([XML|R], [index(N,ID,Preds)|Worlds]):-
	XML = element(w,Atts,XPreds),
	getAV(n,Atts,ID),
	getAV(c,Atts,N0),
	atom_number(N0, N),
	!,
	getPredsXML2(XPreds, Preds),
	getWorldsXML(R,Worlds).
getWorldsXML([_|R], Worlds):-
	getWorldsXML(R,Worlds).
	
getPredsXML2([],[]).
getPredsXML2([XML|R], [f(Arity,PredName,Dens)|Preds]):-
	XML = element(p,Atts,XDen),
	getAV(n, Atts, PredName),
	getAV(a, Atts, Arity0),
	atom_number(Arity0, Arity),
	!,
	getDensXML(Arity, XDen, Dens),
	getPredsXML2(R,Preds).
getPredsXML2([_|R], Preds):-
	getPredsXML2(R,Preds).


getDensXML(_,[],[]).
getDensXML(1, [F|R], [I1|Ds]):-
	F = element(d, Atts, _),
	getAV(i1, Atts, I1),
	!,
	getDensXML(1, R, Ds).
getDensXML(2, [F|R], [[I1,I2]|Ds]):-
	F = element(d, Atts, _),
	getAV(i1, Atts, I1),
	getAV(i2, Atts, I2),
	!,
	getDensXML(2,R,Ds).
getDensXML(3, [F|R], [[I1,I2,I3]|Ds]):-
	F = element(d, Atts, _),
	getAV(i1, Atts, I1),
	getAV(i2, Atts, I2),
	getAV(i3, Atts, I3),
	!,
	getDensXML(3,R,Ds).
getDensXML(Arity, [_|R], Dens):-
	getDensXML(Arity, R, Dens).

getFirstElement([element(ElementName, Atts, SubElementList)|_], ElementName, Atts, SubElementList):-!.
getFirstElement([_|R], ElementName, Atts, SubElementList):-
	getFirstElement(R, ElementName, Atts, SubElementList).

getAV(Attr,[Attr = V0 |_],V):-
	normalize_space(atom(V), V0),
	!.
getAV(Attr, [_|R], V):-
	getAV(Attr, R, V).