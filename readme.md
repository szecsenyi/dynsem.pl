# dynsem.pl
v 1.1 Szécsényi Tibor 2022

Dinamikus szemantikai modellek építése, amely modellek több 
interpretációs világot tartalmaznak, és a diskurzus során 
megfogalmazott új információk, logikai kifejezések leszűkítik 
az interpretációs világok halmazát. 

* [A program részei, futtatás](#a-program-reszei-futtatas)
* [Dinamikus modellreprezentáció](#a-dinamikus-modellreprezentacio)
* [A használható parancsok leírása](#a-hasznalhato-parancsok-leirasa)

<a id="a-program-reszei-futtatas"></a>

# A program részei, futtatás
A program Prolog nyelven íródott, kipróbálva 
az [SWI-Prolog](https://www.swi-prolog.org/) 8.0.1 verzióján lett. 

A program öt fájlból áll. A *modellChecker3.pl* és a *comsemPredicates.pl* 
fájlok [Patrick Blackburn és Johan Bos *Representation and Inference for 
Natural Language. A First Course in Computational Semantics*](http://www.let.rug.nl/bos/comsem/book1.html) 
című könyvéhez tartozó programkönyvtár két darabjának kibővített változata. 
A *dynsem.pl* maga a főprogram. A *model2tsv.pl* és a *model2xm.pl* a létrehozott modellek 
lemezre írását és lemezről való beolvasását végzik.


A program futtatása a *dynsem.pl* fájlnak az SWI-Prolog értelmezőbe való 
betöltésével történik. A parancsokat a prolog értelmezőben megjelenő `?-` 
prompt után kell beírni. A parancsokat lehet egyenként is futattni, 
vagy egyszerre több parancsot is megadva, azokat egymás után, vesszővel beírva. 
A beírt parancsokat a `.` jel beírásával kell zárni, majd entert nyomni.
```
?- createModel(beatles, [john, paul, ringo], []).
...
?- addNewPredicate(hungry, 1), writeWorlds.
```

A parancsok megadását először egy dinamikus szemantikai modell létrehozásával 
célszerű kezdeni (pl.: `createModel(beatles, [john, paul, ringo], [])`), 
majd új nevek, predikátumok bevezetésével 
(pl.: `addNewPredicate(hungry, 1)`), 
illetve a világra vonatkozó új információk megadásával lehet folytatni (pl.: 
`and(beatles, hungry(john))`. 
Új predikátumok bevezetésével növelhetjük, új információk megadásával 
csökkenthetjük az interpretációs világok számát. 
Eközben megjeleníthetjük a modellt, vagyis az interpretációs világokat 
a `writeWorlds` paranccsal.

```
?- createModel(beatles, [john, paul, ringo], []).
Model beatles has been created.
There is only 1 world in model beatles.
true .

?- addNewPredicate(hungry, 1), writeWorlds.
Predicate hungry is added to model beatles.
There are 8 worlds in model beatles.
There are 8 worlds in model beatles.
john --> john
paul --> paul
ringo --> ringo
index(i7): hungry=[ringo], beatles=[john,paul,ringo], 
index(i6): hungry=[paul,ringo], beatles=[john,paul,ringo], 
index(i5): hungry=[paul], beatles=[john,paul,ringo], 
index(i4): hungry=[john,ringo], beatles=[john,paul,ringo], 
index(i3): hungry=[john,paul,ringo], beatles=[john,paul,ringo], 
index(i2): hungry=[john,paul], beatles=[john,paul,ringo], 
index(i1): hungry=[john], beatles=[john,paul,ringo], 
index(i0): hungry=[], beatles=[john,paul,ringo], 
true .

?- and(beatles, hungry(john)).
"hungry(john)" has been asserted in model beatles.
There are 4 worlds in model beatles.
true.

?- writeWorlds.
There are 4 worlds in model beatles.
john --> john
paul --> paul
ringo --> ringo
index(i4): hungry=[john,ringo], beatles=[john,paul,ringo], 
index(i3): hungry=[john,paul,ringo], beatles=[john,paul,ringo], 
index(i2): hungry=[john,paul], beatles=[john,paul,ringo], 
index(i1): hungry=[john], beatles=[john,paul,ringo], 
true.

?-
```

<a id="a-dinamikus-modellreprezentacio"></a>

# A dinamikus modellreprezentáció
A modellek öt részből állnak: `model(ModelID,D,Preds,Names,I)`
- [`ModelID`: a modell azonosítója vagy neve](#modelid) - atom
- [`D`: a modellben található individuumok halmaza, modelluniverzum](#individuumok-d) - lista
- [`Preds`: a modellben használható predikátumok nevei és argumentumszáma](#predikatumok-preds) - lista
- [`Names`: a modellben használható nevek és azok denotátuma (mely individuumra utal)](#nevek-names) - lista
- [`I`: a modellben lehetséges interpretációs világok, 'indexek'](#indexek) - lista

A modell azonosítója és az individuumok halmaza a modell létrehozása 
után nem változtatható, a predikátumok és nevek listája viszont bővíthető. 
Az indexek automatikusan változnak, explicit módosításuk nem lehetséges. 

<a id="modelid"></a>

### Modellazonosító: ModelID
A modell azonosítója tetszőleges prolog atom lehet: kis betűvel kezdődő, 
betűket, számokat vagy aláhúzást tartalmazó kifejezés.
A modellekre a nevükkel lehet hivatkozni. Ha nincs név megadva, akkor 
az aktuális modellre vonatkozik az utasítás.
Az aktuális modell neve az `actualModelName\1` dinamikus predikátummal 
érhető el: `:- actualModelName(ModelName).`

### Individuumok: D
A modell individuumainak a halmaza, azaz a modelluniverzum listaként van 
tárolva, az individuumok azonosítója tetszőleges prolog atom lehet. 
Az individuumhalmaz a modell létrehozása után nem változtatható. 
Az individuumokra az azonosítójukkal névként is lehet hivatkozni, 
a modell létrehozása során az individuumok azonosítójával megegyező nevek 
is keletkeznek.

### Nevek: Names
A nevek listaként vannak megadva, nevezetesen `f(0, name, ind)` kifejezések 
listájaként, ahol a 0 azt fejezi ki, hogy a nevek nullaargumentumú predikátumok, 
a *name* az adott név, *ind* pedig az az individuum, amire utal. A nevek 
nullaargumentumú predikátumokként vannak kezelve, ezért a predikátumok 
listáján is fel vannak sorolva. A nevek denotátuma minden interpretációs 
világban ugyanaz, ezért a különböző indexű világokban nincsenek külön feltüntetve.
A nevek denotátuma állandó, a deklarálásuk után nem változtatható.
Egy modell létrehozásakor a modell individuumai névként is deklarálódnak. 
A modellbe a modell létrehozása után új nevek is bevezethetők: 
[`addNewName`](#addnewname)

<a id="predikatumok-preds"></a>

### Predikátumok: Preds
A predikátumok az argumentumuk számával rendezett párt alkotva listaként 
vannak megadva, pl.: `[[name1, 0], [name2, 0], [pred1, 1], [pred2, 1], [pred3,2]]` 
A modell létrehozásakor a modell nevével megegyező nevű egyargumentumú 
predikátum is deklarálódik, amelynek a denotátuma a modell összes individuumát 
tartalmazza minden egyes interpretációs világban. 
A program egy-, két és háromargumentumú predikátumokat tud kezelni. 
A már deklarált predikátumok a [`renamePredicate`](#renamepredicate)  paranccsal 
átnevezhetők vagy a [`deletePredicate`](#deletepredicate) törölhetők, 
de újakat is hozhatunk létre: 
[`addNewPredicate`](#addnewpredicate), 
[`addNewOrdering`](#addnewordering), 
[`addNewClassifier`](#addnewclassifier) 

<a id="indexek"></a>
### Interpretációs világok: I
Egy modellhez tartozó interpretációs világok vagy indexek listaként vannak megadva. 
Az egyes indexek `index(Num, ID, Interperations)` információhármassal 
vannak jellemezve: az első az indexvilágnak a sorszáma (automatikusan számozott), 
a másik az indexnek az egyedi azonosítója (automatikusan generált, 'i' + NUM alakú), 
végül a harmadik a modellben szereplő predikátumok indexbeli interpretációját adja meg. 
A predikátumok interpretációi `f(ArgNum, PredName, Den)` alakban megadott kifejezések 
listája, ahol az *ArgNum* a predikátum argumentumainak a számát adja meg, 
a *PredName* a predikátum nevét, a *Den* pedig a jelöletét az aktuális világban: 
egyargumentumú predikátumok esetében individuumok halmazaként (lista!), 
kétargumentumú predikátumoknál individuumpárok halmazaként (kételemű listák listája), 
háromargumentumú predikátumoknál pedig individuumhármasok halmazaként. 
A modell létrehozásakor a modell nevével megegyező nevű egyargumentumú 
predikátum is deklarálódik, amelynek a denotátuma a modell összes individuumát 
tartalmazza minden egyes interpretációs világban. 
```
[index(1,i1,[f(1,pred1,[a,b,c], f(1,pred2,[a,c]), f(2,pred3,[[a,b],[b,a]), ...],
 index(2,i2,[f(1,pred1,[c], f(1,pred2,[a,b,c]), f(2,pred3,[[a,b],[a,c]), ...],
 ...]
```
Az interpretációs világok, az indexek a program használata során automatikusan 
jönnek létre. Egy világ tartalma a [`writeWorld`](#writeworld) paranccsal jeleníthető meg.

<a id="a-hasznalhato-parancsok-leirasa"></a>

# A használható parancsok leírása
* [Modellek létrehozása, egyesítése és törlése, lekérdezése](#modellmuveletek)
* [Új név bevezetése a modellbe](#uj-nev)
* [Predikátumok kezelése](#uj-predikatum)
* [Logikai formula kiértékelése egy modellben](#kiertekeles)
* [Használható logikai kifejezések](#logikai-kifejezesek)

<a id="modellmuveletek"></a>

## Modellek létrehozása, egyesítése és törlése, lekérdezése

### createModel
```
createModel(ModelName, D, Names)
```

Létrehoz egy *ModelName* azonosítójú modellt *D*-ben felsorolt individuumokkal. 

A *Names* paraméterben a modellben használandó neveket adhatjuk meg a denotátumiakkal. 
Ha nem akarunk a modell létrehozásakor neveket megadni, akkor `[]` üres listát 
használhatunk. A *D* individuumhalmaz elemei névként is használhatóak, azokat 
nem kell itt felsorolni.

Példák:
```
createModel(beatles, [john, george, paul, ringo], [[john_lennon, john]])
createModel(test, [a,b,c], [])
```
A modell létrehozásakor egyetlen interpretációs világ keletkezik, amelyben 
csak a modell nevével megegyező nevű eyargumentumú predikátum található, 
az interpretációja pedig a *D* individuumhalmazzal megegyező.

### loadModel, saveModel
```
saveModel(FileName, ModelName) 
saveModel(FileName)
saveModelXml(FileName, ModelName)
saveModelXml(FileName)
saveModelTsv(FileName, ModelName)
saveModelTsv(ModelName)
loadModel(FileName)
loadModelXml(FileName)
loadModelTsv(FileName)
```   

A megadott nevű modellt vagy az [aktuális modellt](#changeactualmodel) elmenti 
a megadott nevű fájlba, vagy beolvassa annak tartalmát, és létrehoz egy új 
modellt a fájlban megadott névvel. File-ba írás során az esetleg meglevő fájlt 
felülírja. Beolvasás során csak akkor hozza létre a modellt, ha azzal a névvel 
még nem volt modell.

A program háromféle fájltípust kezel: az első prolog predikátumként írja és 
olvassa a fájl tartalmát, a második XML formátumúként kezeli, a harmadik 
pedig TSV (tab-szeparált szövegfájl) formátumúként - ez utóbbi a legkisebb 
fájlméretű.

A fájlnevet aposztrófok között, kiterjesztéssel együtt kell megadni, pl. 
`saveModelXml('beatles.xml', beatles)`

### dropModel
```
dropModel(ModelName)
```

A megadott nevű modellt "elfelejti".

Ha az aktuális modellt dobjuk el, akkor egy másik, létező modell válik aktuálissá.

### combineModels
```
combineModels(Model1, Model2, NewModel)
```

A *Model1* és a *Model2* azonosítójú modelleket egyesíti egy új modellben, amelynek 
a neve *MewModel* lesz.

Az új modellben az individuumok halmaza két kombinált modell individuumainak 
az uniója lesz (nem kell azonosnak vagy diszjunktnak lenniük).

Az új modellben a nevek a két modellben használt nevek uniója lesz az eredeti 
denotátumaikkal (nem ellenőrzi, hogy a mindkét modellben szereplő neveknek 
ugyanaz-e a denotátuma).

A kombinálandó modellekben nem lehetnek azonos nevű és argumentumszámú 
predikátumok, az új modellben a predikátumok mindegyike megjelenik.

Az új modellben az interpretációs világok a két modell interpretációs világainak 
a kombinációi jelennek meg, minden indexet minden másik modellbeli indexszel kibővítve, 
így ha az első modell *n*, a másik pedig *m* indexet tartalmazott, az új modell *n*x*m* 
indexből áll.

Az új modellben keletkezik egy olyan egyargumentumú predikátum is, 
amelynek minden interpretációs világban ugyanaz lesz a denotátuma: 
az új modell individuumainak a halmaza.


### copyModel
```
copyModel(ModelName, NewName)  
copyModel(NewName)
```

A *ModelName* nevű modellről vagy [az aktuális modellről](#changeactualmodel) 
készít egy másolatot *NewName* néven. Az eredei modellben levő *ModelName* 
nevű egyargumentumú predikátum [átneveződik](#renamepredicate) *NewName* 
névre, ezért csak akkor használható, ha korábban ilyen nevű predikátum nem 
volt a modellben. A másolás után a másolat lesz az aktuális modell.

### renameModel
```
renameModel(ModelName, NewName)  
renameModel(NewName)
```

Átnevezei a *ModelName* nevű modellt vagy [az aktuális modellt](#changeactualmodel) 
*newName* névre. Hatásában megegyezik a 
[`copyModell(ModelName, NewName),`](#copymodel) [`dropModel(ModelName)`](#dropmodel) 
parancskombinációval.

### changeActualModel
```
changeActuakModel(ModelName)
```

Kijelöli az aktuális modellt.

### actualModelName
```
actualModelName(ModelName)
```

Ellenőrzi, illetve lekérdezi az aktuális modell nevét, azonosítóját. 

### wirteWorlds
```
writeWorlds
```  
Megjeleníti az aktuális modell tulajdonságait.
```
?- writeWorlds.
There are 4 worlds in model beatles.
john --> john
paul --> paul
ringo --> ringo
index(i4): hungry=[john,ringo], beatles=[john,paul,ringo], 
index(i3): hungry=[john,paul,ringo], beatles=[john,paul,ringo], 
index(i2): hungry=[john,paul], beatles=[john,paul,ringo], 
index(i1): hungry=[john], beatles=[john,paul,ringo], 
true.
```

### writeWorld
```
writeWorld(ID)
```  
Megjeleníti az aktuális model egy világának tartalmát.
```
?- writeWorld(i4).
index(i4): hungry=[john,ringo], beatles=[john,paul,ringo], 
true .
```

### writeVocabulary
```
writeVocabulary
```  
Kilistázza az aktuális modellben található predikátumokat  
```
?- writeVocabulary.
[beatles,1]
[hungry,1]
true.
```

### writeWorldNumber
```
writeWorldNumber
```  
Kiírja az aktuális modellben található lehetséges világok számát.  
```
?- writeWorldNumber.
There are 4 worlds in model beatles.
true.
```



<a id="uj-nev"></a>

## Új név bevezetése a modellbe

### addNewName
```
addNewName(Name, Ind)
```
Hozzáadja a *Name* nevet az aktuális modellhez *Ind* referenciával. 

<a id="uj-predikatum"></a>

## Új predikátum bevezetése a modellbe
Új egy-, két- vagy háromargumentumú predikátum bevezetése során a modell 
predikátumokat felsoroló részében megjelenik az új predikátum, valamint 
a predikátum típusától függően a predikátum lehetséges interpretációi 
meghatározásra kerülnek. A modellben levő interpretációs világok az új predikátum 
lehetséges interpretációi szerint megtöbbszöröződnek.

Az új predikátum lehetséges interpretációinak meghatározása történhet a modell 
összes individuumára vonatkoztatott, vagy lehet az összes individuumának 
egy részhalmazára leszűkített - ez utóbbi esetben meg kell adni [a predikátum 
hatókörét ("scope")](#scope). Ha például a modell az `[a,b,c,d]` individumokat tartalmazza, 
akkor egy általános egyargumentumú predikátumnak 2<sup>4</sup> különböző 
interpretációja lehet, a modelluniverzum bármelyik részhalmaza. Ha viszont 
a predikátum hatókörét az `[a,b,c]` részhalmazra szűkítjük le, akkor ugyanennek 
a predikátumnak már csak 2<sup>3</sup> lehetséges interpretációja lesz. 

Az [`addNewPredicate`](#addnewpredicate) paranccsal általános predikátumokat adhatunk meg, 
az [`addNewOrdering`](#addnewordering) paranccsal kétargumentumú lineáris rendezéseket, 
az [`addNewClassifier`](#addnewclassifier) paranccsal pedig kétargumentumú ekvivalenciarelációkat 
a hozzá tartozó egyargumentumú ekvivalenciaosztályokkal.

### addNewPredicate
```
addNewPredicate(PredName, ArgNum)
addNewPredicate(PredName, ArgNum, Scope)
```
Új általános predikátum bevezetésénél mindig meg kell adni a predikátum elnevezését
(*PredName*) és az argumentumainak a számát (*ArgNum*), ami 1, 2 vagy 3 lehet. 
A predikátum az aktuális modellben kerül bevezetésre (lásd: [`actualModelName`](#actualmodelname)). 
A predikátum [hatókörét](#scope) a *Scope* argumentum határozza meg, az alapértelmezett 
hatókör a modelluniverzum.

Új általános predikátum bevezetése során a korábbi interpretációs világok, indexek 
kibővülnek a lehetséges interpretációival, vagyis az indexek megtöbbszöröződnek. 
Egyargumentumú predikátumok esetén a predikátum lehetséges interpretációi a hatókör 
részhalmazai, kétargumentumú predikátum esetén a hatókör(ök) Descartes szorzatának 
részhalmazai stb. 

### addNewOrdering
```
addNewOrdering(PredName)
addNewOrdering(PredName, Scope)
```
Új rendezés bevezetésénél csak a predikátum nevét kötelező megadni. 
Az új predikátum az aktuális modellben kerül bevezetésre (lásd: [`actualModelName`](#actualmodelname)). 
A predikátum [hatókörét](#scope) a *Scope* argumentum határozza meg, az alapértelmezett 
hatókör a modelluniverzum.
A rendezések mindig kétargumentumú predikátumok, a két argumentumra ugyanaz a 
hatóköri megszorítás érvényes.

Új rendezés bevezetése során a korábbi interpretációs világok, indexek kibővülnek 
a predikátum lehetséges interpretációival, vagyis az indexek megtöbbszöröződnek. 
A lehetséges interpretációkat a hatókör elemeinek lehetséges lineáris rendezései 
adják. 

Példa (hatókör megadása nélkül):
```
?- addNewOrdering(younger).
Predicate younger is added to model beatles.
There are 24 worlds in model beatles.
true.

?- writeWorld(i4).
index(i4): younger=[[ringo,john],[ringo,paul],[john,paul]], hungry=[john,ringo], beatles=[john,paul,ringo], 
true .
```

### addNewClassifier
```
addNewClassifier(PredName, ClassNames)
addNewClassifier(PredName, ClassNames, Scope)
addNewClassifier(PredName, ClassNames, noempty)
addNewClassifier(PredName, ClassNames, Scope, noempty)
```
Új osztályozó megadásával egy új, *PredName* nevű kétargumentumú 
ekvivalenciarelációt (reflexív, szimmetrikus, tranzitív) vezetünk be 
az aktuális modellbe (lásd: [`actualModelName`](#actualmodelname)).  
Az osztályozó előre megadott 
számú és nevű ekvivalenciaosztályba sorolja a hatókör elemeit. 
Az ekvivalenciosztályok neveit a *classNames* listán adhatjuk meg, 
az ekvivalenciaosztályok egyargumentumú predikátumként kerülnek bevezetésre.  
Az osztályozó predikátum [hatókörét](#scope) a *Scope* argumentum határozza meg, 
az alapértelmezett hatókör a modelluniverzum. 
Az osztályozók mindig kétargumentumú predikátumok, a két argumentumra 
ugyanaz a hatóköri megszorítás érvényes.  
A `noempty` konstans megadásával jelezhetjük, hogy a létrejövő 
ekvivalenciaosztályok nem lehetnek üresek. 

Új osztályozó bevezetése során a korábbi interpretációs világok, indexek kibővülnek 
az osztályozó lehetséges interpretációival, vagyis az indexek megtöbbszöröződnek. 
A lehetséges interpretációkat a hatókör elemeinek lehetséges osztályozásai adják. 
Az osztályozás során a hatókör elemeit szétbontjuk diszjunkt 
ekvivalenciaosztályokra, az ekvivalenciaosztályok uniója a hatókört adja meg. 
Az egyes ekvivalenciaosztályok lehetnek üres halmazok is.

Példák (hatókör megadása nélkül):
```
?- addNewClassifier(sameSex, [female, male]).
Predicate sameSex is added to model beatles.
There are 192 worlds in model beatles.
true.

?- writeWorld(i4).
index(i4): sameSex=[[john,john],[john,ringo],[ringo,john],[ringo,ringo],[paul,paul]], female=[john,ringo], male=[paul], younger=[[ringo,john],[ringo,paul],[paul,john]], hungry=[john,ringo], beatles=[john,paul,ringo], 
true .
```

### deletePredicate
```
deletePredicate(PredName,ArgNum)
deletePredicate(PredName)
```
A megadott nevű és argumentumszámú predikátumot törli a modellből. 
Ha nincs megadva az argumentumszám, akkor a legutoljára bevezetett predikátum 
törlődik.

A predikátum törlése során minden egyes lehetséges világból eltávolításra 
kerül az adott predikátum, és azon lehetséges világok közül, amelyek ezután 
megkülönböztethetetlenek, vagyis minden maradék predikátum denotátuma azonos, 
csak egy marad meg.
```
?- deletePredicate(sameSex,2).
Predicate sameSex is deleted from model beatles.
There are 192 worlds in model beatles.
true .

?- writeWorld(i4).
index(i4): female=[john,ringo], male=[paul], younger=[[ringo,john],[ringo,paul],[paul,john]], boy=[john,ringo], beatles=[john,paul,ringo], 
true .
```

### renamePredicate
```
renamePredicate(OldName,ArgNum,NewName)
renamePredicate(OldName,NewName)
```
A `renamePredicate` segítségével egy predikátum nevét változtathatjuk meg. 
Ha nincs megadva az argumentumszám, akkor a legutoljára bevezetett predikátum 
kerül átnevezésre.  
Az új predikátum argumentumszáma megegyezik az eredeti predikátum 
argumentumszámával.  
Az új predikátum denotátuma minden lehetséges világban ugyanaz lesz, mint az 
eredeti predikátumé volt. 

<a id="scope"></a>

### A hatókör megadása
A predikátumok hatókörét megadhatjuk az összes argumentumra vonatkozóan, 
vagy az argumentumokra külön-külön is. Ez utóbbi esetben megadhatjuk, hogy pl. 
egy kétargumentumú predikátum első argumentuma az `[a,b,c]` halmaz elemei 
közül, a második argumentuma viszont a `[c,d,e]` halmaz elemei közül 
kerüljenek ki. Ha többargumentumú predikátum esetében csak egy hatókört 
adunk meg, akkor az az összes argumentumra vonatkozik. Rendezések és 
osztályozók esetén ugyanaz a hatóköre az összes argumentumnak. 

A hatókör megadása lehet abszolút, vagyis minden világban azonos, 
vagy relatív, azaz világról világra változó. 

**Abszolút** hatókörmegadásnál 
felsoroljuk a hatókört alkotó individuumokat: 
```
addNewPredicate(see, 2, [[john,ringo],[paul,george]]).
```
Az első példában az aktuális modellben bevezettük a *see* kétargumentumú 
predikátumot, mégpedig úgy, hogy csak *john* és *ringo* láthatja *paul*-t 
vagy *george*-ot. Itt a hatókört individuumhalmazok listájaként adtuk meg. 

```
addNewPredicate(know, 2, [john,ringo]).
```
A második példában a *know* kétargumentumú predikátum mindkét argumentumára 
ugyanaz a hatókör vonatkozik, ezt az individuumhalmaz egyszeri megadásával 
jeleztük.

**Relatív** hatókörmegadásnál egy szabad változót tartalmazó logikai 
kifejezést adunk meg, amely minden interpretációs világban kiértékelésre 
kerül, és a hatókört azok az individuumok alkotják, amelyekkel a szabad 
változót értelmezve igazzá teszik a kifejezést. A relatív hatókört az 
`l_x(X,Form)` vagy `X: Form` formában adhatjuk meg, ahol *Form* egy 
szabad változót tartalmazó logikai kifejezés, *X* pedig a benne található 
szabad változó. Ez valójában egy *λx.φ(x)* formula megadását jelenti. 

```
addNewPredicate(bald, 1, l_x(Z, male(Z)))
```
A példában a *bald* 'kopasz' egyargumentumú predikátumot vezettük be, 
de az egyes interpretációs világokban csak 
azok a *Z* individuumok lehettek kopaszak, amelyek a *male(Z)* kifejezést 
igazzá teszik, vagyis a hímneműek.

```
addNewPredicate(chase, 2 [X: dog(X), Y: cat(Y)])
```
A példában bevezetett *chase* 'kerget' kétargumentumú predikátum 
első argumentumai kutyák (*dog*), a második argumentumai pedig csak 
macskák (*cat*) lehetnek. A két hatóköri kifejezésben különböző 
változókat kell használni! 
A logikai kifejezések lehetnek sokkal bonyolultabbak is.

<a id="kiertekeles"></a>

## Logikai formula kiértékelése egy modellben
### eval
```
eval(Form, I)
```
Az aktuális modell minden interpretációs 
világában kiértékeli a *Form* logikai kifejezést, és amelyikben igaz, 
azokat a világokat, indexeket visszaadja az *I* listában. 
Az *I* magát a tényleges háttér-reprezentációt tartalmazza. Az indexek azonosítóit 
a [`writeIndices`](#writeindices),  az egyes indexek tartalmát pedig a [`writeWorld`](#writeworld) 
paranccsal érdemes kiíratni. 

### and
```
and(Form)
```
Az aktuális modell minden interpretációs 
világában kiértékeli a *Form* logikai kifejezést, és amelyikben nem igaz, 
azokat a világokat törli a modell interpretációs világai közül. 

Ezzel a paranccsal tudunk új információt adni a dinamikus modellnek. Az új 
információ megadásával csökkentjük a lehetséges világok számát, vagyis egyre 
jobban leszűkítjük, hogy milyen lehet az információkkal leírt világ.

### writeIndices

<a id="logikai-kifejezesek"></a>

## Használható logikai kifejezések
A program elsőrendű predikátumlogikai kifejezéseket tud értelmezni. 
Az individuumokra nevekkel, változókkal vagy *iota* operátoros kifejezésekkel tudunk referálni. 
A változók nagy betűvel kezdődő, betűt, számot és aláhúzást tartalmazó kifejezések. 
Az individuumokra referáló kifejezések a *Term*-ek. 
A predikátumokból és a megfelelő számú *Term*-ből elemi állításokat hozhatunk 
létre: a predikátum nevéhez tapadó nyitó zárójel után vesszőkkel elválasztva 
felsoroljuk a *Term*-eket, majd csukó zárójellel fejezzük be: `see(john, X)`

A logikai kijelentéseket, *Form*-okat etekkel kapcsolhatjuk egymáshoz. 
A logikai műveletek prefixáltak, vagyis a logikai művelet is egyfajta 
predikátumként viselkedik, aminek a kijelentések az argumentumai. 
A kvantorok is prefixáltak, a kvantor neve után zárójelben következnek a kvantor 
által lekötött változó, majd a kvantor hatókörében levő (nyitott) kijelentés. 

### not
```
not(Form)
~ Form
```
A *Form* kijelentés tagadása, pl.: `not(see(john,X))`  vagy 
`~ see(john,X)` 'John nem látja X-et'.  

Az infix negációs műveleti jel (`~`) nem lehet közvetlenül szomszédos 
(azaz szóköz nélkül) az infix `& | -> ~` műveleti jelekkel, illetve nem 
követheti zárójel nélkül a `:` jelet (de zárójellel igen, pl. `all X: (~p(X))`)

### and
```
and(Form1, Form2)
Form1 & Form2
```
*Form1* és *Form2* konjunkciója, pl.: `and(male(john), see(john,X))` vagy 
`male(john) & see(john,X)` 'John férfi és John látja X-et'.  

A `& | ->` műveletek jobb-asszociatívak, azaz zárójelezés nélkül a jobb oldali 
műveletek végződnek el először: a `(p & q | r -> p` kifejezés 
`and(p, or(q, imp(r, p)))`-ként értelmeződik.

### or
```
or(Form1,Form2)
Form1 | Form2
```
*Form1* és *Form2* diszjunkciója, pl.: `or(male(john), see(john,X))` vagy 
`male(john) | see(john,X)` 'John férfi vagy John látja X-et'. (megengedő vagy) 

A `& | ->` műveletek jobb-asszociatívak, azaz zárójelezés nélkül a jobb oldali 
műveletek végződnek el először: a `(p & q | r -> p` kifejezés 
`and(p, or(q, imp(r, p)))`-ként értelmeződik.

### imp
```
imp(Form1,Form2)
Form1 -> Form2
```
*Form1* és *Form2* kondicionálisa vagy implikációja, pl.: `imp(male(john), see(john,X))` vagy 
`male(john) -> see(john,X)` 'ha John férfi, akkor John látja X-et'.

A `& | ->` műveletek jobb-asszociatívak, azaz zárójelezés nélkül a jobb oldali 
műveletek végződnek el először: a `(p & q | r -> p` kifejezés 
`and(p, or(q, imp(r, p)))`-ként értelmeződik.


### all
```
all(X,Form)
all X: Form
```
Univerzális kvantor, az *X* változót köti a *Form* kijelentésben 

pl.: `all(X, imp(male(X), see(X,john)))` vagy `all X: (male(X) -> see(X,john))` 
'Minden férfi látja Johnt'. (∀x.(male(x) → see(x,John))) 

A változó mindig nagy betűvel kezdődik.  
Az infix univerzális kvantor precedenciája nagyobb, mint a `& | ->` műveleteké, 
vagyis a zárójel nélküli `all X: male(X) -> see(X,john)` kifejezésben a kvantor 
csak a `male(X)` kifejezésben köti *X*-et, a `see(X,john)` kifejezésben nem.

### some
```
some(X,Form)
some X: Form
```
Egzisztenciális kvantor, az *X* változót köti a *Form* kijelentésben 

pl.: `some(X, and(male(X), see(X,john)))` vagy `some X: (male(X) & see(X,john))` 
'Egy férfi látja Johnt'. (∃x.(male(x) & see(x,John))) 

A változó mindig nagy betűvel kezdődik.  
Az infix egzisztenciális kvantor precedenciája nagyobb, mint a `& | ->` műveleteké, 
vagyis a zárójel nélküli `some X: male(X) & see(X,john)` kifejezésben a kvantor 
csak a `male(X)` kifejezésben köti *X*-et, a `see(X,john)` kifejezésben nem.

### eq
```
eq(Term1, Term2)
Term1 = Term2
```
Kétargumentumú predikátum, akkor igaz, ha a két term ugyanarra 
az individuumra referál. pl.: `eq(john, iota(X, singer(X)))` vagy 
`john = iota X: singer(X)` 'John az énekes' 

### iota
```
iota(X, Form)
iota X: Form
```
Arra az egyetlen individuumra utaló kifejezés (*Term*), amely *X*-ként 
a *Form* kijelentést igazzá teszi: ióta operátor. 

*Form*-ban az *X* az egyetlen szabad változó, így egy individuumhalmazt 
határoz meg. Ha ez az individuumhalmaz egyelemű, akkor az *iota* kifejezés 
erre az egyetlen elemre referál. Ha az individuumhalmaz nem egyelemű 
(üres, vagy több elemű), akkor az *iota* kifejezés nem referál semmire, 
az ilyen *Term*-et tartalmazó kijelentés hamis.

A természetes nyelvi *a/az* határozott névelőnek felel meg: 
`iota(X, singer(X))` 'az énekes'.

Az infix ióta operátor precedenciája nagyobb, mint a `& | ->` műveleteké, 
vagyis a zárójel nélküli `iota X: male(X) & see(X,john)` (szándékolt jelentés: 
'az a férfi, aki látja Johnt') kifejezésben az operátor 
csak a `male(X)` kifejezésben köti *X*-et, a `see(X,john)` kifejezésben nem.
