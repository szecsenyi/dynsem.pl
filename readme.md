# dynsem.pl 

Dinamikus szemantikai modellek építése, amely modellek több 
interpretációs világot tartalmaznak, és a diskurzus során 
megfogalmazott új információk, logikai kifejezések leszűkítik 
az interpretációs világok halmazát. 

* A program részei, futtatás
* A dinamikus modellreprezentácó
* A használható parancsok leírása

# A program részei, futtatás
A program Prolog nyelven íródott, kipróbálva 
az [SWI-Prolog](https://www.swi-prolog.org/) 8.0.1 verzióján lett. 

A program három fájlból áll. A *modellChecker3.pl* és a *comsemPredicates.pl* 
fájlok [Patrick Blackburn és Johan Bos *Representation and Inference for 
Natural Language. A First Course in Computational Semantics*](http://www.let.rug.nl/bos/comsem/book1.html) 
című könyvéhez tartozó programkönyvtár két darabjának kibővített változata, 
a harmadik pedig fájl pedig maga a főprogram, a *dynsem.pl*. 
A programcsomaghoz tartozik még az *examples.pl* nevű, példamodelleket 
tartalmazó fájl is. 
TODO

A program futtatása a *dynsem.pl* fájlnak az SWI-Prolog értelmezőbe való 
betöltésével történik. A parancsokat a prolog értelmezőben megjelenő `?-` 
prompt után kell beírni. A parancsokat lehet egyenként is futattni, 
vagy egyszerre több parancsot is megadva, azokat egymás után, vesszővel beírva. 
A beírt parancsokat a `.` jel beírásával kell zárni, majd entert nyomni.
```
?- createModel(beatles, [john, paul, ringo], []).
...
?- addNewPredicate(model:beatles, boy, 1), writeWorlds.
```

A parancsok megadását először egy dinamikus szemantikai modell létrehozásával 
célszerű kezdeni (pl.: `createModel(beatles, [john, paul, ringo], [])`), 
majd új nevek, predikátumok bevezetésével 
(pl.: `addNewPredicate(model:beatles, boy, 1)`), 
illetve a világra vonatkozó új információk megadásával lehet folytatni (pl.: 
`and(beatles, boy(john))`. 
Új predikátumok bevezetésével növelhetjük, új információk megadásával 
csökkenthetjük az interpretációs világok számát. 
Eközben megjeleníthetjük a modellt, vagyis az interpretációs világokat 
a `writeWorlds` paranccsal.

```
?- createModel(beatles, [john, paul, ringo], []).
There is only 1 world in model beatles.
true .

?- addNewPredicate(model:beatles, boy, 1), writeWorlds.
There are 8 worlds in model beatles.
There are 8 worlds in model beatles.
john --> john
paul --> paul
ringo --> ringo
index(i7): boy=[ringo], beatles=[john,paul,ringo], 
index(i6): boy=[paul,ringo], beatles=[john,paul,ringo], 
index(i5): boy=[paul], beatles=[john,paul,ringo], 
index(i4): boy=[john,ringo], beatles=[john,paul,ringo], 
index(i3): boy=[john,paul,ringo], beatles=[john,paul,ringo], 
index(i2): boy=[john,paul], beatles=[john,paul,ringo], 
index(i1): boy=[john], beatles=[john,paul,ringo], 
index(i0): boy=[], beatles=[john,paul,ringo], 
true .

?- and(beatles, boy(john)).
There are 4 worlds in model beatles.
true.

?- writeWorlds.
There are 4 worlds in model beatles.
john --> john
paul --> paul
ringo --> ringo
index(i4): boy=[john,ringo], beatles=[john,paul,ringo], 
index(i3): boy=[john,paul,ringo], beatles=[john,paul,ringo], 
index(i2): boy=[john,paul], beatles=[john,paul,ringo], 
index(i1): boy=[john], beatles=[john,paul,ringo], 
true.

?-
```

# A dinamikus modellreprezentáció
A modellek öt részből állnak: `model(ModelID,D,Preds,Names,I)`
- `ModelID`: a modell azonosítója vagy neve - atom
- `D`: a modellben található individuumok halmaza, modelluniverzum - lista
- `Preds`: a modellben használható predikátumok nevei és argumentumszáma - lista
- `Names`: a modellben használható nevek és azok denotátuma (mely individuumra utal) - lista
- `I`: a modellben lehetséges interpretációs világok, 'indexek' - lista

A modell azonosítója és az individuumok halmaza a modell létrehozása 
után nem változtatható, a predikátumok és nevek listája viszont bővíthető. 
Az indexek automatikusan változnak, explicit módosításuk nem lehetséges. 

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

### Predikátumok: Preds
A predikátumok az argumentumuk számával rendezett párt alkotva listaként 
vannak megadva, pl.: `[[name1, 0], [name2, 0], [pred1, 1], [pred2, 1], [pred3,2]]` 
A modell létrehozásakor a modell nevével megegyező nevű egyargumentumú 
predikátum is deklarálódik, amelynek a denotátuma a modell összes individuumát 
tartalmazza minden egyes interpretációs világban. 
A program egy-, két és háromargumentumú predikátumokat tud kezelni. 
A már deklarált predikátumok nem törölhetőek, de újak deklarálhatóak: 
[`addNewPredicate`](#addnewpredicate), 
[`addNewOrdering`](#addnewordering), 
[`addNewClassifier`](#addnewclassifier) 

### Interpretációs világok: I
Egy modellhez tartozó interpretációs világok vagy indexek listaként vannak megadva. 
Az egyes indexek `index(Num, ID, Interperations)` információhármassal 
vannak jellemezve: az első az indexvilágnaknek a sorszáma (automatikusan számozott), 
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
jönnek létre.

# A használható parancsok leírása
* Modellek létrehozása, egyesítése és törlése, lekérdezése
* Új név bevezetése a modellbe
* Új predikátum bevezetése a modellbe
* Logikai formula kiértékelése egy modellben
* Használható logikai kifejezések

## Modellek létrehozása, egyesítése és törlése, lekérdezése
### createModel
`createModel(modelName, D, Preds, Names)` 

Létrehoz egy *modelName* azonosítójú modellt *D*-ben felsorolt individuumokkal. 

A *Preds* argumentumként megadhatunk egy-, két- és háromargumentumú predikátumokat 
az argumentumszámaikkal együtt. Ha nem akarunk a modell létrehozásakor predikátumokat 
megadni, akkor `[]` üres listát használhatunk. A modell létrehozásakor a modell 
nevével megegyező egyargumentumú predikátum is bevezetődik.

A *Names* paraméterben a modellben használandó neveket adhatjuk meg a denotátumiakkal. 
Ha nem akarunk a modell létrehozásakor neveket megadni, akkor `[]` üres listát 
használhatunk. A *D* individuumhalmaz elemei névként is használhatóak, de azokat 
nem kell itt felsorolni. A nevek a modellben nullaargumentumú predikátumként lesznek 
kezelve, de a predikátumok között nem kell felsorolni őket.

Példák:
```
createModel(beatles, [john, george, paul, ringo], [[boy,1],[hungry,1],[see,2]] [[john_lennon, john]])
createModel(test, [a,b,c],[],[])
```
A modell létrehozása során generálódnak az interpretációs világok, indexek, a következő módon: 
az indexek a használt (nem nullaargumentumú) predikátumok mindegyikéhez rendelnek 
valamilyen interpretációt. A modell nevével megegyező egyargumentumú predikátum interpretációja 
minden indexben a *D* halmazzal egyezik meg. Az egyargumentumú predikátumok interpretációja 
*D*-nek valamely részhalmaza lesz, a kétargumentumú predikátumok interpretációja *D*x*D* 
valamely részhalmaza, a háromargumentumúaké pedig *D*x*D*x*D* valamely részhalmaza. 
A különböző indexekben a predikátumok interpretációi közül legalább egy különböző, így 
az indexek száma a lehetséges interpretációk összes lehetséges kombinációjával egyezik meg. 
A példa *beatles* modelljében 4 individuum található, ezért az egyargumentumú predikátumoknak 
2<sup>4</sup>, a kétargumentumú predikátumoknak pedig 2<sup>4x4</sup> különböző interpretációja 
lehetséges, az indexek száma így 2<sup>4</sup>x2<sup>4</sup>x2<sup>4x4</sup> = 2<sup>24</sup> lesz.

### readModel, writeModel
ilyen még nincs, de tervben van: fájlba ment ki, ill. abból olvas be modellt.

TODO

### dropModel
`dropModel(modelName)`

A megadott nevű modellt "elfelejti".

TODO: ha az aktuális modellt dobjuk el, mi lesz az új aktuális? null?

### combineModels
`combineModels(model1, model2, newModel)`

A *model1* és a *model2* azonosítójú modelleket egyesíti egy új modellben, amelynek 
a neve *newModel* lesz.

Az új modellben az individuumok halmaza két kombinált modell individuumainak 
az uniója lesz (nem kell azonosnak vagy diszjunktnak lenniük).

Az új modellben a nevek a két modellben használt nevek uniója lesz az eredeti 
denotátumaikkal (nem ellenőrzi, hogy a mindkét modellben szereplő neveknek 
ugyanaz-e a denotátuma).

A kombinálandó modellekben nem lehetnek azonos nevű és argumentumszámú 1-2-3 argumentumú 
predikátumok, az új modellben a predikátumok mindegyike megjelenik.

Az új modellben az interpretációs világok a két modell interpretációs világainak 
a kombinációi jelennek meg, minden indexet minden másik modellbeli indexszel kibővítve, 
így ha az első modell *n*, a másik pedig *m* indexet tartalmazott, az új modell *n*x*m* 
indexből áll.

Terv: meg lehetne nézni, hogy hogyan lehetne olyan modelleket kombinálni, amelyek 
ugyanolyan predikátumokat is tartalmaznak: ekkor csak azokat az indexeket kellene 
kombinálni, amelyekben a közös predikátumok ugyanolyan interpretációval rendelkeznek.

### actualModelName
`actualModelName(ModelName)`

Ellenőrzi, illetve lekérdezi az aktuális modell nevét, azonosítóját. 

### changeActualModel
`changeActuakModel(modelName)`

Kijelöli az aktuális modellt.

## Új név bevezetése a modellbe
### addNewName
```
addNewName(modelName, name, ind)
addNewName(name, ind)
```
Hozzáadja a *name* nevet a megadott azonosítójú modellhez *ind* referenciával. 
Ha nincs megadva a modell azonosítója, akkor az aktuális modellhez adja.

## Új predikátum bevezetése a modellbe
Új egy-, két- vagy háromargumentumú predikátum bevezetése során a modell 
predikátumokat felsoroló részében megjelenik az új predikátum, valamint 
a predikátum típusától függően a predikátum lehetséges interpretációi 
meghatározásra kerülnek. A modellben levő interpretációs világok az új predikátum 
lehetséges interpretációi szerint megtöbbszöröződnek.

Az új predikátumlehetséges interpretációinak meghatározása történhet a modell 
összes individuumára vonatkoztatott, vagy lehet az összes individuumának 
egy részhalmazára leszűkített - ez utóbbi esetben meg kell adni a predikátum 
hatókörét ("scope"). Ha például a modell az `[a,b,c,d]` individumokat tartalmazza, 
akkor egy általános egyargumentumú predikátumnak 2<sup>4</sup> különböző 
interpretációja lehet, a modelluniverzum bármelyik részhalmaza. Ha viszont 
a predikátum hatókörét az `[a,b,c]` részhalmazra szűkítjük le, akkor ugyanennek 
a predikátumnak már csak 2<sup>3</sup> lehetséges interpretációja lesz. 

Az `addNewPredicate` paranccsal általános predikátumokat adhatunk meg, 
az `addNewOrdering` paranccsal kétargumentumú lineáris rendezéseket, 
az `addNewClassifier` paranccsal pedig kétargumentumú ekvivalenciarelációkat 
a hozzá tartozó egyargumentumú ekvivalenciaosztályokkal.

### addNewPredicate
```
addNewPredicate(predName, argNum)
addNewPredicate(predName, argNum, Scope)
addNewPredicate(model:modelName, predName, argNum)
addNewPredicate(model:modelName, predName, argNum, Scope)
```
Új általános predikátum bevezetésénél mindig meg kell adni a predikátum elnevezését
(*predName*) és az argumentumainak a számát (*argNum*), ami 1, 2 vagy 3 lehet. 
A predikátum az aktuális modellben kerül bevezetésre (lásd: `actualModelName`), 
más modellre vonatkozóan a `model:` prefixum után adhatjuk meg a modell azonosítóját. 
A predikátum hatókörét a *Scope* argumentum határozza meg, az alapértelmezett 
hatókör a modelluniverzum.

Új általános predikátum bevezetése során a korábbi interpretációs világok, indexek 
kibővülnek a lehetséges interpretációival, vagyis az indexek megtöbbszöröződnek. 
Egyargumentumú predikátumok esetén a predikátum lehetséges interpretációi a hatókör 
részhalmazai, kétargumentumú predikátum esetén a hatókör(ök) Descartes szorzatának 
részhalmazai stb. 

### addNewOrdering
```
addNewOrdering(predName)
addNewOrdering(predName, Scope)
addNewOrdering(model:modelName, predName)
addNewOrdering(model:modelName, predName, Scope)
```
Új rendezés bevezetésénél csak a predikátum nevét kötelező megadni. 
A modell, amiben bevezetésre kerül az új rendezés, alapértelmezésben 
az aktuális modell (lásd: `actualModelName`), más modellre vonatkozóan a `model:` 
prefixum után adhatjuk meg a modell azonosítóját.
A predikátum hatókörét a *Scope* argumentum határozza meg, az alapértelmezett 
hatókör a modelluniverzum.
A rendezések mindig kétargumentumú predikátumok, a két argumentumra ugyanaz a 
hatóköri megszorítás érvényes.

Új rendezés bevezetése során a korábbi interpretációs világok, indexek kibővülnek 
a predikátum lehetséges interpretációival, vagyis az indexek megtöbbszöröződnek. 
A lehetséges interpretációkat a hatókör elemeinek lehetséges lineáris rendezései 
adják. 

Példa (hatókör megadása nélkül):
```
addNewOrdering(model:beatles, older)
```

### addNewClassifier
```
addNewClassifier(predName, ClassNames)
addNewClassifier(predName, ClassNames, Scope)
addNewClassifier(model:modelName, predName, ClassNames)
addNewClassifier(model:modelName, predName, ClassNames, Scope)
```
Új osztályozó megadásával egy új, *predName* nevű kétargumentumú 
ekvivalenciarelációt (reflexív, szimmetrikus, tranzitív) vezetünk be 
a megadott modellbe: alapértelmezésben az aktuális modellbe 
(lásd: `actualModelName`), más modellre vonatkozóan a `model:` 
prefixum után adhatjuk meg a modell azonosítóját. Az osztályozó előre megadott 
számú és nevű ekvivalenciaosztályba sorolja a hatókör elemeit. 
Az ekvivalenciosztályok neveit a *ClassNames* listán adhatjuk meg, 
az ekvivalenciaosztályok egyargumentumú predikátumként kerülnek bevezetésre. 
Az osztályozó predikátum hatókörét a *Scope* argumentum határozza meg, 
az alapértelmezett hatókör a modelluniverzum.
Az osztályozók mindig kétargumentumú predikátumok, a két argumentumra 
ugyanaz a hatóköri megszorítás érvényes.

Új osztályozó bevezetése során a korábbi interpretációs világok, indexek kibővülnek 
az osztályozó lehetséges interpretációival, vagyis az indexek megtöbbszöröződnek. 
A lehetséges interpretációkat a hatókör elemeinek lehetséges osztályozásai adják. 
Az osztályozás során a hatókör elemeit szétbontjuk diszjunkt 
ekvivalenciaosztályokra, az ekvivalenciaosztályok uniója a hatókört adja meg. 
Az egyes ekvivalenciaosztályok lehetnek üres halmazok is.

Példák (hatókör megadása nélkül):
```
addNewClassifier(model:school, classmate, [first, second, third])
addNewClassifier(model:people, sameSex, [female, male])
```

### A hatókör megadása
A predikátumok hatókörét megadhatjuk az összes argumentumra vonatkozóan, 
vagy az argumentumokra külön-külön is. Ez utóbbi esetben megadhatjuk, hogy pl. 
egy kétargumentumú predikátum első argumentuma az \[a,b,c\] halmaz elemei 
közül, a második argumentuma viszont a \[c,d,e\] halmaz elemei közül 
kerüljenek ki. Ha többargumentumú predikátum esetében csak egy hatókört 
adunk meg, akkor az az összes argumentumra vonatkozik. Rendezések és 
osztályozók esetén ugyanaz a hatóköre az összes argumentumnak. 

A hatókör megadása lehet abszolút, vagyis minden indexre azonos, 
vagy relatív, azaz indexről indexre változó. 

**Abszolút** hatókörmegadásnál 
felsoroljuk a hatókört alkotó individuumokat: 
```
addNewPredicate(model:beatles, see, 2, [[john,ringo],[paul,george]]).
```
Az első példában a *beatles* modellben bevezettük a *see* kétargumentumú 
predikátumot, mégpedig úgy, hogy csak *john* és *ringo* láthatja *paul*-t 
vagy *george*-ot. Itt a hatókört individuumhalmazok listájaként adtuk meg. 

```
addNewPredicate(model:beatles, know, 2, [john,ringo]).
```
A második példában a *know* kétargumentumú predikátum mindkét argumentumára 
ugyanaz a hatókör vonatkozik, ezt az individuumhalmaz egyszeri megadásával 
jeleztük.

**Relatív** hatókörmegadásnál egy szabad változót tartalmazó logikai 
kifejezést adunk meg, amely minden interpretációs világban kiértékelésre 
kerül, és a hatókört azok az individuumok alkotják, amelyekkel a szabad 
változót értelmezve igazzá teszik a kifejezést. A relatív hatókört az 
`l_x(X,Form)` formában adhatjuk meg, ahol *Form* egy szabad változót tartalmazó 
logikai kifejezés, *X* pedig a benne található szabad változó. 
Ez valójában egy *λx.φ(x)* formula megadását jelenti. 

```
addNewPredicate(model:people, bald, 1, l_x(Z, male(Z)))
```
A példában a *people* modellben vezettük be a *bald* 'kopasz' 
egyargumentumú predikátumot, de az egyes interpretációs világokban csak 
azok a *Z* individuumok lehettek kopaszak, amelyek a *male(Z)* kifejezést 
igazzá teszik, vagyis a hímneműek.

```
addNewPredicate(model:animals, chase, 2 [l_x(X, dog(X)), l_x(Y, cat(Y))])
```
A példában bevezetett *chase* 'kerget' kétargumentumú predikátum 
első argumentumai kutyák (*dog*), a második argumentumai pedig csak 
macskák (*cat*) lehetnek. A két hatóköri kifejezésben különböző 
változókat kell használni! 
A logikai kifejezések lehetnek sokkal bonyolultabbak is.

## Logikai formula kiértékelése egy modellben
### eval
```
eval(Form, I)
eval(modelName, Form, I)
```
Az aktuális modell vagy a *modelName* azonosítójú modell minden interpretációs 
világában kiértékeli a *Form* logikai kifejezést, és amelyikben igaz, 
azokat a világokat, indexeket visszaadja az *I* listában.
Az *I* magát a tényleges háttér-reprezentációt tartalmazza, a kiíratását a 
`writeWorld` paranccsal érdemes csinálni.

### and
```
and(Form)
and(modelName, Form)
```
Az aktuális modell vagy a *modelName* azonosítójú modell minden interpretációs 
világában kiértékeli a *Form* logikai kifejezést, és amelyikben nem igaz, 
azokat a világokat törli a modell interpretációs világai közül. 

Ezzel a paranccsal tudunk új információt adni a dinamikus modellnek. Az új 
információ megadásával csökkentjük a lehetséges világok számát, vagyis egyre 
jobban leszűkítjük, hogy milyen lehet az információkkal leírt világ.

## Használható logikai kifejezések
A program elsőrendű predikátumlogikai kifejezéseket tud értelmezni. 
Az individuumokra nevekkel, változókkal vagy *iota* kifejezésekkel tudunk referálni. 
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
```
A *Form* kijelentés tagadása, pl.: `not(see(john,X))` 
'John nem látja X-et'.

### and
```
and(Form1, Form2)
```
*Form1* és *Form2* konjunkciója, pl.: `and(male(john), see(john,X))` 
'John férfi és John látja X-et'.

### or
```
or(Form1,Form2)
```
*Form1* és *Form2* diszjunkciója, pl.: `or(male(john), see(john,X))` 
'John férfi vagy John látja X-et'. (megengedő vagy)

### imp
```
imp(Form1,Form2)
```
*Form1* és *Form2* kondicionálisa vagy implikációja, pl.: `imp(male(john), see(john,X))` 
'ha John férfi, akkor John látja X-et'. 

### all
```
all(X,Form)
```
Univerzális kvantor, az *X* változót köti a *Form* kijelentésben 

pl.: `all(X, imp(male(X), see(X,john)))` 
'Minden férfi látja Johnt'. (∀x.(male(x) → see(x,John))) 

A változó mindig nagy betűvel kezdődik.

### some
```
some(X,Form)
```
Egzisztenciális kvantor, az *X* változót köti a *Form* kijelentésben 

pl.: `some(X, and(male(X), see(X,john)))` 
'Egy férfi látja Johnt'. (∃x.(male(x) & see(x,John))) 

A változó mindig nagy betűvel kezdődik.

### eq
```
eq(Term1, Term2)
```
Kétargumentumú predikátum, akkor igaz, ha a két term ugyanarra 
az individuumra referál.

pl.: `eq(john, iota(X, singer(X)))` 'John az énekes'

### iota
```
iota(X, Form)
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


```python

```
