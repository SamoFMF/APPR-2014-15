# Analiza podatkov s programom R, 2014/15

Avtor: Samo Metličar

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2014/15.

## Naslov

McDonald's

## Osnovna ideja ter plan dela

Nameravam upoštevati ter primerjati hranilne vrednosti ponudbe, kakor tudi ponudbo samo po različnih državah, kot tudi razširjenost same znamke po Sloveniji in svetu, ter gibanje njihove vrednosti.

## Opis podatkovnih virov

McDonaldsova spletna stran, Wikipedia, Forbes ter drugi viri, ki jih bom še dodal tekom dela (kot tudi same povezave).

http://nutrition.mcdonalds.com/getnutrition/nutritionfacts.pdf
http://nutrition.mcdonalds.com/getnutrition/ingredientslist.pdf
http://markets.ft.com/research/Markets/Tearsheets/Summary?s=MCD:NYQ
http://www.macroaxis.com/invest/ratio/MCD--Number-of-Employees
http://en.wikipedia.org/wiki/List_of_countries_with_McDonald%27s_restaurants

## Tematika

Izbrali si boste temo, s katero se bo vaš projekt ukvarjal. Tukaj boste
napisali, kje ste dobili podatke, ter kakšen je vaš cilj.

## Uporaba

Zaženemo datoteko `projekt.r` v RStudio ter poženemo program. Izvedle se bodo vse funkcije in dobili bomo tri tabele ter tri grafe (shranjene v `slike/graf1.pdf`, `slike/graf2.pdf`, `slike/graf3.pdf`) ter zemljevid (shranjen v `slike/zemljevid2.pdf`), da dobimo še drugi zemljevid moramo označiti vrstice 11-15 ter klikniti `Run` (shrani se nam v `slike/zemljevid1.pdf`).

Nujno si prenesite sliko iz http://www2.pef.uni-lj.si/kemija/profiles/images/uni-lj.png ter jo shranite v ../slike/uni-lj.png, saj drugače ne bo prva stran poročila vsebovale logotipa univerze.

## Program

Glavni program se nahaja v datoteki `projekt.r`. Ko ga poženemo, se izvedejo
programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/` ter `vizualizacija`. Podatkovni
viri so v mapi `podatki/`. Slike, ki jih program naredi, se shranijo v mapo
`slike/`. Zemljevidi v obliki SHP, ki jih program pobere, se shranijo v mapo
`zemljevid/`.

## Poročilo

Poročilo se nahaja v mapi `porocilo/`. Za izdelavo poročila v obliki PDF je
potrebno datoteko `porocilo/porocilo.tex` prevesti z LaTeXom. Pred tem je
potrebno pognati program, saj so v poročilu vključene slike iz mape `slike/`.