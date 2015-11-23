#Abiskript, kuidas funktsioonile fromJsonLong tekitada loetelu andmetest, mida
#tahame saada. nt:
#'2014.Letiteenus.link'=jstring('teenuste_kanalid_ja_moodikud','2014',\
#'Letiteenus','link'),
#'2014.Kliendi juures.link'=jstring('teenuste_kanalid_ja_moodikud','2014',\
#'Kliendi juures','link')

#Kuna loopimine spread_values funktsiooni puhul loopimine välja ei tulnud, 
#tegin kiire lahenduse kuidas faili luua loetelu, mida funktsiooni käsitsi üle
#kopida.

#Esiteks nimekiri kanalitest, aastatest ja näitajatest. Peab olema nii nagu
#algselt failis on kirjas, vstasel juhul ei leia vastet
kanalid=c("Veebileht / portaal","E-iseteenindus", "Eesti.ee","Nutirakendus",
          "Digitelevisioon", "E-post","Tekstisõnum","Telefon", "Faks","Post", 
          "Letiteenus","Kliendi juures")
aastad=c("empty", "2014")
naitajad=c("osutamistearv", "rahulolu", "halduskulu", "ajakulu")

#Loobime abimuutujad statistika, mille põhjalt saab muutujate nimed valmis teha
nimek=c()
aasta=c()
kanal=c()
naitaja=c()
p=1
for (i in 1:length(aastad)){
  for (j in 1:length(kanalid)) {
    for (k in 1:length(naitajad)) {
      nimek[p]=(paste0(aastad[i], ".",kanalid[j], ".",naitajad[k]))
      aasta[p]=c(aastad[i])
      kanal[p]=c(kanalid[j])
      naitaja[p]=c(naitajad[k])
      p=p+1
    }
  }
}

#nüüd loobime väärtused juurde, saame muutujate nimekirja
jumps=c()

for (i in 1:length(nimek)) {
  jumps[i]=paste0("'",nimek[i],"'", "=jnumber(",  
                  "'teenuste_kanalid_ja_moodikud'", ",", "'",aasta[i], "'",",",
                  "'", kanal[i], "'",",", "'", naitaja[i], "'", ")", ",")
}
#see salvesta csv-sse ning kopi sealt spread_values järgi nimekiri
write.table(jumps, "jumps.csv", sep=";")

#Teeme sama asja linkidega (kuna seal tulemus string, mitte number, siis 
#on skript nati erinev)
nimekLink=c()
aastaLink=c()
kanalLink=c()
p=1
for (i in 1:length(aastad)){
  for (j in 1:length(kanalid)) {
    nimekLink[p]=(paste0(aastad[i], ".",kanalid[j], ".", "link"))
    aastaLink[p]=c(aastad[i])
    kanalLink[p]=c(kanalid[j])
    p=p+1
  }
}
#loobime muutujad valmis
link=c()

for (i in 1:length(nimekLink)) {
  link[i]=paste0("'",nimekLink[i],"'", "=jstring(", 
                 "'teenuste_kanalid_ja_moodikud'", ",", "'",aastaLink[i], 
                 "'",",", "'", kanalLink[i], "'",",", "'", "link", "'", ")", ",")
}
#salvesta csv ja kopi sealt spread_values järgi
write.table(link, "link.csv", sep=";")

#Teenuste üldandmetega sama asi
info=c()

for (i in 1:length(naitajad)) {
  info[i]=paste0("'",naitajad[i],"'", "=jstring(",   "'", naitajad[i], "'", ")", ",")
}
#salvesta csv ja kopi sealt
write.table(info, "info.csv", sep=";")
