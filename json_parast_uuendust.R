###katsetus, mis ei andnud soovitud tulemust!!!

library(jsonlite)
andmed=fromJSON("2015-11-01_riigiteenused.json", flatten=F)
kanalid=andmed$teenuste_kanalid_ja_moodikud
kanalid2=unlist(kanalid)

kanaliddf=as.data.frame(matrix(kanalid2))

andmed$teenuste_kanalid_ja_moodikud$empty
andmed$teenuste_kanalid_ja_moodikud$empty$`Veebileht / portaal`$osutamistearv

###sellega saan mõõdikud ja kanalid ühte veergu, vaja veel regexiga lahti arutada
library("tidyjson")
library(dplyr)

jsonfile=readLines("2015-11-01_riigiteenused.json")

proov <- jsonfile %>%
  gather_array %>%                                     # stack the users 
  spread_values(id = jstring("identifikaator")) %>%          # extract the user name
  spread_values(kanal = jstring("teenuste_kanalid_ja_moodikud")) 


##############Esimene töötav variant originaaliga
library("tidyjson")
library(dplyr)

jsonfile=readLines("2015-11-01_riigiteenused.json")

kanalid=c("Veebileht","E-iseteenindus", "Eesti.ee","Nutirakendus","Digitelevisioon", "E-post",
          "Tekstisõnum","Telefon", "Faks","Post", "Letiteenus","Kliendi juures")

stat=jsonfile %>% 
  as.tbl_json %>% 
  gather_array %>% 
  spread_values("link" = jstring("teenuste_kanalid_ja_moodikud","empty","E-iseteenindus", 
                                 "link"), "id" = jstring("identifikaator"), 
                "rahulolu"=jnumber("teenuste_kanalid_ja_moodikud","empty","E-iseteenindus", 
                                   "rahulolu"))

#########hakkab nagu looma 
  #ettevalimistus
  aastad=c("empty", "2014")
  naitajad=c("osutamistearv", "rahulolu", "halduskulu", "ajakulu") 
  
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
#tühi df
statistika=data.frame(matrix(ncol = 96, nrow = 237))

for (i in 1:length(nimek)) {
  statistika[[nimek[i]]]=jsonfile %>% 
  as.tbl_json %>% 
    gather_array %>% 
      spread_values(
        muutuja=jnumber("teenuste_kanalid_ja_moodikud", aasta[i], kanal[i], naitaja[i])
      )
}


#link juurde
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
#loobime lingid juurde
for (i in 1:length(nimekLink)) {
  statistika[[nimekLink[i]]]=jsonfile %>% 
    as.tbl_json %>% 
    gather_array %>% 
    spread_values(
      link=jstring("teenuste_kanalid_ja_moodikud", aastaLink[i], kanalLink[i], "link")
    )
}

#eemaldame kõid document.id ja array.index columnid
for (i in 1:ncol(statistika)) {
  statistika[[i]]$document.id=NULL
  statistika[[i]]$array.index=NULL
}

######funktsiooniks
riigteenusteStat=function(jsonfile, aastad, kanalid, naitajad) {
  #jsonfile=readlines jsoni file, aastad=vektor aastatega mis on jsonis olemas,
  #kanalid, mis jsonis olemas, naitajad, mis on jsonis
  #tagastab dataframe, iga column 1 näitaja (või link)
  #teeme aasta, kanalite ja näitajatete vektorid, kus lopimise jaoks on 
  #iga i jaoks juba nende näitajate jaotus õige. 
  library(tidyjson)
  library(dplyr)
  nimek=c() #koond järgnevatest, selle järgi tehakse lõplik col name
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
  #tühi df, kuhu stati hakkame loopima
  statistika=data.frame(matrix(ncol = 0, nrow = 237)) #see tuleb autoaatseks teha
  #loobime stat sisse
  for (i in 1:length(nimek)) {
    statistika[[nimek[i]]]=jsonfile %>% 
      as.tbl_json %>% 
      gather_array %>% 
      spread_values(
        muutuja=jnumber("teenuste_kanalid_ja_moodikud", aasta[i], kanal[i], naitaja[i])
      )
  }
  #lingi juurde loopimiseks vajalikud vektorid
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
  #loobime lingid juurde
  for (i in 1:length(nimekLink)) {
    statistika[[nimekLink[i]]]=jsonfile %>% 
      as.tbl_json %>% 
      gather_array %>% 
      spread_values(
        link=jstring("teenuste_kanalid_ja_moodikud", aastaLink[i], kanalLink[i], "link")
      )
  }
  #eemaldame kõid document.id ja array.index columnid
  for (i in 1:ncol(statistika)) {
    statistika[[i]]$document.id=NULL
    statistika[[i]]$array.index=NULL
  }
  statistika
}

##test
aastad=c("empty", "2014")
naitajad=c("osutamistearv", "rahulolu", "halduskulu", "ajakulu") 
kanalid=c("Veebileht","E-iseteenindus", "Eesti.ee","Nutirakendus","Digitelevisioon", "E-post",
          "Tekstisõnum","Telefon", "Faks","Post", "Letiteenus","Kliendi juures")

 koikAndmed=fromJSON("2015-11-01_riigiteenused.json", flatten=T)
###########funktsioon statistika ja ülejäänud andmete ühendamiseks
#eemaldame vanadast kanalite ja mõõdikute info
#koikAndmed$aasta=ifelse(grepl(koikAndmed$teenuste_kanalid_ja_moodikud, "empty"), "empty", "2014")
koikAndmed$teenuste_kanalid_ja_moodikud=NULL

andmedKoos=cbind(koikAndmed, statistika)

##proovin vanade funktsioonide jaosk söödavaks teha
dataFlat2014=andmedKoos[, !grepl("empty", names(andmedKoos))]


proov=melt(dataFlat2014, id=c())

nimed=c("createdAt", "objectId", "updatedAt", "keel", "nimetus" , "eluarisyndmus" ,
"identifikaator", "kirjeldus","tegevusvaldkond","teenusetyyp", "ministeerium",
"allasutus","osakondyksus", "omanikunimi", "omanikuamet", "omanikutelefon",
"omanikuemail", "makse", "konfinfo", "eeltingimus", "jareltingimus", 
"seotuddokumendid", "seisund","muudatustvajav","aegumisekpv","funktsioon",
"veebiaadress","sihtgrupp", "regulatsioon")

vars=names(andmedKoos) %in% c("regulatsioon", "objectId", "eluarisyndmus", "kirjeldus", 
                            "eeltingimus", "jareltingimus", "createdAt", "updatedAt", "keel",
                            "identifikaator", "tegevusvaldkond", "sihtgrupp", "teenusetyyp",
                            "ministeerium", "osakondyksus", "omanikunimi", "omanikuamet", 
                            "omanikutelefon","omanikuemail", "makse", "konfinfo", 
                            "seotuddokumendid", "seisund", 
                            "muudatustvajav", "aegumisekpv", "funktsioon", "veebiaadress")
dataFlat=andmedKoos[!vars]

dataFlat2014=dataFlat[, !grepl("empty.", names(dataFlat))]
names(dataFlat2014)=gsub("2014.","" ,names(dataFlat2014))
names(dataFlat2014)=gsub("muutuja","" ,names(dataFlat2014))

dataFlat2014proov=dataFlat2014[, colSums(is.na(dataFlat2014)) != nrow(dataFlat2014)]
#EI tööta!!
iseteen=meltimine("E-iseteenindus", data=dataFlat2014)
#täisporno:
write.table(dataFlat2014,file="proov.txt")
