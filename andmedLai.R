#kui tahad ainult faili sisse lugeda ja salevstada, kasuta seda funktsiooni
#NB! see funktsioon teeb küll data frame kuid statisitka ja lingi osa paneb
#ühte veergu listina. Selle välistamiseks kasutan ise järgmist skripti
andmedSisse=function(url) {
  failinimi=paste0(Sys.Date(), "_riigiteenused.json")
  download.file(url=url, failinimi, method="auto", 
                quiet = FALSE, mode = "w",cacheOK = TRUE,
                extra = getOption("download.file.extra"))
  
  #loeme andmed sisse
  library(jsonlite)
  library(data.table)
  andmed=fromJSON(readLines("https://www.riigiteenused.ee/api/et/all"), flatten=T)
  andmed=andmed["teenuste_kanalid_ja_moodikud"!="list()"]
  andmedMoodik <- rbindlist(lapply(andmed[["teenuste_kanalid_ja_moodikud"]], function(x) {
    as.list(unlist(x))
  }), fill=TRUE)
  
  andmed
}
#näide
andmed=andmedSisse("https://www.riigiteenused.ee/api/et/all")

###uuendatud versioon, ei downloadi arvutisse faili, kasutab data tabelit
andmedSisse=function(url, poleVajaVar) {
  #poleVajaVar=read, mis eemdaldatakse
  #loeme andmed sisse
  library(jsonlite)
  library(data.table)
  andmed=data.table(fromJSON(readLines(url), flatten=T))
  andmed=andmed[teenuste_kanalid_ja_moodikud!="list()"]
  moodik <- rbindlist(lapply(andmed[["teenuste_kanalid_ja_moodikud"]], function(x) {
    as.list(unlist(x))
  }), fill=TRUE)
  andmed[, c(poleVajaVar):=NULL] #kindlasti peab olema eemaldatud: 
  #"teenuste_kanalid_ja_moodikud", "sihtgrupp", "regulatsioon"
  andmed=cbind(andmed, moodik)
  andmed
}

#näide
andmed=andmedSisse("https://www.riigiteenused.ee/api/et/all",
                   c("teenuste_kanalid_ja_moodikud", "sihtgrupp", "regulatsioon",
                     "createdAt", "objectId", "updatedAt", "keel", "eluarisyndmus",
                     "kirjeldus", "osakondyksus", "omanikunimi", "omanikuamet",
                     "omanikutelefon", "omanikuemail", "konfinfo", "eeltingimus",
                     "jareltingimus", "seotuddokumendid", "muudatustvajav", 
                     "aegumisekpv","funktsioon", "veebiaadress", "seisund", 
                     "makse", "teenusetyyp", "tegevusvaldkond"))
########################

#funktsioon jsoni failist andmete sisselugemist laiaks formaadiks. Statistika ja
#lingid paneb ilusti eraldi veergudesse.
#kuidas loetelu tekitada spread_values järgi, vaata abiskript.R
#hetkel ei loe eel- ja järeltingimust (välja kommenteeritud), kuna need lahtrid
#võivad liiga palju teksti lisada ning edaspidi töötlemisel probleeme tekitada
#üldse võib jätta ainult andmed, mida vaja
fromJsonLong=function(andmedJson) {
  library(tidyjson)
  library(dplyr)
  andmedWide=andmedJson %>% 
    as.tbl_json %>% 
    gather_array %>% 
    spread_values(
      #üldandmed
      'createdAt'=jstring('createdAt'),
      'objectId'=jstring('objectId'),
      'updatedAt'=jstring('updatedAt'),
      'keel'=jstring('keel'),
      'nimetus'=jstring('nimetus'),
      'eluarisyndmus'=jstring('eluarisyndmus'),
      'identifikaator'=jstring('identifikaator'),
      'kirjeldus'=jstring('kirjeldus'),
      'tegevusvaldkond'=jstring('tegevusvaldkond'),
      'teenusetyyp'=jstring('teenusetyyp'),
      'ministeerium'=jstring('ministeerium'),
      'allasutus'=jstring('allasutus'),
      'osakondyksus'=jstring('osakondyksus'),
      'omanikunimi'=jstring('omanikunimi'),
      'omanikuamet'=jstring('omanikuamet'),
      'omanikutelefon'=jstring('omanikutelefon'),
      'omanikuemail'=jstring('omanikuemail'),
      'konfinfo'=jstring('konfinfo'),
      #'eeltingimus'=jstring('eeltingimus'),
      #'jareltingimus'=jstring('jareltingimus'), #kui liiga pikk tekitab jama
      'seotuddokumendid'=jstring('seotuddokumendid'),
      'muudatustvajav'=jstring('muudatustvajav'),
      'aegumisekpv'=jstring('aegumisekpv'),
      'funktsioon'=jstring('funktsioon'),
      'veebiaadress'=jstring('veebiaadress'),
      'makse'=jstring('makse'),
      'seisund'=jstring('seisund'),
      'sihtgrupp'=jstring('sihtgrupp'),
      'regulatsioon'=jstring('regulatsioon'),
      #statistika
      'empty.Veebileht / portaal.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','empty','Veebileht / portaal','osutamistearv'),
      'empty.Veebileht / portaal.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Veebileht / portaal','rahulolu'),
      'empty.Veebileht / portaal.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Veebileht / portaal','halduskulu'),
      'empty.Veebileht / portaal.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Veebileht / portaal','ajakulu'),
      'empty.E-iseteenindus.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','empty','E-iseteenindus','osutamistearv'),
      'empty.E-iseteenindus.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','empty','E-iseteenindus','rahulolu'),
      'empty.E-iseteenindus.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','E-iseteenindus','halduskulu'),
      'empty.E-iseteenindus.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','E-iseteenindus','ajakulu'),
      'empty.Eesti.ee.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','empty','Eesti.ee','osutamistearv'),
      'empty.Eesti.ee.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Eesti.ee','rahulolu'),
      'empty.Eesti.ee.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Eesti.ee','halduskulu'),
      'empty.Eesti.ee.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Eesti.ee','ajakulu'),
      'empty.Nutirakendus.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','empty','Nutirakendus','osutamistearv'),
      'empty.Nutirakendus.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Nutirakendus','rahulolu'),
      'empty.Nutirakendus.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Nutirakendus','halduskulu'),
      'empty.Nutirakendus.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Nutirakendus','ajakulu'),
      'empty.Digitelevisioon.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','empty','Digitelevisioon','osutamistearv'),
      'empty.Digitelevisioon.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Digitelevisioon','rahulolu'),
      'empty.Digitelevisioon.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Digitelevisioon','halduskulu'),
      'empty.Digitelevisioon.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Digitelevisioon','ajakulu'),
      'empty.E-post.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','empty','E-post','osutamistearv'),
      'empty.E-post.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','empty','E-post','rahulolu'),
      'empty.E-post.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','E-post','halduskulu'),
      'empty.E-post.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','E-post','ajakulu'),
      'empty.Tekstisõnum.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','empty','Tekstisõnum','osutamistearv'),
      'empty.Tekstisõnum.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Tekstisõnum','rahulolu'),
      'empty.Tekstisõnum.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Tekstisõnum','halduskulu'),
      'empty.Tekstisõnum.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Tekstisõnum','ajakulu'),
      'empty.Telefon.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','empty','Telefon','osutamistearv'),
      'empty.Telefon.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Telefon','rahulolu'),
      'empty.Telefon.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Telefon','halduskulu'),
      'empty.Telefon.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Telefon','ajakulu'),
      'empty.Faks.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','empty','Faks','osutamistearv'),
      'empty.Faks.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Faks','rahulolu'),
      'empty.Faks.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Faks','halduskulu'),
      'empty.Faks.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Faks','ajakulu'),
      'empty.Post.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','empty','Post','osutamistearv'),
      'empty.Post.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Post','rahulolu'),
      'empty.Post.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Post','halduskulu'),
      'empty.Post.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Post','ajakulu'),
      'empty.Letiteenus.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','empty','Letiteenus büroos','osutamistearv'),
      'empty.Letiteenus.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Letiteenus büroos','rahulolu'),
      'empty.Letiteenus.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Letiteenus büroos','halduskulu'),
      'empty.Letiteenus.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Letiteenus büroos','ajakulu'),
      'empty.Kliendi juures.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','empty','Kliendi juures','osutamistearv'),
      'empty.Kliendi juures.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Kliendi juures','rahulolu'),
      'empty.Kliendi juures.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Kliendi juures','halduskulu'),
      'empty.Kliendi juures.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','empty','Kliendi juures','ajakulu'),
      '2014.Veebileht / portaal.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','2014','Veebileht / portaal','osutamistearv'),
      '2014.Veebileht / portaal.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Veebileht / portaal','rahulolu'),
      '2014.Veebileht / portaal.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Veebileht / portaal','halduskulu'),
      '2014.Veebileht / portaal.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Veebileht / portaal','ajakulu'),
      '2014.E-iseteenindus.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','2014','E-iseteenindus','osutamistearv'),
      '2014.E-iseteenindus.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','2014','E-iseteenindus','rahulolu'),
      '2014.E-iseteenindus.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','E-iseteenindus','halduskulu'),
      '2014.E-iseteenindus.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','E-iseteenindus','ajakulu'),
      '2014.Eesti.ee.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','2014','Eesti.ee','osutamistearv'),
      '2014.Eesti.ee.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Eesti.ee','rahulolu'),
      '2014.Eesti.ee.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Eesti.ee','halduskulu'),
      '2014.Eesti.ee.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Eesti.ee','ajakulu'),
      '2014.Nutirakendus.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','2014','Nutirakendus','osutamistearv'),
      '2014.Nutirakendus.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Nutirakendus','rahulolu'),
      '2014.Nutirakendus.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Nutirakendus','halduskulu'),
      '2014.Nutirakendus.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Nutirakendus','ajakulu'),
      '2014.Digitelevisioon.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','2014','Digitelevisioon','osutamistearv'),
      '2014.Digitelevisioon.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Digitelevisioon','rahulolu'),
      '2014.Digitelevisioon.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Digitelevisioon','halduskulu'),
      '2014.Digitelevisioon.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Digitelevisioon','ajakulu'),
      '2014.E-post.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','2014','E-post','osutamistearv'),
      '2014.E-post.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','2014','E-post','rahulolu'),
      '2014.E-post.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','E-post','halduskulu'),
      '2014.E-post.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','E-post','ajakulu'),
      '2014.Tekstisõnum.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','2014','Tekstisõnum','osutamistearv'),
      '2014.Tekstisõnum.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Tekstisõnum','rahulolu'),
      '2014.Tekstisõnum.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Tekstisõnum','halduskulu'),
      '2014.Tekstisõnum.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Tekstisõnum','ajakulu'),
      '2014.Telefon.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','2014','Telefon','osutamistearv'),
      '2014.Telefon.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Telefon','rahulolu'),
      '2014.Telefon.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Telefon','halduskulu'),
      '2014.Telefon.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Telefon','ajakulu'),
      '2014.Faks.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','2014','Faks','osutamistearv'),
      '2014.Faks.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Faks','rahulolu'),
      '2014.Faks.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Faks','halduskulu'),
      '2014.Faks.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Faks','ajakulu'),
      '2014.Post.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','2014','Post','osutamistearv'),
      '2014.Post.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Post','rahulolu'),
      '2014.Post.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Post','halduskulu'),
      '2014.Post.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Post','ajakulu'),
      '2014.Letiteenus.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','2014','Letiteenus büroos','osutamistearv'),
      '2014.Letiteenus.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Letiteenus büroos','rahulolu'),
      '2014.Letiteenus.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Letiteenus büroos','halduskulu'),
      '2014.Letiteenus.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Letiteenus büroos','ajakulu'),
      '2014.Kliendi juures.osutamistearv'=jnumber('teenuste_kanalid_ja_moodikud','2014','Kliendi juures','osutamistearv'),
      '2014.Kliendi juures.rahulolu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Kliendi juures','rahulolu'),
      '2014.Kliendi juures.halduskulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Kliendi juures','halduskulu'),
      '2014.Kliendi juures.ajakulu'=jnumber('teenuste_kanalid_ja_moodikud','2014','Kliendi juures','ajakulu'),
      
      #linkide tabelist
      'empty.Veebileht / portaal.link'=jstring('teenuste_kanalid_ja_moodikud','empty','Veebileht / portaal','link'),
      'empty.E-iseteenindus.link'=jstring('teenuste_kanalid_ja_moodikud','empty','E-iseteenindus','link'),
      'empty.Eesti.ee.link'=jstring('teenuste_kanalid_ja_moodikud','empty','Eesti.ee','link'),
      'empty.Nutirakendus.link'=jstring('teenuste_kanalid_ja_moodikud','empty','Nutirakendus','link'),
      'empty.Digitelevisioon.link'=jstring('teenuste_kanalid_ja_moodikud','empty','Digitelevisioon','link'),
      'empty.E-post.link'=jstring('teenuste_kanalid_ja_moodikud','empty','E-post','link'),
      'empty.Tekstisõnum.link'=jstring('teenuste_kanalid_ja_moodikud','empty','Tekstisõnum','link'),
      'empty.Telefon.link'=jstring('teenuste_kanalid_ja_moodikud','empty','Telefon','link'),
      'empty.Faks.link'=jstring('teenuste_kanalid_ja_moodikud','empty','Faks','link'),
      'empty.Post.link'=jstring('teenuste_kanalid_ja_moodikud','empty','Post','link'),
      'empty.Letiteenus.link'=jstring('teenuste_kanalid_ja_moodikud','empty','Letiteenus büroos','link'),
      'empty.Kliendi juures.link'=jstring('teenuste_kanalid_ja_moodikud','empty','Kliendi juures','link'),
      '2014.Veebileht / portaal.link'=jstring('teenuste_kanalid_ja_moodikud','2014','Veebileht / portaal','link'),
      '2014.E-iseteenindus.link'=jstring('teenuste_kanalid_ja_moodikud','2014','E-iseteenindus','link'),
      '2014.Eesti.ee.link'=jstring('teenuste_kanalid_ja_moodikud','2014','Eesti.ee','link'),
      '2014.Nutirakendus.link'=jstring('teenuste_kanalid_ja_moodikud','2014','Nutirakendus','link'),
      '2014.Digitelevisioon.link'=jstring('teenuste_kanalid_ja_moodikud','2014','Digitelevisioon','link'),
      '2014.E-post.link'=jstring('teenuste_kanalid_ja_moodikud','2014','E-post','link'),
      '2014.Tekstisõnum.link'=jstring('teenuste_kanalid_ja_moodikud','2014','Tekstisõnum','link'),
      '2014.Telefon.link'=jstring('teenuste_kanalid_ja_moodikud','2014','Telefon','link'),
      '2014.Faks.link'=jstring('teenuste_kanalid_ja_moodikud','2014','Faks','link'),
      '2014.Post.link'=jstring('teenuste_kanalid_ja_moodikud','2014','Post','link'),
      '2014.Letiteenus.link'=jstring('teenuste_kanalid_ja_moodikud','2014','Letiteenus büroos','link'),
      '2014.Kliendi juures.link'=jstring('teenuste_kanalid_ja_moodikud','2014','Kliendi juures','link')
    )
  #eemaldame sodi
  andmedWide$document.id=NULL
  andmedWide$array.index=NULL
  andmedWide
}

#näide
andmedLai=fromJsonLong(andmedJson=readLines("https://www.riigiteenused.ee/api/et/all"))
#kui tahad salvestada:
failinimi=paste0(Sys.Date(), "_andmedLai.csv")
write.table(andmedLai, failinimi, sep=";", row.names = F)
