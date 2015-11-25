#######abiskriptid andmete pikka formaati viimiseks

#leiame muutujad, mida ei taha hoida (kui neid pole kunagi vaja, siis täienda 
#fromJsonWide funktsiooni, et neid üldse sisse ei loetaks)
vars=names(andmedLai) %in% c("regulatsioon", "objectId", "eluarisyndmus", "kirjeldus", 
                            "eeltingimus", "jareltingimus", "createdAt", 
                          "updatedAt", "keel", "osakondyksus", "omanikunimi", 
                          "omanikuamet", "omanikutelefon","omanikuemail", 
                           "konfinfo", "seotuddokumendid", "seisund", 
                            "muudatustvajav", "aegumisekpv", "funktsioon", 
                          "veebiaadress")
#eemaldame muutujad
andmedLai=andmedLai[,!vars]

#aastate põhjal teeme andmed 2-ks (kui aastaid rohkem siis vastavalt sellele
#arv muutub)
andmedLai2014=andmedLai[, !grepl("empty.", names(andmedLai))]
andmedLaiEmpty=andmedLai[, !grepl("X2014.", names(andmedLai))]

#abifunktsioon andmete laiast formaadist pikka viimiseks
meltimine=function(kanal, data) {
  library(reshape2)
  #leiame ainult seda kanalit puudutavad muutujad
  sub=data[, grepl(paste(kanal, "|identifikaator|ministeerium|allasutus|tegevusvaldkond|teenusetyyp|makse", sep=""), names(data))]
  #määran id-d, mis meltimisel meltimata jäävad
  id=grep(c("identifikaator|ministeerium|allasutus|tegevusvaldkond|teenusetyyp|makse|link"), names(sub), value=T)
  #kui selle kanali kohta stati pole, anna vastuseks null
  if(length(id)<=2) {
    tulem=NULL
  } else {
    #meldime andmed kitsaks
    tulem=melt(sub, id=id)
    #muudan variable nime ära, mis on kanalispets, muidu ei saa rbindida
    names(tulem)=c("identifikaator", "tegevusvaldkond", "tyyp", "ministeerium", "allasutus",  
                   "makse", "link",  "variable",           
                   "value")
  }
  tulem
}

##abifunktsioon, mis eemaldab kanali nimedest sodi ning teeb kõikide kanalite 
#andmed laiast pikaks
korrastaja=function(andmed, eemalda) {
  library(reshape2)
  #eemalda - mis osa columnite nimedest tuleb eemdalda
  names(andmed)=gsub(pattern=eemalda,"" ,names(andmed))
  #kanalite lõikes meldime
  veeb=meltimine("Veebileht...portaal.", data=andmed)
  iseteen=meltimine("E.iseteenindus.", data=andmed)
  eesti=meltimine("Eesti.ee.", data=andmed)
  nuti=meltimine("Nutirakendus.", data=andmed)
  digitv=meltimine("Digitelevisioon.", data=andmed)
  epost=meltimine("E.post.", data=andmed)
  sms=meltimine("Tekstisõnum.", data=andmed)
  telefon=meltimine("Telefon.", data=andmed)
  faks=meltimine("Faks.", data=andmed)
  post=meltimine("Post.", data=andmed)
  lett=meltimine("Letiteenus.", data=andmed) #võib muutuda! vaja ka gsubi siis lisada
  kodus=meltimine("Kliendi.juures.", data=andmed)
  #rbindime
  koos=rbind(veeb, iseteen, eesti, nuti, digitv, epost, sms, telefon, faks, 
             post, lett, kodus)
  #leiame kanali ja näitaja
  #kanal <- strsplit(as.character(koos$variable), split ="\\.\\w{1,}$")
  #stati saamiseks eemaldame punktid kanali nimedest
  koos$variable=gsub(".ee.", ".", as.character(koos$variable), fixed=T)
  #skoos$variable=gsub("Letiteenus.büroos", "Letiteenus", as.character(koos$variable), fixed=T)
  koos$variable=gsub("E.iseteenindus", "Eiseteenindus", as.character(koos$variable), fixed=T)
  koos$variable=gsub("E.post", "Epost", as.character(koos$variable), fixed=T)
  koos$variable=gsub("Veebileht...portaal", "Veebileht", as.character(koos$variable), fixed=T)
  stat=gsub("Kliendi.juures", "Kliendijuures", as.character(koos$variable), fixed=T)

  #lõikame punktini asja maha
  stat <- strsplit(stat, split ="\\.")
  #teeme df-ks
  df=as.data.frame(stat)
  #transponeerime
  df=as.data.frame(t(df))
  #lisame algsesse andmestikku
  koos$kanal=df[,1]
  koos$naitaja=df[,2]
  #viskame välja tühjad read, kus pole linki
  koos=koos[!is.na(koos$link),]
  koos
}

#############näide
puhas2014=korrastaja(andmedLai2014, "X2014.")
puhasEmpty=korrastaja(andmedLaiEmpty, "empty.")

#paneme andme kokku
andmedPikk=rbind(puhas2014, puhasEmpty)

#salvesta
failinimi=paste0(Sys.Date(), "_andmedPikk.csv")
write.table(andmedPikk, failinimi, sep=";", row.names = F)

failinimitxt=paste0(Sys.Date(), "_andmedPikk.txt")
write.table(andmedPikk, failinimitxt, sep=";", row.names = F)
