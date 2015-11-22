vars=names(andmed) %in% c("regulatsioon", "objectId", "eluarisyndmus", "kirjeldus", 
                            "eeltingimus", "jareltingimus", "createdAt", "updatedAt", "keel",
                            "identifikaator", "tegevusvaldkond", "sihtgrupp", "teenusetyyp",
                            "ministeerium", "osakondyksus", "omanikunimi", "omanikuamet", 
                            "omanikutelefon","omanikuemail", "makse", "konfinfo", 
                            "seotuddokumendid", "seisund", 
                            "muudatustvajav", "aegumisekpv", "funktsioon", "veebiaadress")
dataFlat=andmed[,!vars]

dataFlat2014=dataFlat[, !grepl("empty.", names(dataFlat))]
#puhasEmpty=korrastaja(dataFlatEmpty, "teenuste_kanalid_ja_moodikud.empty")
dataFlatEmpty=dataFlat[, !grepl("X2014.", names(dataFlat))]

meltimine=function(kanal, data) {
  library(reshape2)
  #leiame ainult seda kanalit puudutavad muutujad
  sub=data[, grepl(paste(kanal, "|nimetus|allasutus", sep=""), names(data))]
  #määran id-d, mis meltimisel meltimata jäävad
  id=grep(c("nimetus|allasutus|link"), names(sub), value=T)
  #kui selle kanali kohta stati pole, anna vastuseks null
  if(length(id)<=2) {
    tulem=NULL
  } else {
    tulem=melt(sub, id=id)
    #muudan variable nime ära, mis on kanalispets, muidu ei saa rbindida
    names(tulem)=c("nimetus", "allasutus", "link", "variable",           
                   "value")
  }
  
  tulem
}

##
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
  lett=meltimine("Letiteenus", data=andmed)
  kodus=meltimine("Kliendi.juures.", data=andmed)
  #rbindime
  koos=rbind(veeb, iseteen, eesti, nuti, digitv, epost, sms, telefon, faks, 
             post, lett, kodus)
  #leiame kanali ja näitaja
  #kanal <- strsplit(as.character(koos$variable), split ="\\.\\w{1,}$")
  #stati saamiseks eemaldame . kanali nimest
  koos$variable=gsub(".ee.", ".", as.character(koos$variable), fixed=T)
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

puhas2014=korrastaja(dataFlat2014, "X2014.")
puhasEmpty=korrastaja(dataFlatEmpty, "empty.")


#kokku
AndmedShort=rbind(puhas2014, puhasEmpty)

