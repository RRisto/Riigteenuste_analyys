##############################
#########################################kui palju on tasulisi, tasuta teenuseid kanalite lõikes
#andmed flatiks kõige pealt
library(jsonlite)
#esialgu lae käsitsi alla ning salvesta directorisse
data=fromJSON("teenused_all.json")
dataFlat=flatten(data, recursive = T)
#eemaldan ebavajalikud muutujad NB! erinevad, mis eespool on
vars=names(dataFlat) %in% c("regulatsioon", "objectId", "eluarisyndmus", "kirjeldus", 
                            "eeltingimus", "jareltingimus", "createdAt", "updatedAt", "keel",
                            "ministeerium", "osakondyksus", "omanikunimi", 
                            "omanikuamet", "omanikutelefon", "omanikuemail", "konfinfo", 
                            "seotuddokumendid", "seisund", 
                            "muudatustvajav", "aegumisekpv", "funktsioon", "veebiaadress")
dataFlat=dataFlat[!vars]

#teeme 2 dataseti, üks selle jaoks, milles pole stat (empty), teine 2014 a stati kohta
#tulevikus kui aastaid tuleb juurde, siis tuleb neid veelgi teha
dataFlat2014=dataFlat[, !grepl("teenuste_kanalid_ja_moodikud.2014", names(dataFlat))]
dataFlatEmpty=dataFlat[, !grepl("teenuste_kanalid_ja_moodikud.empty", names(dataFlat))]

#teen kohandatud funktsiooni metlimiseks et saaks ka muid variable kaasata
meltimine2=function(kanal, data) {
        library(reshape2)
        #leiame ainult seda kanalit puudutavad muutujad
        sub=data[, grepl(paste(kanal,"|nimetus|identifikaator|allasutus|sihtgrupp|tegevusvaldkond|teenusetyyp|makse", sep=""), names(data))]
        #määran id-d, mis meltimisel meltimata jäävad
        id=grep(c("nimetus|identifikaator|allasutus|sihtgrupp|tegevusvaldkond|teenusetyyp|makse|link"), names(sub), value=T)
        #kui selle kanali kohta stati pole, anna vastuseks null
        if(length(id)<=7) {
                tulem=NULL
        } else {
                tulem=melt(sub, id=id)
                #muudan variable nime ära, mis on kanalispets, muidu ei saa rbindida
                names(tulem)=c("nimetus", "identifikaator","sihtgrupp","tegevusvaldkond",
                               "teenusetyyp","allasutus","makse", "link", "variable", "value")
        }
        tulem
}

#testime
iseteen=meltimine2(kanal="E-iseteenindus", data=dataFlat2014)

#eelnev funktsiooniks
korrastaja2=function(andmed, eemalda) {
        library(reshape2)
        #eemalda - mis osa columnite nimedest tuleb eemdalda
        names(andmed)=gsub(pattern=eemalda,"" ,names(andmed))
        #kanalite lõikes meldime
        iseteen=meltimine2("E-iseteenindus", data=andmed)
        telefon=meltimine2("Telefon", data=andmed)
        lett=meltimine2("Letiteenus", data=andmed)
        veeb=meltimine2("Veebileht", data=andmed)
        eesti=meltimine2("Eesti.ee", data=andmed)
        epost=meltimine2("E-post", data=andmed)
        kodus=meltimine2("Kliendi juures", data=andmed)
        #rbindime
        koos=rbind(iseteen, telefon, lett, veeb, eesti, epost, kodus)
        #leiame kanali ja näitaja
        #kanal <- strsplit(as.character(koos$variable), split ="\\.\\w{1,}$")
        #stati saamiseks eemaldame .ee eesti.ee-st need põhujstavad muidu probleeme
        stat=gsub(".ee.", ".", as.character(koos$variable), fixed=T)
        #lõikame punktini asja maha
        stat <- strsplit(stat, split ="\\.")
        #teeme df-ks
        df=as.data.frame(stat)
        #transponeerime
        df=as.data.frame(t(df))
        #lisame algsesse andmestikku
        koos$kanal=df[,3]
        koos$naitaja=df[,4]
        #viskame välja tühjad read, kus pole linki
        koos=koos[!is.na(koos$link),]
        koos
}

#teeme funktsioonidega parakas andmed
puhas2014=korrastaja2(dataFlat2014, "teenuste_kanalid_ja_moodikud.2014")
puhasEmpty=korrastaja2(dataFlatEmpty, "teenuste_kanalid_ja_moodikud.empty")      

#paneme kokku
andmed=rbind(puhas2014, puhasEmpty)

#eemaldame variable, seda oli ainult kontrollimiseks vaja
andmed$variable=NULL

###############################KUI PALJU ON TASULISI TEENUSEID ametite lõikes
library(jsonlite)
data=fromJSON("teenused_all.json")
#üldine
table(data$makse)
#asutuste lõikes
makseAsutus=as.data.frame(table(data$makse, data$allasutus))

library(ggplot2)
ggplot(makseAsutus, aes(x=Var2, y=Freq))+
        geom_bar(stat="identity")+
        facet_wrap(~Var1)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(makseAsutus, aes(x=Var1, y=Freq))+
        geom_bar(stat="identity")+
        facet_wrap(~Var2)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

#sama asi protsentides
library(dplyr)
makseAsutuseLoikes=makseAsutus %>%
        group_by(Var2) %>%
        mutate(n=sum(Freq)) %>%
        group_by(Var1, add=TRUE) %>%
        mutate(protsent = Freq/n)
#plotime
ggplot(makseAsutuseLoikes, aes(x=Var2, y=protsent))+
        geom_bar(stat="identity")+
        facet_wrap(~Var1)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(makseAsutuseLoikes, aes(x=Var1, y=protsent))+
        geom_bar(stat="identity")+
        facet_wrap(~Var2)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

#######################KUI PALJU ON TASULISI TEENUSED KANALITE LÕIKES





