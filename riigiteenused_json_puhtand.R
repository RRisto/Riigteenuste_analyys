####eelmine asi läks jube segaseks teen puhtandi
library(jsonlite)
#data=fromJSON("teenused_all.json")
data=fromJSON("./andmed/22.10.2015.json")
dataFlat=flatten(data, recursive = T)
#eemaldan ebavajalikud muutujad
vars=names(dataFlat) %in% c("regulatsioon", "objectId", "eluarisyndmus", "kirjeldus", 
                            "eeltingimus", "jareltingimus", "createdAt", "updatedAt", "keel",
                            "identifikaator", "tegevusvaldkond", "sihtgrupp", "teenusetyyp",
                            "ministeerium", "osakondyksus", "omanikunimi", "omanikuamet", 
                            "omanikutelefon","omanikuemail", "makse", "konfinfo", 
                            "seotuddokumendid", "seisund", 
                            "muudatustvajav", "aegumisekpv", "funktsioon", "veebiaadress")
dataFlat=dataFlat[!vars]

#võtame teenuste_kanalid_ja_moodikud.empty. välja, tööteleme neid eraldi
dataFlat2014=dataFlat[, !grepl("teenuste_kanalid_ja_moodikud.empty", names(dataFlat))]
dataFlatEmpty=dataFlat[, !grepl("teenuste_kanalid_ja_moodikud.2014", names(dataFlat))]
#2014 mõõdikute colmnite nimedest eemaldame "teenuste_kanalid_ja_moodikud.2014."
names(dataFlat2014)=gsub("teenuste_kanalid_ja_moodikud.2014.","" ,names(dataFlat2014))

#proovime laia formaadi pikaks teha
library(tidyr)
library(dplyr)
library(reshape2)
#teen teenuste kaupa

#teen funktsiooniks
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

#testime
iseteen=meltimine("E-iseteenindus", data=dataFlat2014)
telefon=meltimine("Telefon", data=dataFlat2014)
lett=meltimine("Letiteenus", data=dataFlat2014)
veeb=meltimine("Veebileht", data=dataFlat2014)
eesti=meltimine("Eesti.ee", data=dataFlat2014)
epost=meltimine("E-post", data=dataFlat2014)
kodus=meltimine("Kliendi juures", data=dataFlat2014)

#eelnev funktsiooniks
korrastaja=function(andmed, eemalda) {
        library(reshape2)
        #eemalda - mis osa columnite nimedest tuleb eemdalda
        names(andmed)=gsub(pattern=eemalda,"" ,names(andmed))
        #kanalite lõikes meldime
        iseteen=meltimine("E-iseteenindus", data=andmed)
        telefon=meltimine("Telefon", data=andmed)
        lett=meltimine("Letiteenus", data=andmed)
        veeb=meltimine("Veebileht", data=andmed)
        eesti=meltimine("Eesti.ee", data=andmed)
        epost=meltimine("E-post", data=andmed)
        kodus=meltimine("Kliendi juures", data=andmed)
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
        koos$kanal=df[,2]
        koos$naitaja=df[,3]
        #viskame välja tühjad read, kus pole linki
        koos=koos[!is.na(koos$link),]
        koos
}
        
#proov
puhas2014=korrastaja(dataFlat2014, "teenuste_kanalid_ja_moodikud.2014")
puhasEmpty=korrastaja(dataFlatEmpty, "teenuste_kanalid_ja_moodikud.empty")      

############################################################kogu ahel funktsioonidega
###################################################################################
#andmed flatiks
library(jsonlite)
#esialgu lae käsitsi alla ning salvesta directorisse
data=fromJSON("teenused_all.json")
dataFlat=flatten(data, recursive = T)
#eemaldan ebavajalikud muutujad
vars=names(dataFlat) %in% c("regulatsioon", "objectId", "eluarisyndmus", "kirjeldus", 
                            "eeltingimus", "jareltingimus", "createdAt", "updatedAt", "keel",
                            "identifikaator", "tegevusvaldkond", "sihtgrupp", "teenusetyyp",
                            "ministeerium", "osakondyksus", "omanikunimi", "omanikuamet", 
                            "omanikutelefon", "omanikuemail", "makse", "konfinfo", 
                            "seotuddokumendid", "seisund", 
                            "muudatustvajav", "aegumisekpv", "funktsioon", "veebiaadress")
dataFlat=dataFlat[!vars]

#teeme 2 dataseti, üks selle jaoks, milles pole stat (empty), teine 2014 a stati kohta
#tulevikus kui aastaid tuleb juurde, siis tuleb neid veelgi teha
dataFlat2014=dataFlat[, !grepl("teenuste_kanalid_ja_moodikud.empty", names(dataFlat))]
dataFlatEmpty=dataFlat[, !grepl("teenuste_kanalid_ja_moodikud.2014", names(dataFlat))]

#teeme funktsioonidega parakas andmed
puhas2014=korrastaja(dataFlat2014, "teenuste_kanalid_ja_moodikud.2014")
puhasEmpty=korrastaja(dataFlatEmpty, "teenuste_kanalid_ja_moodikud.empty")      

#paneme kokku
andmed=rbind(puhas2014, puhasEmpty)

#eemaldame variable, seda oli ainult kontrollimiseks vaja
andmed$variable=NULL

####################statistika tegemine
library(dplyr)
#teeme funktsiooni
summary2=function(data, ...) { #... paned jutumärkidesse variabled mille järgi grupeerida
        library(dplyr)
        tulem=data %>%
                group_by_(...) %>%
                summarize(stat_olemas_tk=sum(value!=0),
                          max_stat=length(value), #ehk kui palju oleks kanali näitaja hulk
                          stat_olemas_pr=sum(value!=0)/length(value)) 
        tulem
}

#summeerime iga asutuse kõikide kanalite ja näitajate lõikes 
summaryAsutusKanalNait =summary2(data=andmed,  "allasutus", "kanal", "naitaja")

#keskmine tõenäosus, et kanali konkreetse näitaja kohta on stat (%)
nrow(subset(andmed, value!=0))/length(andmed$value)*100

#tõenäosus, et kanali kohta on mingi stat
summaryKanal =summary2(data=andmed,  "kanal")

#tõenäosus, et näitaja kohta on stat olemas
summaryNaitaja =summary2(data=andmed,  "naitaja")

#tõenäosus, et stat on asutuste, näitajate lõikes olemas
summaryAsutusNaitaja =summary2(data=andmed, "allasutus", "naitaja")

#tõenäosus, et stat on olemas asutuste lõikes
summaryAsutus =summary2(data=andmed, "allasutus")

######################visualiseerimine

#visualiseerime asutustes lõikes olemaosleva stati osakaalu
#enne kasvavassejärjekorda
summaryAsutus <- transform(summaryAsutus, 
        jrk = reorder(allasutus, -stat_olemas_pr))
                                        
library(ggplot2)
library(scales)
ggplot(summaryAsutus, aes(x=jrk, y=stat_olemas_pr))+
        geom_bar(stat = "identity", fill="lightblue")+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust=1, size=13))+
        xlab("")+
        ylab("Olemasolevate näidikute osakaal")+
        scale_y_continuous(labels = percent)
        
#kanalite lõikes
summaryKanal <- transform(summaryKanal, 
                           jrk = reorder(kanal, -stat_olemas_pr))

library(ggplot2)
library(scales)
ggplot(summaryKanal, aes(x=jrk, y=stat_olemas_pr))+
        geom_bar(stat = "identity", fill="lightblue")+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust=1, size=13))+
        xlab("")+
        ylab("Olemasolevate näidikute osakaal")+
        scale_y_continuous(labels = percent)

#näitajate lõikes
summaryNaitaja <- transform(summaryNaitaja, 
                          jrk = reorder(naitaja, -stat_olemas_pr))

library(ggplot2)
library(scales)
ggplot(summaryNaitaja, aes(x=jrk, y=stat_olemas_pr))+
        geom_bar(stat = "identity", fill="lightblue")+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust=1, size=13))+
        xlab("")+
        ylab("Olemasolevate näidikute osakaal")+
        scale_y_continuous(labels = percent)

#asutuse ja naitaja loikes
summaryAsutusNaitaja <- transform(summaryAsutusNaitaja, 
                            jrk = reorder(naitaja,-stat_olemas_pr))

library(ggplot2)
library(scales)
ggplot(summaryAsutusNaitaja, aes(x=jrk, y=stat_olemas_pr))+
        geom_bar(stat = "identity", fill="lightblue")+
        facet_wrap(~allasutus)+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust=1, size=13))+
        xlab("")+
        ylab("Olemasolevate näidikute osakaal")+
        scale_y_continuous(labels = percent)

####################üldine stat
#teenuste arv asutuste lõikes
#kõlbab dataFlat
teenusteArv=as.data.frame(table(dataFlat$allasutus))

#kanalite arv asutuste lõikes
library(dplyr)
kanaliteArv=andmed %>%
        group_by(allasutus) %>%
        summarize(kanal_arv=sum(naitaja=="osutamistearv")) 

#paneme kokku
kanaliteArv$teenuste_arv=teenusteArv$Freq
#keskmine kanalite arv teenuse kohta asutuste lõikes
kanaliteArv$KeskKanaliteArv=kanaliteArv$kanal_arv/kanaliteArv$teenuste_arv

#############tahan infot asutuse ja sihtgrupi lõikes
library(jsonlite)
#data=fromJSON("teenused_all.json")
data=fromJSON("./andmed/22.10.2015.json")
sihtgrupp=data[,c("allasutus","sihtgrupp")]

#loome sihtgrupi eraldi colmnitesse
sihtgrupp$kodanik=ifelse(grepl("Kodanik",data$sihtgrupp), "Kodanik", NA)
sihtgrupp$ettevotja=ifelse(grepl("Ettevõtja",data$sihtgrupp), "Ettevõtja", NA)
sihtgrupp$ametnik=ifelse(grepl("Ametnik",data$sihtgrupp), "Ametnik", NA)
#algset colmnit pole vaja
sihtgrupp$sihtgrupp=NULL

#meldime
library(reshape2)
sihtgruppMelt=melt(sihtgrupp, id=c("allasutus"), measure.vars = c("kodanik", "ettevotja", "ametnik"))
#NAd eemaldame
sihtgruppMelt=sihtgruppMelt[complete.cases(sihtgruppMelt[3]),]
#arvutame välja
library(dplyr)
Asutuse_sihtgrupi_loikes=sihtgruppMelt %>%
        group_by(allasutus, value) %>%
        summarize(teenusteArv=n())

Asutuse_sihtgrupi_loikes

#plotime
library(ggplot2)
ggplot(Asutuse_sihtgrupi_loikes, aes(x=allasutus, y=teenusteArv))+
        geom_bar(stat="identity")+
        facet_wrap(~value)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))








