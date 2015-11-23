#########################################kui palju on tasulisi, tasuta teenuseid 
#kanalite lõikes

#teen kohandatud funktsiooni metlimiseks et saaks ka muid muutujaid kaasata
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

#näide
iseteen=meltimine("E.iseteenindus.", data=dataFlat2014)

#korrastaja funktsiooni kohandame ka
korrastaja2=function(andmed, eemalda) {
        library(reshape2)
        #eemalda - mis osa columnite nimedest tuleb eemdalda
        names(andmed)=gsub(pattern=eemalda,"" ,names(andmed))
        #kanalite lõikes meldime
        veeb=meltimine2("Veebileht...portaal.", data=andmed)
        iseteen=meltimine2("E.iseteenindus.", data=andmed)
        eesti=meltimine2("Eesti.ee.", data=andmed)
        nuti=meltimine2("Nutirakendus.", data=andmed)
        digitv=meltimine2("Digitelevisioon.", data=andmed)
        epost=meltimine2("E.post.", data=andmed)
        sms=meltimine2("Tekstisõnum.", data=andmed)
        telefon=meltimine2("Telefon.", data=andmed)
        faks=meltimine2("Faks.", data=andmed)
        post=meltimine2("Post.", data=andmed)
        lett=meltimine2("Letiteenus", data=andmed)
        kodus=meltimine2("Kliendi.juures.", data=andmed)
        #rbindime
        koos=rbind(veeb, iseteen, eesti, nuti, digitv, epost, sms, telefon, faks, 
                   post, lett, kodus)        #leiame kanali ja näitaja
        #stati saamiseks eemaldame .ee eesti.ee-st need põhujstavad muidu probleeme
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

#teeme funktsioonidega parajaks andmed
puhas2014=korrastaja2(dataFlat2014, "X2014.")
puhasEmpty=korrastaja2(dataFlatEmpty, "empty.")      

#paneme kokku
andmed=rbind(puhas2014, puhasEmpty)

#eemaldame variable, seda oli ainult kontrollimiseks vaja
andmed$variable=NULL

###############################KUI PALJU ON TASULISI TEENUSEID ametite lõikes
#üldine
table(andmedLai$makse)
#asutuste lõikes
makseAsutus=as.data.frame(table(andmedLai$makse, andmedLai$allasutus))

library(ggplot2)
ggplot(makseAsutus, aes(x=Var2, y=Freq))+
        geom_bar(stat="identity",  fill="lightblue")+
        facet_wrap(~Var1)+
  theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(makseAsutus, aes(x=Var1, y=Freq))+
        geom_bar(stat="identity", fill="lightblue")+
        facet_wrap(~Var2)+
  theme_minimal()+
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
        geom_bar(stat="identity", fill="lightblue")+
        facet_wrap(~Var1)+
  theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()

ggplot(makseAsutuseLoikes, aes(x=Var1, y=protsent))+
        geom_bar(stat="identity",fill="lightblue")+
        facet_wrap(~Var2)+
  theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

#######################KUI PALJU ON TASULISI TEENUSED KANALITE LÕIKES





