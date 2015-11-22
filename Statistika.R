library(dplyr)
#teeme funktsiooni
summary2=function(data, ...) { #... paned jutumärkidesse variabled mille järgi grupeerida
  library(dplyr)
  tulem=data %>%
    group_by_(...) %>%
    summarize(stat_olemas_tk=sum(!is.na(value)),
              max_stat=length(value), #ehk kui palju oleks kanali näitaja hulk
              stat_olemas_pr=sum(!is.na(value))/length(value)) 
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
#kanaliteArv$teenuste_arv=teenusteArv$Freq[2:length(teenusteArv$Freq)]
#keskmine kanalite arv teenuse kohta asutuste lõikes
kanaliteArv$KeskKanaliteArv=kanaliteArv$kanal_arv/kanaliteArv$teenuste_arv

#############tahan infot asutuse ja sihtgrupi lõikes
library(jsonlite)
#data=fromJSON("teenused_all.json")
data=fromJSON("2015-11-22_riigiteenused.json")
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
