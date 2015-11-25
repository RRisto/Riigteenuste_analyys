#####abifunktsioonid, näidised statistika tegemiseks

#abifunktsioon andmete summeerimiseks
summeerija=function(data, ...) { #... paned jutumärkidesse variabled mille järgi grupeerida
  library(dplyr)
  tulem=data %>%
    group_by_(...) %>%
    summarize(stat_olemas_tk=sum(!is.na(value)),
              max_stat=length(value), #ehk kui palju oleks kanali näitaja hulk
              stat_olemas_pr=sum(!is.na(value))/length(value)) 
  tulem
}
#abifunktsioon andmete visualiseerimiseks
visualiseerija=function(data, mapping, ylab) {
  #localenv <- environment()
  library(ggplot2)
  library(scales)
  ggplot(data, mapping)+
    geom_bar(stat = "identity", fill="lightblue")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust=1, size=13))+
    xlab("")+
    ylab(ylab)+
    coord_cartesian(ylim=c(0,1))+
    scale_y_discrete(labels = percent)
}

#ilma %deta
visualiseerija2=function(data, mapping, ylab) {
  #localenv <- environment()
  library(ggplot2)
  #library(scales)
  ggplot(data, mapping)+
    geom_bar(stat = "identity", fill="lightblue")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust=1, size=13))+
    xlab("")+
    ylab(ylab)#+
    #coord_cartesian(ylim=c(0,1))#+
    #scale_y_discrete(labels = percent)
}

##abifunktsioon sihtgrupi andmete visualiseerimiseks
sihtgruppStat=function(andmed) {
  sihtgrupp=andmed[,c("allasutus","sihtgrupp")]
  #loome sihtgrupi eraldi colmnitesse
  sihtgrupp$kodanik=ifelse(grepl("Kodanik",andmed$sihtgrupp), "Kodanik", NA)
  sihtgrupp$ettevotja=ifelse(grepl("Ettevõtja",andmed$sihtgrupp), "Ettevõtja", NA)
  sihtgrupp$ametnik=ifelse(grepl("Ametnik",andmed$sihtgrupp), "Ametnik", NA)
  #algset colmnit pole vaja
  sihtgrupp$sihtgrupp=NULL
  #meldime
  library(reshape2)
  sihtgruppMelt=melt(sihtgrupp, id=c("allasutus"), measure.vars = c("kodanik",
                                                                    "ettevotja", 
                                                                    "ametnik"))
  #NAd eemaldame
  sihtgruppMelt=sihtgruppMelt[complete.cases(sihtgruppMelt[3]),]
  #arvutame välja
  library(dplyr)
  Asutuse_sihtgrupi_loikes=sihtgruppMelt %>%
    group_by(allasutus, value) %>%
    summarize(teenusteArv=n())
  #plotime
  library(ggplot2)
  ggplot(Asutuse_sihtgrupi_loikes, aes(x=allasutus, y=teenusteArv))+
    geom_bar(stat="identity")+
    facet_wrap(~value)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

##########näidised####################################

#summeerime iga asutuse kõikide kanalite ja näitajate lõikes 
summaryAsutusKanalNait =summeerija(data=andmedPikk,  "allasutus", "kanal", "naitaja")

#keskmine tõenäosus, et kanali konkreetse näitaja kohta on stat (%)
nrow(subset(andmedPikk, value!=0))/length(andmedPikk$value)*100

#tõenäosus, et kanali kohta on mingi stat
summaryKanal =summeerija(data=andmedPikk,  "kanal")

#tõenäosus, et näitaja kohta on stat olemas
summaryNaitaja =summeerija(data=andmedPikk,  "naitaja")

#tõenäosus, et stat on asutuste, näitajate lõikes olemas
summaryAsutusNaitaja =summeerija(data=andmedPikk, "allasutus", "naitaja")

#tõenäosus, et stat on olemas asutuste lõikes
summaryAsutus =summeerija(data=andmedPikk, "allasutus")

#tõenäosus, et stat on olemas ministeeriumite lõikes
summaryMin =summeerija(data=andmedPikk, "ministeerium")

#tõenäosus, et stat on ministeeriumite, näitajate lõikes olemas
summaryMinNaitaja =summeerija(data=andmedPikk, "ministeerium", "naitaja")

#summeerime iga ministeeriumi kõikide kanalite ja näitajate lõikes 
summaryMinKanalNait =summeerija(data=andmedPikk,  "ministeerium", "kanal", "naitaja")

######################visualiseerimine

#visualiseerime asutustes lõikes olemaosleva stati osakaalu
#enne kasvavassejärjekorda
summaryAsutus <- transform(summaryAsutus, 
                           jrk = reorder(allasutus, -stat_olemas_pr))

visualiseerija(data=summaryAsutus, aes(x=jrk, y=stat_olemas_pr), 
               "Olemasolevate näidikute osakaal")

#kanalite lõikes
summaryKanal <- transform(summaryKanal, 
                          jrk = reorder(kanal, -stat_olemas_pr))

visualiseerija(data=summaryKanal, aes(x=jrk, y=stat_olemas_pr), 
               "Olemasolevate näidikute osakaal")

#näitajate lõikes
summaryNaitaja <- transform(summaryNaitaja, 
                            jrk = reorder(naitaja, -stat_olemas_pr))

visualiseerija(data=summaryNaitaja, aes(x=jrk, y=stat_olemas_pr), 
               "Olemasolevate näidikute osakaal")

#asutuse ja naitaja loikes
summaryAsutusNaitaja <- transform(summaryAsutusNaitaja, 
                                  jrk = reorder(naitaja,-stat_olemas_pr))

visualiseerija(data=summaryAsutusNaitaja, aes(x=jrk, y=stat_olemas_pr), 
               "Olemasolevate näidikute osakaal")+
  facet_wrap(~allasutus)

#minsteeriumi ja näitaja lõikes
summaryMinNaitaja <- transform(summaryMinNaitaja, 
                                  jrk = reorder(naitaja,-stat_olemas_pr))

visualiseerija(data=summaryMinNaitaja, aes(x=jrk, y=stat_olemas_pr), 
               "Olemasolevate näidikute osakaal")+
  facet_wrap(~ministeerium)

####################üldine stat
#teenuste arv asutuste lõikes
teenusteArv=as.data.frame(table(andmedLai$allasutus))

#kanalite arv asutuste lõikes
library(dplyr)
kanaliteArv=andmedPikk %>%
  group_by(allasutus) %>%
  summarize(kanal_arv=sum(naitaja=="osutamistearv")) 

#paneme kokku
kanaliteArv$teenuste_arv=teenusteArv$Freq
#kanaliteArv$teenuste_arv=teenusteArv$Freq[2:length(teenusteArv$Freq)]
#keskmine kanalite arv teenuse kohta asutuste lõikes
kanaliteArv$KeskKanaliteArv=kanaliteArv$kanal_arv/kanaliteArv$teenuste_arv

##teenuste arv kanalites
summaryTeenustKanalis =summeerija(data=andmedPikk,  "naitaja")

summeerija2=function(data, ...) { #... paned jutumärkidesse variabled mille järgi grupeerida
  library(dplyr)
  tulem=data %>%
    group_by_(...) %>%
    summarize(arv=n()) 
  #jrk õigeks
  tulem <- transform(tulem,jrk = reorder(kanal,-arv))
  tulem
}

summaryTeenustKanalis=summeerija2(andmedPikk, c("kanal"))

visualiseerija2(data=summaryTeenustKanalis, aes(x=kanal, y=arv), 
               "Teenuste arv kanalis")


summeerija2=function(data, ...) { #... paned jutumärkidesse variabled mille järgi grupeerida
  library(dplyr)
  tulem=data %>%
    group_by_(...) %>%
    summarize(arv=n()) 
  #jrk õigeks
  tulem <- transform(tulem,jrk = reorder(kanal,-arv))
  tulem
}
#############tahan infot asutuse ja sihtgrupi lõikes
sihtgruppStat(andmedLai)

