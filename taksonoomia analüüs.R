##valdkondade sagedus
andmed=andmedLai

library(dplyr)
pikkus=nrow(andmed)
valdkondSagedus=andmed  %>%
  group_by(tegevusvaldkond)  %>%
  summarise(n=n()) %>%
  mutate(osakaal=n/pikkus)
#sordime
valdkondSagedus <- valdkondSagedus[order(-valdkondSagedus$osakaal),]

#valdkonna sagedus ministeeriumite järgi
valdkondSagedusMin=andmed  %>%
  group_by(ministeerium)%>%
  group_by(ministeerium,tegevusvaldkond)  %>%
  summarise(n=n())%>%
  group_by(ministeerium)  %>%
  mutate(teenusteArv=sum(n), 
         osakaal=n/teenusteArv)

#############################teenuse tüüpide sagedus
pikkus=nrow(andmed)
tyypSagedus=andmed  %>%
  group_by(teenusetyyp)  %>%
  summarise(n=length(teenusetyyp))%>%
  mutate(osakaal=n/pikkus)
#sordime
tyypSagedus <- tyypSagedus[order(-tyypSagedus$osakaal),]

#tüüpide sagedus ministeeriumite järgi
tyypSagedusMin=andmed  %>%
  group_by(ministeerium)%>%
  group_by(ministeerium,teenusetyyp)  %>%
  summarise(n=n())%>%
  group_by(ministeerium)  %>%
  mutate(teenusteArv=sum(n), 
         osakaal=n/teenusteArv)

