#funktsioon andmete alla laadimiseks ja sisselugemiseks
andmedSisse=function(url) {
  failinimi=paste0(Sys.Date(), "_riigiteenused.json")
  download.file(url=url, failinimi, method="auto", 
                quiet = FALSE, mode = "w",cacheOK = TRUE,
                extra = getOption("download.file.extra"))
  
  #loeme andmed sisse
  library(jsonlite)
  andmed=fromJSON(failinimi, flatten=T)
  andmed
}
#proovime
andmed=andmedSisse("https://www.riigiteenused.ee/api/et/all")

