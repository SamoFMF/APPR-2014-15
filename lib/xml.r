# Uvoz s spletne strani

library(XML)
Sys.setlocale("LC_TIME", "C")

# Vrne vektor nizov z odstranjenimi začetnimi in končnimi "prazninami" (whitespace)
# iz vozlišč, ki ustrezajo podani poti.
stripByPath <- function(x, path) {
  unlist(xpathApply(x, path,
                    function(y) gsub("^\\s*(.*?)\\s*$", "\\1",
                                     gsub("\\[[^]]*\\]", "",
                                          xmlValue(y)))))
}

uvozi.lokacije <- function() {
  url.lokacije <- "http://en.wikipedia.org/wiki/List_of_countries_with_McDonald%27s_restaurants"
  doc.lokacije <- htmlTreeParse(url.lokacije, useInternalNodes=TRUE, encoding="Windows-1250")
  
  # Poiščemo vse tabele v dokumentu
  tabele <- getNodeSet(doc.lokacije, "//table")
  
  # Iz druge tabele dobimo seznam vrstic (<tr>) neposredno pod
  # trenutnim vozliščem
  vrstice <- getNodeSet(tabele[[2]], "./tr")
  
  # Seznam vrstic pretvorimo v seznam (znakovnih) vektorjev
  # s porezanimi vsebinami celic (<td>) neposredno pod trenutnim vozliščem
  seznam <- lapply(vrstice[2:length(vrstice)], stripByPath, "./td")
  
  # Znebimo se CEO stolpca, ki ni bil uporaben ter je samo povzročal težave
  seznam <- lapply(seznam, function(x) x[1:6])
  
  # Iz seznama vrstic naredimo matriko
  matrika <- matrix(unlist(seznam), nrow=length(seznam), byrow=TRUE)
  
  # Imena stolpcev matrike dobimo iz celic (<th>) glave (prve vrstice) prve tabele
  colnames(matrika) <- gsub("\n", " ", stripByPath(vrstice[[1]], ".//th"))[2:7]
  
  # Znebimo se teksta in nepotrebnih znakov pri številu McDonaldsov
  matrika[,4] <- gsub("[a-z ]", "", matrika[,4])
  matrika[53,4] <- sum(as.numeric(unlist(strsplit(matrika[53,4], split = "+", fixed = TRUE))))
  matrika[,4] <- gsub("[+,]", "", matrika[,4])
  
  # Popravimo datume, ki niso bili v pravilnem formatu, ali pa so bili pomanjlkjivi
  matrika[17,2] <- "November 13, 1974"
  matrika[117,2] <- "August 1, 2011"
  
  matrika[,2] <- format(as.Date(matrika[,2], "%B %d, %Y"), "%d.%m.%Y")
  
  matrika[,1] <- gsub("[^a-z ]", "", matrika[,1], ignore.case = TRUE)
  
  #Popravimo/posodobimo nekatera imena držav
  matrika[17, 1] <- "United Kingdom"
  matrika[44, 1] <- "Macau"
  matrika[45, 1] <- "Serbia"
  matrika[48, 1] <- "Russia"
  matrika[56, 1] <- "Czech Republic"
  
  # Podatke iz matrike spravimo v razpredelnico
  return(
    data.frame(Country.or.territory = matrika[,1],
               First.outlet.location = matrika[,3],
               Number.of.currently.operating.outlets = as.integer(matrika[,4]),
               Date = matrika[,2],
               row.names=matrika[,1])
  )
}

lokacije <- uvozi.lokacije()
lokacije <- lokacije[order(lokacije[,1]),]
lokacije <- lokacije[-1]

#Lahko bi tudi odstranili stolpec "Country.or.territory" z lokacije <- lokacije[-1]