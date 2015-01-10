# 3. faza: Izdelava zemljevida

# Uvozimo funkcijo za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r")
source("lib/xml.r", encoding="UTF-8")

# Uvozimo zemljevid.
cat("Uvažam zemljevid...\n")
# obcine <- uvozi.zemljevid("http://e-prostor.gov.si/fileadmin/BREZPLACNI_POD/RPE/OB.zip",
#                           "obcine", "OB/OB.shp", mapa = "zemljevid",
#                           encoding = "Windows-1250")

svet <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                        "svet", "ne_50m_admin_0_countries.shp", mapa = "zemljevid",
                        encoding = "Windows-1252")

# Funkcija, ki podatke preuredi glede na vrstni red v zemljevidu
preuredi <- function(podatki, zemljevid) {
  nove.drzave <- svet$admin[!svet$admin %in% row.names(podatki)]
  manjkajo <- ! nove.drzave %in% rownames(podatki)
  M <- as.data.frame(matrix(nrow=sum(manjkajo), ncol=length(podatki)))
  names(M) <- names(podatki)
  row.names(M) <- nove.drzave[manjkajo]
  podatki <- rbind(podatki, M)
  
  out <- data.frame(podatki[order(rownames(podatki)), ])[rank(levels(zemljevid$admin)[rank(zemljevid$admin)]), ]
  if (ncol(podatki) == 1) {
    out <- data.frame(out)
    names(out) <- names(podatki)
    rownames(out) <- rownames(podatki)
  }
  return(out)
}


lokacije <- lokacije[-c(34, 38, 40, 65, 67, 89), ]

lokacije <- preuredi(lokacije, svet)

ni.trgovin <- is.na(lokacije$Number.of.currently.operating.outlets)
lokacije$Number.of.currently.operating.outlets[ni.trgovin] <- 0

svet$stevilo.trgovin <- lokacije$Number.of.currently.operating.outlets

# Narišimo zemljevid v PDF.
cat("Rišem zemljevid števila trgovin...\n")
pdf("slike/zemljevid1.pdf", width=6, height=4)

spplot(svet, "stevilo.trgovin", col.regions = rainbow(16))

dev.off()

lokacije <- lokacije[order(as.Date(lokacije$Date, format="%d.%m.%Y", na.rm=TRUE)), ]
ni.trgovin <- is.na(lokacije$Date)

# Nekaj ne deluje vredu pri spodnji kodi - javi, da je alpha -6.9, čeprav je pri vseh med 0 in 1
# barve <- ifelse(ni.trgovin, "black",
#                 ifelse(as.integer(format(as.Date(lokacije$Date, "%d.%m.%Y"), "%Y"))>=2010, rgb(1, 0, 0, (as.integer(format(as.Date(lokacije$Date, "%d.%m.%Y"), "%Y")) - 2009)/10),
#                 ifelse(as.integer(format(as.Date(lokacije$Date, "%d.%m.%Y"), "%Y"))>=2000, rgb(0, 1, 0, (as.integer(format(as.Date(lokacije$Date, "%d.%m.%Y"), "%Y")) - 1999)/10),
#                 ifelse(as.integer(format(as.Date(lokacije$Date, "%d.%m.%Y"), "%Y"))>=1990, rgb(0, 0, 1, (as.integer(format(as.Date(lokacije$Date, "%d.%m.%Y"), "%Y")) - 1989)/10),
#                 ifelse(as.integer(format(as.Date(lokacije$Date, "%d.%m.%Y"), "%Y"))>=1980, rgb(0.5, 0.5, 0, (as.integer(format(as.Date(lokacije$Date, "%d.%m.%Y"), "%Y")) - 1979)/10),
#                 ifelse(as.integer(format(as.Date(lokacije$Date, "%d.%m.%Y"), "%Y"))>=1970, rgb(1, 0.843137, 0, (as.integer(format(as.Date(lokacije$Date, "%d.%m.%Y"), "%Y")) - 1969)/10),
#                 ifelse(as.integer(format(as.Date(lokacije$Date, "%d.%m.%Y"), "%Y"))>=1960, rgb(1, 0, 1, (as.integer(format(as.Date(lokacije$Date, "%d.%m.%Y"), "%Y")) - 1959)/10),
#                 rgb(1, 1, 0))))))))



barve <- ifelse(ni.trgovin, "black", "white")
barve[1] <- rgb(1, 1, 0)
barve[2:3] <- rgb(1, 0, 1, (as.integer(format(as.Date(lokacije$Date[2:3], "%d.%m.%Y"), "%Y")) - 1959)/10)
barve[4:26] <- rgb(1, 0.843137, 0, (as.integer(format(as.Date(lokacije$Date[4:26], "%d.%m.%Y"), "%Y")) - 1969)/10)
barve[27:47] <- rgb(0.5, 0.5, 0, (as.integer(format(as.Date(lokacije$Date[27:47], "%d.%m.%Y"), "%Y")) - 1979)/10)
barve[48:106] <- rgb(0, 0, 1, (as.integer(format(as.Date(lokacije$Date[48:106], "%d.%m.%Y"), "%Y")) - 1989)/10)
barve[107:109] <- rgb(0, 1, 0, (as.integer(format(as.Date(lokacije$Date[107:109], "%d.%m.%Y"), "%Y")) - 1999)/10)
barve[110:112] <- rgb(1, 0, 0, (as.integer(format(as.Date(lokacije$Date[110:112], "%d.%m.%Y"), "%Y")) - 2009)/10)

barve <- barve[order(row.names(lokacije))]
svet <- svet[order(svet$admin), ]

drzave <- read.csv("podatki/drzave_sveta_2.csv", row.names=4, sep=";")
drzave <- drzave[order(row.names(drzave)), ]
row.names(drzave)[c(194, 234)] <- c("Republic of Serbia", "United States of America")
drzave <- drzave[-grep(TRUE, (!row.names(drzave) %in% svet$admin)), ]
drzave <- preuredi(drzave, svet)
drzave <- drzave[order(row.names(drzave)), ]

povrsina <- read.csv("podatki/povrsina.csv", row.names=2, sep=";")
row.names(povrsina)[c(3, 106)] <- c("United States of America", "Republic of Serbia")
povrsina <- povrsina[-grep(TRUE, (!row.names(povrsina) %in% svet$admin)), ]
povrsina <- preuredi(povrsina, svet)
povrsina <- povrsina[order(row.names(povrsina)), ]

drzave$Area <- povrsina$Area
slabe <- is.na(drzave$Area)
levels(drzave$Area) <- c(levels(drzave$Area), 0)
drzave$Area[slabe] <- 0
lok1 <- lokacije[order(row.names(lokacije)), ]

vsa.imena <- (drzave$Area > 500000) & (lok1$Number.of.currently.operating.outlets > 0)
koordinate <- drzave[vsa.imena, c("long", "lat")]
imena <- drzave[vsa.imena, ]


cat("Rišem zemljevid odprtja prve trgovine...\n")
pdf("slike/zemljevid2.pdf", width=6, height=4)
plot(svet, col = barve)

text(coordinates(imena[c("long", "lat")]),
     label = ifelse(imena$Area>2000000, as.character(row.names(imena)), ifelse(imena$Area>1000000, as.character(imena$country), "")),
     cex = 0.25, col= rgb(0, 0.5, 0))


points(coordinates(imena[imena$Area<1000000, c("long", "lat")]),
       col = "grey",
       pch = 19,
       cex = 0.2)

dev.off()