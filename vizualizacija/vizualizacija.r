# 3. faza: Izdelava zemljevida

# Uvozimo funkcijo za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r")
source("lib/xml.r", encoding="UTF-8")

# Uvozimo zemljevid.
cat("Uvažam zemljevid...\n")
# obcine <- uvozi.zemljevid("http://e-prostor.gov.si/fileadmin/BREZPLACNI_POD/RPE/OB.zip",
#                           "obcine", "OB/OB.shp", mapa = "zemljevid",
#                           encoding = "Windows-1250")

svet <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip",
                        "svet", "ne_110m_admin_0_countries.shp", mapa = "zemljevid",
                        encoding = "Windows-1252")

# Funkcija, ki podatke preuredi glede na vrstni red v zemljevidu
preuredi <- function(podatki, zemljevid) {
  nove.drzave <- svet$admin[!svet$admin %in% row.names(lokacije)]
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

# Preuredimo podatke, da jih bomo lahko izrisali na zemljevid.
# druzine <- preuredi(druzine, obcine)

# Izračunamo povprečno velikost družine.
# druzine$povprecje <- apply(druzine[1:4], 1, function(x) sum(x*(1:4))/sum(x))
# min.povprecje <- min(druzine$povprecje, na.rm=TRUE)
# max.povprecje <- max(druzine$povprecje, na.rm=TRUE)

# n = 100
# barve = topo.colors(n)[1+(n-1)*(druzine$povprecje-min.povprecje)/(max.povprecje-min.povprecje)]
# plot(obcine, col = barve)

# dev.off()

# crne <- !svet$admin %in% row.names(lokacije)

# test <- row.names(lokacije) %in% svet$admin
# test1 <- !row.names(lokacije) %in% svet$admin

# barve <- ifelse(crne, "black", "yellow")

lokacije <- lokacije[-c(1, 2, 4, 8, 22, 31, 34, 35, 38, 40, 41, 44, 50, 54, 59, 62, 64, 65, 66, 67, 70, 76, 89, 92, 93, 95, 96, 116), ]

lokacije <- preuredi(lokacije, svet)

ni.trgovin <- is.na(lokacije$Number.of.currently.operating.outlets)
lokacije$Number.of.currently.operating.outlets[ni.trgovin] <- 0

svet$stevilo.trgovin <- lokacije$Number.of.currently.operating.outlets

# Narišimo zemljevid v PDF.
cat("Rišem zemljevid števila trgovin...\n")
pdf("slike/stevilo_trgovin_po_svetu.pdf", width=6, height=4)

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
barve[4:21] <- rgb(1, 0.843137, 0, (as.integer(format(as.Date(lokacije$Date[4:21], "%d.%m.%Y"), "%Y")) - 1969)/10)
barve[22:39] <- rgb(0.5, 0.5, 0, (as.integer(format(as.Date(lokacije$Date[22:39], "%d.%m.%Y"), "%Y")) - 1979)/10)
barve[40:86] <- rgb(0, 0, 1, (as.integer(format(as.Date(lokacije$Date[40:86], "%d.%m.%Y"), "%Y")) - 1989)/10)
barve[87] <- rgb(0, 1, 0, (as.integer(format(as.Date(lokacije$Date[87], "%d.%m.%Y"), "%Y")) - 1999)/10)
barve[88:90] <- rgb(1, 0, 0, (as.integer(format(as.Date(lokacije$Date[88:90], "%d.%m.%Y"), "%Y")) - 2009)/10)

barve <- barve[order(row.names(lokacije))]
svet <- svet[order(svet$admin), ]

cat("Rišem zemljevid odprtja prve trgovine...\n")
pdf("slike/stevilo_trgovin_po_svetu.pdf", width=6, height=4)
plot(svet, col = barve)
dev.off()
