# 2. faza: Obdelava, uvoz in čiščenje podatkov
source("lib/tabeli.r", encoding="UTF-8")
source("lib/graf1.r", encoding="UTF-8")
source("lib/xml.r", encoding="UTF-8")
source("vizualizacija/vizualizacija.r", encoding="UTF-8")

# Potrebno naknadno zagnati z "Run", če ne ne bo odprlo 'slike/zemljevid1.pdf'!
pdf("slike/zemljevid1.pdf", width=6, height=4)

spplot(svet, "stevilo.trgovin", col.regions = rainbow(16))

dev.off()

# 3. faza: Analiza in vizualizacija podatkov
#source("vizualizacija/vizualizacija.r")

# 4. faza: Napredna analiza podatkov
#source("analiza/analiza.r")

cat("Končano.\n")