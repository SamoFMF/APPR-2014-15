# 2. faza: Obdelava, uvoz in čiščenje podatkov
source("fontconfig.r", encoding="UTF-8")
source("uvoz/uvoz.r", encoding="UTF-8")
source("lib/tabeli.r", encoding="UTF-8")
source("lib/graf1.r", encoding="UTF-8")
source("lib/xml.r", encoding="UTF-8")


# 3. faza: Analiza in vizualizacija podatkov
source("vizualizacija/vizualizacija.r", encoding="UTF-8")

# 4. faza: Napredna analiza podatkov
source("analiza/animacija.r", encoding="UTF-8")
#source("analiza/analiza.r")

cat("Končano.\n")