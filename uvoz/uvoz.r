# 2. faza: Uvoz podatkov

# Funkcija, ki uvozi podatke iz datoteke nutrition2.csv
uvoziNutrition2 <- function() {
  return(read.table("podatki/nutrition2.csv", sep = ",", skip = 1, as.is = TRUE,
                    row.names = 1,
                    col.names = c("Jed", "Serving Size (g)", "Calories", "Calories From Fat", "Total Fat (g)", "% Daily Value", "Saturated Fat (g)")
                    ))
}

# Zapišimo podatke v razpredelnico nutrition.
cat("Uvažam podatke o hranilnih vrednostih...\n")
nutrition <- uvoziNutrition2()

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.