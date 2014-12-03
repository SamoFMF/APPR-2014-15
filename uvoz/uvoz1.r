# 2. faza: Uvoz podatkov

# Funkcija, ki uvozi podatke iz datoteke nutrition.csv
uvoziNutrition <- function() {
  return(read.table("podatki/nutrition.csv", sep = ",", skip = 1, as.is = TRUE,
                    row.names = NULL,
                    col.names = c("Jed", "Serving Size (g)", "Calories", "Calories From Fat", "Total Fat (g)", "% Daily Value", "Saturated Fat (g)", "% Daily Value", "Tans Fat (g)", "Cholesterol (mg)", "% Daily Value", "Sodium (mg)", "% Daily Value", "Carbohydrates (g)", "% Daily Value", "Dietary Fiber (g)", "% Daily Value", "Sugars (g)", "Protein (g)", "Vitamin A", "Vitamin C", "Calcium", "Iron")
  ))
}

# Zapišimo podatke v razpredelnico nutrition..
cat("Uvažam podatke o sestavinah...\n")
nutrition2 <- uvoziNutrition()

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.