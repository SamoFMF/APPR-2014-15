# 2. faza: Uvoz podatkov

# Funkcija, ki uvozi podatke iz datoteke nutrition2.csv
uvoziNutrition <- function() {
  return(read.table("podatki/nutrition.csv", sep = ",", skip = 1, as.is = TRUE,
                    row.names = NULL,
                    col.names = c("Jed", "Serving Size (g)", "Calories", "Calories From Fat", "Total Fat (g)", "Part of Daily Value", "Saturated Fat (g)", "Part of Daily Value", "Trans Fat (g)", "Cholesterol (mg)", "Part of Daily Value", "Sodium (mg)", "Part of Daily Value", "Carbohydrates (g)", "Part of Daily Value", "Dietary Fiber (g)", "Part of Daily Value", "Sugars (g)", "Protein (g)", "Vitamin A", "Vitamin C", "Calcium", "Iron"),
                    fileEncoding = "Windows-1250"
  ))
}

# Zapišimo podatke v razpredelnico nutrition.
cat("Uvažam podatke o hranilnih vrednostih...\n")
nutrition <- uvoziNutrition()

# Funkcija, ki uvozi podatke iz datoteke sestavine.csv
uvoziSestavine <- function() {
  return(read.table("podatki/sestavine.csv", sep = ";", skip = 1, as.is = TRUE,
                    row.names = NULL,
                    col.names = c("Jed", "BEEF PATTY", "SHREDDED LETTUCE", "CHEESE", "PICKLE SLICES", "REGULAR BUN", "MCCHICKEN PATTY"),
                    fileEncoding = "Windows-1250"
  ))
}

# Zapišimo podatke v razpredelnico sestavine.
cat("Uvažam podatke o sestavinah...\n")
sestavine <- uvoziSestavine()

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.