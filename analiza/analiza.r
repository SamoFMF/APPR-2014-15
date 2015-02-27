# 4. faza: Analiza podatkov

# Uvozimo funkcijo za uvoz spletne strani.
# source("lib/xml.r")

# # Preberemo spletno stran v razpredelnico.
# cat("Uvažam spletno stran...\n")
# tabela <- preuredi(uvozi.obcine(), obcine)
# 
# # Narišemo graf v datoteko PDF.
# cat("Rišem graf...\n")
# pdf("slike/naselja.pdf", width=6, height=4)
# plot(tabela[[1]], tabela[[4]],
#      main = "Število naselij glede na površino občine",
#      xlab = "Površina (km^2)",
#      ylab = "Št. naselij")
# dev.off()

podatki <- nutrition
podatki$Part.of.Daily.Value <- NULL
podatki$Part.of.Daily.Value.1 <- NULL
podatki$Part.of.Daily.Value.2 <- NULL
podatki$Part.of.Daily.Value.3 <- NULL
podatki$Part.of.Daily.Value.4 <- NULL
podatki$Part.of.Daily.Value.5 <- NULL
podatki$Calories.per.Serving.Size <- NULL
podatki$Dietary.Fiber..g. <- NULL
podatki$Trans.Fat..g. <- NULL
podatki$Saturated.Fat..g. <- NULL

podatki <- podatki[-(48:54),]
row.names(podatki) <- podatki$Jed
podatki <- podatki[, -1]

skalar1 <- scale(podatki)

k <- kmeans(skalar1, 6, nstart=1000)
kat <- k$cluster
barve <- c("red", "green", "blue", "yellow", "gold", "black")

# Uporabimo pairs
# pairs(skalar1, col = barve[kat])
# Vidimo, da je modro obarvan element samo eden (table(k$cluster)), ki je pa tudi osamelec
# Z row.names(skalar1)[kat %in% 3] dobimo ime osamelca
# Ime je "Double Quarter Pounder with Cheese ++"
# pairs(skalar1[!(kat %in% 3),], col=barve[kat[!(kat %in% 3)]])
# Vidimo, da razen pri Vitamin A, ki ga ne bomo uporabili, več ni osamelcev
# Prvotni osamelec nato odstranimo

kat <- kat[!(row.names(podatki) == "Double Quarter Pounder with Cheese ++")]
podatki <- podatki[!(row.names(podatki) == "Double Quarter Pounder with Cheese ++"), ]

pdf("slike/grupiranje1.pdf")

plot(podatki[, "Serving.Size..g."], podatki[, "Calories"],
     col = barve[kat],
     xlab = "Masa jedi v gramih",
     ylab = "Kalorije",
     main = "Razmerje med velikostjo porcije ter kalorijami le-te")

dev.off()

pdf("slike/grupiranje2.pdf")

plot(podatki$Calories, podatki$Calories.From.Fat,
     col = barve[kat],
     xlab = "Kalorije",
     ylab = "Kalorije iz maščob",
     main = "Razmerje med celotnimi kalorijami in kalorijami iz maščob")

dev.off()