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

podatki <- nutrition[, -c(6, 7, 8, 9, 11, 13, 15, 16, 17, 24)]

podatki <- podatki[-(48:54),]
row.names(podatki) <- podatki$Jed
podatki <- podatki[, -1]

skalar1 <- scale(podatki)

k <- kmeans(skalar1, 6, nstart=1000)
kat <- k$cluster
barve <- c("red", "green", "blue", "yellow", "gold", "black")


# Uporabimo pairs
pdf("slike/pairs1.pdf")
pairs(skalar1, col = barve[kat])
dev.off()
# Vidimo, da imamo osamelec, ki ga lahko dobimo z naslednjo funkcijo
os_st <- as.integer(row.names(table(k$cluster))[table(k$cluster) == 1]) # Vrne grupo osamelca v tem primeru
os <- row.names(skalar1)[kat %in% os_st] # Ime osamelca
# Pogledamo pairs brez le-tega
pdf("slike/pairs2.pdf")
pairs(skalar1[!(kat %in% os_st), ], col = barve[kat[!(kat %in% os_st)]])
dev.off()
# Osamelcev več ni v podatkih, ki jih bomo uporabili
# Prvotni osamelec nato odstranimo iz podatkov

podatki <- podatki[!(kat %in% os_st), ]
kat <- kat[!(kat %in% os_st)]

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