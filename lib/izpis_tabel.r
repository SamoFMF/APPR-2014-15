library(gridExtra)

cat("Rišem tabele...\n")

prikaz_nutrition <- nutrition[1:30, c(1:4, 22:24)]
row.names(prikaz_nutrition) <- prikaz_nutrition$Jed
prikaz_nutrition <- prikaz_nutrition[, -1]

cairo_pdf("slike/tabela_nutrition.pdf", height=7, width=8.5, family="Courier New")
grid.table(prikaz_nutrition, gp=gpar(fontsize=8))
dev.off()

prikaz_sestavine <- sestavine[, c(2:4, 7:9)]

cairo_pdf("slike/tabela_sestavine.pdf", height=4, width=8.5, family="Courier New")
grid.table(prikaz_sestavine, gp=gpar(fontsize=8))
dev.off()

prikaz_lokacije <- lokacije[1:40, ]

cairo_pdf("slike/tabela_lokacije.pdf", height=9, width=8.5, family="Courier New")
grid.table(prikaz_lokacije, gp=gpar(fontsize=7))
dev.off()