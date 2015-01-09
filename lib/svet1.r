source("lib/uvozi.zemljevid.r", encoding="UTF-8")
source("lib/xml.r", encoding="UTF-8")

crne <- !svet$admin %in% row.names(lokacije)

test <- row.names(lokacije) %in% svet$admin
test1 <- !row.names(lokacije) %in% svet$admin

barve <- ifelse(crne, "black", "yellow")

plot(svet, col = barve)