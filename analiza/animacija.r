# 4. faza: Animacija

cat("Izdelujem animacijo...\n")
pdf("slike/animacija.pdf", width=6, height=4)

# Prazen zemljevid
plot(svet)
title(main = "Države po datumih odprtja prve restavracije")
legend("bottomleft",
       legend = c("1940-1949", "", "", "", "", "", ""),
       col = c("white", "white", "white", "white", "white", "white", "white"),
       lty = c("solid", "solid", "solid", "solid", "solid", "solid", "solid"),
       lwd = c(8, 8, 8, 8, 8, 8, 8),
       bg = "white", title = "Legenda", cex = 0.5, text.col = "white", title.col = "black"
)

# Prvo obdobje (1940)
an_barve <- ifelse(barve == rgb(1, 1, 0), "red", "white")
plot(svet, col = an_barve)
title(main = "Države po datumih odprtja prve restavracije")
legend("bottomleft",
       legend = c("1940-1949", "", "", "", "", "", ""),
       col = c("red", "white", "white", "white", "white", "white", "white"),
       lty = c("solid", "solid", "solid", "solid", "solid", "solid", "solid"),
       lwd = c(8, 8, 8, 8, 8, 8, 8),
       bg = "white", title = "Legenda", cex = 0.5
)

# Drugo obdobje (1960-1969)
an_barve[an_barve == "red"] <- rgb(1, 1, 0)
an_barve[(as.integer(format(as.Date(lok1$Date, "%d.%m.%Y"), "%Y")) < 1970) & (as.integer(format(as.Date(lok1$Date, "%d.%m.%Y"), "%Y")) >= 1960)] <- "red"
an_imena <- imena[imena$leto < 1960, ]
plot(svet, col = an_barve)
title(main = "Države po datumih odprtja prve restavracije")
legend("bottomleft",
       legend = c("1940-1949", "1960-1969", "", "", "", "", ""),
       col = c(rgb(1, 1, 0), "red", "white", "white", "white", "white", "white"),
       lty = c("solid", "solid", "solid", "solid", "solid", "solid", "solid"),
       lwd = c(8, 8, 8, 8, 8, 8, 8),
       bg = "white", title = "Legenda", cex = 0.5
)
text(coordinates(an_imena[c("long", "lat")]),
     label = an_imena$slo,
     cex = 0.25, col = rgb(0, 0.5, 0))

# Tretje obdobje (1970-1979)
an_barve[an_barve == "red"] <- barve[an_barve == "red"]
an_barve[(as.integer(format(as.Date(lok1$Date, "%d.%m.%Y"), "%Y")) < 1980) & (as.integer(format(as.Date(lok1$Date, "%d.%m.%Y"), "%Y")) >= 1970)] <- "red"
an_imena <- imena[imena$leto < 1970, ]
plot(svet, col = an_barve)
title(main = "Države po datumih odprtja prve restavracije")
legend("bottomleft",
       legend = c("1940-1949", "1960-1969", "1970-1979", "", "", "", ""),
       col = c(rgb(1, 1, 0), rgb(1, 0, 1), "red", "white", "white", "white", "white"),
       lty = c("solid", "solid", "solid", "solid", "solid", "solid", "solid"),
       lwd = c(8, 8, 8, 8, 8, 8, 8),
       bg = "white", title = "Legenda", cex = 0.5
)
text(coordinates(an_imena[c("long", "lat")]),
     label = an_imena$slo,
     cex = 0.25, col = c("gold", rgb(0, 0.5, 0)))

# Cetrto obdobje (1980-1989)
an_barve[an_barve == "red"] <- barve[an_barve == "red"]
an_barve[(as.integer(format(as.Date(lok1$Date, "%d.%m.%Y"), "%Y")) < 1990) & (as.integer(format(as.Date(lok1$Date, "%d.%m.%Y"), "%Y")) >= 1980)] <- "red"
an_imena <- imena[imena$leto < 1980, ]
plot(svet, col = an_barve)
title(main = "Države po datumih odprtja prve restavracije")
legend("bottomleft",
       legend = c("1940-1949", "1960-1969", "1970-1979", "1980-1989", "", "", ""),
       col = c(rgb(1, 1, 0), rgb(1, 0, 1), rgb(1, 0.843137, 0), "red", "white", "white", "white"),
       lty = c("solid", "solid", "solid", "solid", "solid", "solid", "solid"),
       lwd = c(8, 8, 8, 8, 8, 8, 8),
       bg = "white", title = "Legenda", cex = 0.5
)
text(coordinates(an_imena[c("long", "lat")]),
     label = ifelse(an_imena$Area>2000000, as.character(an_imena$slo), ifelse(an_imena$Area>1000000, as.character(an_imena$country), "")),
     cex = 0.25,
     col = ifelse(an_imena$slo == "", "black",
                  ifelse(row.names(an_imena) == "Canada", "gold", rgb(0, 0.5, 0))))
points(coordinates(an_imena[an_imena$Area<1000000, c("long", "lat")]),
       col = "red",
       pch = 19,
       cex = 0.2)

# Peto obdobje (1990-1999)
an_barve[an_barve == "red"] <- barve[an_barve == "red"]
an_barve[(as.integer(format(as.Date(lok1$Date, "%d.%m.%Y"), "%Y")) < 2000) & (as.integer(format(as.Date(lok1$Date, "%d.%m.%Y"), "%Y")) >= 1990)] <- "red"
an_imena <- imena[imena$leto < 1990, ]
plot(svet, col = an_barve)
title(main = "Države po datumih odprtja prve restavracije")
legend("bottomleft",
       legend = c("1940-1949", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "", ""),
       col = c(rgb(1, 1, 0), rgb(1, 0, 1), rgb(1, 0.843137, 0), rgb(0.5, 0.5, 0), "red", "white", "white"),
       lty = c("solid", "solid", "solid", "solid", "solid", "solid", "solid"),
       lwd = c(8, 8, 8, 8, 8, 8, 8),
       bg = "white", title = "Legenda", cex = 0.5
)
text(coordinates(an_imena[c("long", "lat")]),
     label = ifelse(an_imena$Area>2000000, as.character(an_imena$slo), ifelse(an_imena$Area>1000000, as.character(an_imena$country), "")),
     cex = 0.25,
     col = ifelse(an_imena$slo == "", "black",
                  ifelse(row.names(an_imena) == "Canada", "gold", rgb(0, 0.5, 0))))
points(coordinates(an_imena[an_imena$Area<1000000, c("long", "lat")]),
       col = "red",
       pch = 19,
       cex = 0.2)
text(coordinates(postrani[1, c("long", "lat")]),
     label = "Argentina",
     cex = 0.2, srt = -90, col = "red")

# Sesto obdobje (2000-2009)
an_barve[an_barve == "red"] <- barve[an_barve == "red"]
an_barve[(as.integer(format(as.Date(lok1$Date, "%d.%m.%Y"), "%Y")) < 2010) & (as.integer(format(as.Date(lok1$Date, "%d.%m.%Y"), "%Y")) >= 2000)] <- "red"
an_imena <- imena[imena$leto < 2000, ]
plot(svet, col = an_barve)
title(main = "Države po datumih odprtja prve restavracije")
legend("bottomleft",
       legend = c("1940-1949", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", ""),
       col = c(rgb(1, 1, 0), rgb(1, 0, 1), rgb(1, 0.843137, 0), rgb(0.5, 0.5, 0), rgb(0, 0, 1), "red", "white"),
       lty = c("solid", "solid", "solid", "solid", "solid", "solid", "solid"),
       lwd = c(8, 8, 8, 8, 8, 8, 8),
       bg = "white", title = "Legenda", cex = 0.5
)
text(coordinates(an_imena[c("long", "lat")]),
     label = ifelse(an_imena$Area>2000000, as.character(an_imena$slo), ifelse(an_imena$Area>1000000, as.character(an_imena$country), "")),
     cex = ifelse(row.names(an_imena) == "Saudi\nArabia", 0.2, 0.25),
     col = ifelse(row.names(an_imena) == "Indonesia", "red", 
                  ifelse(an_imena$slo == "", "black",
                         ifelse((row.names(an_imena) == "India") | (row.names(an_imena) == "Saudi\nArabia"), "red",
                                ifelse(row.names(an_imena) == "Canada", "gold", rgb(0, 0.5, 0))))))
points(coordinates(an_imena[an_imena$Area<1000000, c("long", "lat")]),
       col = "red",
       pch = 19,
       cex = 0.2)
text(coordinates(postrani[c("long", "lat")]),
     label = ifelse(row.names(postrani) == "Argentina", as.character(row.names(postrani)), as.character(postrani$country)),
     cex = c(0.2, 0.25), srt = -90, col = c("red", "black"))

# Sedmo obdobje (2010-2015)
an_barve[an_barve == "red"] <- barve[an_barve == "red"]
an_barve[(as.integer(format(as.Date(lok1$Date, "%d.%m.%Y"), "%Y")) < 2015) & (as.integer(format(as.Date(lok1$Date, "%d.%m.%Y"), "%Y")) >= 2010)] <- "red"
an_imena <- imena[imena$leto < 2010, ]
plot(svet, col = an_barve)
title(main = "Države po datumih odprtja prve restavracije")
legend("bottomleft",
       legend = c("1940-1949", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2015"),
       col = c(rgb(1, 1, 0), rgb(1, 0, 1), rgb(1, 0.843137, 0), rgb(0.5, 0.5, 0), rgb(0, 0, 1), rgb(0, 1, 0), "red"),
       lty = c("solid", "solid", "solid", "solid", "solid", "solid", "solid"),
       lwd = c(8, 8, 8, 8, 8, 8, 8),
       bg = "white", title = "Legenda", cex = 0.5
)
text(coordinates(an_imena[c("long", "lat")]),
     label = ifelse(an_imena$Area>2000000, as.character(an_imena$slo), ifelse(an_imena$Area>1000000, as.character(an_imena$country), "")),
     cex = ifelse(row.names(an_imena) == "Saudi\nArabia", 0.2, 0.25),
     col = ifelse(row.names(an_imena) == "Indonesia", "red", 
                  ifelse(an_imena$slo == "", "black",
                         ifelse((row.names(an_imena) == "India") | (row.names(an_imena) == "Saudi\nArabia"), "red",
                                ifelse(row.names(an_imena) == "Canada", "gold", rgb(0, 0.5, 0))))))
points(coordinates(an_imena[an_imena$Area<1000000, c("long", "lat")]),
       col = "red",
       pch = 19,
       cex = 0.2)
text(coordinates(postrani[c("long", "lat")]),
     label = ifelse(row.names(postrani) == "Argentina", as.character(row.names(postrani)), as.character(postrani$country)),
     cex = c(0.2, 0.25), srt = -90, col = c("red", "black"))

# Končni zemljevid
an_barve[an_barve == "red"] <- barve[an_barve == "red"]
plot(svet, col = an_barve)
title(main = "Države po datumih odprtja prve restavracije")
legend("bottomleft",
       legend = c("1940-1949", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2015"),
       col = c(rgb(1, 1, 0), rgb(1, 0, 1), rgb(1, 0.843137, 0), rgb(0.5, 0.5, 0), rgb(0, 0, 1), rgb(0, 1, 0), rgb(1, 0, 0)),
       lty = c("solid", "solid", "solid", "solid", "solid", "solid", "solid"),
       lwd = c(8, 8, 8, 8, 8, 8, 8),
       bg = "white", title = "Legenda", cex = 0.5
)
text(coordinates(an_imena[c("long", "lat")]),
     label = ifelse(an_imena$Area>2000000, as.character(an_imena$slo), ifelse(an_imena$Area>1000000, as.character(an_imena$country), "")),
     cex = ifelse(row.names(an_imena) == "Saudi\nArabia", 0.2, 0.25),
     col = ifelse(row.names(an_imena) == "Indonesia", "red", 
                  ifelse(an_imena$slo == "", "black",
                         ifelse((row.names(an_imena) == "India") | (row.names(an_imena) == "Saudi\nArabia"), "red",
                                ifelse(row.names(an_imena) == "Canada", "gold", rgb(0, 0.5, 0))))))
points(coordinates(an_imena[an_imena$Area<1000000, c("long", "lat")]),
       col = "red",
       pch = 19,
       cex = 0.2)
text(coordinates(postrani[c("long", "lat")]),
     label = ifelse(row.names(postrani) == "Argentina", as.character(row.names(postrani)), as.character(postrani$country)),
     cex = c(0.2, 0.25), srt = -90, col = c("red", "black"))

dev.off()