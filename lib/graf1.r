source("lib/tabeli.r")

prvi.graf <- function() {
  attach(nutrition)
  y <- table(Calories.per.Serving.Size)
  
  plot(c(0, 5), c(0, 41), type="n", xlab = "Kategorija \n (Glede na kalorije na velikost porcije)", ylab = "Stevilo jedi")
  abline(h = 0)
  abline(v = 0)
  
  rect(0:4, 0, 1:5, y, col = c("red", "blue", "#82c5dc", "grey", "green"))
  
  lines(c(0, 1), c(y[2], y[2]), lty = "dashed")
  lines(c(0, 2), c(y[3], y[3]), lty = "dashed")
  lines(c(0, 0.25), c(y[4], y[4]), lty = "dashed")
  
  text(0.07, y[1:4], pos = 2, y[1:4])

  
  legend("topright",
         legend = rownames(y),
         col = c("red", "blue", "#82c5dc", "grey", "green"),
         lty = c("solid", "solid", "solid", "solid", "solid"),
         bg = "white")

  
  detach(nutrition)
}

drugi.graf <- function() {
  attach(sestavine)
  y <- table(RAZVRSTITEV)
  barplot(y, names.arg = rownames(y), col = c("grey", "#82c5dc", "red"), xlab = "Razvrstitev \n (Glede na stevilo sestavin iz seznama, ki so v jedi)", ylab = "Stevilo jedi")
  detach(sestavine)
}

tretji.graf <- function() {
  attach(nutrition)
  
  urejeni <- nutrition[order(Calories, decreasing=TRUE),]
  plot(c(0, 16), c(-80, 760), type="n", xlab = "Jedi", ylab = "Kalorije")
  abline(h = 0)
  abline(v = 0)
  
  rect(11:15, 0, 12:16, urejeni[5:1, 3])
  rect(6:10, 0, 7:11, urejeni[30:26, 3])
  rect(0:5, 0, 1:6, sort(tail(urejeni[,3])))
  
  lines(c(-0.05, 15), c(urejeni[1, 3], urejeni[1, 3]), lty = "dashed")
  lines(c(-0.05, 8), c(urejeni[28, 3], urejeni[28, 3]), lty = "dashed")
  lines(c(-0.05, 0.05), c(100, 100))
  lines(c(-0.05, 0.05), c(300, 300))
  lines(c(-0.05, 0.05), c(500, 500))
  lines(c(-0.05, 0.05), c(700, 700))
  lines(c(-0.05, 0), c(sort(tail(urejeni[,3]))[1], sort(tail(urejeni[,3]))[1]))
  
  text(0.15, c(100, 300, 500, 700), pos = 2, c(100, 300, 500, 700), cex = 0.5)
  text(0.15, sort(tail(urejeni[,3]))[1], pos = 2, sort(tail(urejeni[,3]))[1], cex = 0.5)
  text(0.15, urejeni[28, 3], pos = 2, urejeni[28, 3], cex = 0.5)
  text(0.15, urejeni[1, 3], pos = 2, urejeni[1, 3], cex = 0.5)
  
  text(0.5:5.5, c(-60, -60, -60, -60, -60, -60), substr(sort(tail(urejeni[,1])), 1, 12), srt = -60, cex = 0.5)
  text(6.5:10.5, c(-60, -60, -60, -60, -60, -60), substr(urejeni[30:26, 1], 1, 12), srt = -60, cex = 0.5)
  text(11.5:15.5, c(-60, -60, -60, -60, -60, -60), substr(urejeni[5:1, 1], 1, 12), srt = -60, cex = 0.5)
  
  detach(nutrition)
}

#Izven uporabe
# tretji.graf1 <- function() {
#   attach(nutrition)
#   plot(0:54, Calories, type = "l", xlab = "Jedi", ylab = "Kalorije")
#   text(grep(max(Calories), Calories), max(Calories), pos = 4, Jed[grep(max(Calories), Calories)])
#   text(grep(min(Calories), Calories), min(Calories), pos = 2, Jed[grep(min(Calories), Calories)])
#   detach(nutrition)
# }

# pdf("slike/grafi.pdf", paper = "a4r")
# graf1 <- prvi.graf()
# graf2 <- drugi.graf()
# graf3 <- tretji.graf()
# dev.off()

pdf("slike/graf1.pdf")
graf1 <- prvi.graf()
dev.off()

pdf("slike/graf2.pdf")
graf1 <- drugi.graf()
dev.off()

pdf("slike/graf3.pdf")
graf1 <- tretji.graf()
dev.off()