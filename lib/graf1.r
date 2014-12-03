source("lib/tabeli.r")

prvi.graf <- function() {
  attach(nutrition)
  y1 <- 1
  y2 <- length(grep("nizko", Calories.per.Serving.Size))
  y3 <- length(grep("srednje", Calories.per.Serving.Size))
  y4 <- length(grep("visoko", Calories.per.Serving.Size))
  y5 <- 1
  
  plot(c(0, 5), c(0, 41), type="n", xlab = "Kategorija \n (Glede na kalorije na velikost porcije)", ylab = "Stevilo jedi")
  abline(h = 0)
  abline(v = 0)
  rect(0, 0, 1, y1, col = "red")
  rect(1, 0, 2, y2, col = "blue")
  rect(2, 0, 3, y3, col = "#82c5dc")
  rect(3, 0, 4, y4, col = "grey")
  rect(4, 0, 5, y5, col = "green")
  
  lines(c(0, 1), c(y2, y2), lty = "dashed")
  lines(c(0, 2), c(y3, y3), lty = "dashed")
  lines(c(0, 0.25), c(y4, y4), lty = "dashed")
  
  
  text(0, y1, pos = 2, y1)
  text(0, y2, pos = 2, y2)
  text(0, y3, pos = 2, y3)
  text(0, y4, pos = 2, y4)
  
  legend("topright",
         legend = c("najslabse", "nizko", "srednje", "visoko", "najboljse"),
         col = c("red", "blue", "#82c5dc", "grey", "green"),
         lty = c("solid", "solid", "solid", "solid", "solid"),
         bg = "white")

  
  detach(nutrition)
}

drugi.graf <- function() {
  attach(sestavine)
  vrednosti <- c(length(grep("below average", RAZVRSTITEV)), length(grep("average", RAZVRSTITEV)), length(grep("above average", RAZVRSTITEV)))
  barplot(vrednosti, names.arg = c("below average", "average", "above average"), col = c("grey", "#82c5dc", "red"), xlab = "Razvrstitev \n (Glede na Stevilo sestavin iz seznama, ki so v jedi)", ylab = "Stevilo jedi")
  detach(sestavine)
}

tretji.graf <- function() {
  attach(nutrition)
  plot(0:55, Calories, type = "l")
  text(grep(max(Calories), Calories), max(Calories), pos = 2, cex = c(0.65, 0.75), Jed[grep(max(Calories), Calories)])
  text(grep(min(Calories), Calories), min(Calories), pos = 2, Jed[grep(min(Calories), Calories)])
}

pdf("slike/graf1.pdf", paper = "a4r")
graf1 <- prvi.graf()
graf2 <- drugi.graf()
graf3 <- tretji.graf()
dev.off()

