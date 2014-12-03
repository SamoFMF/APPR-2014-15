source("uvoz/uvoz.r")

ustvari.sestavine <- function() {
  attach(sestavine)
  #Izračuna, koliko sestavin iz seznama vsaka jed vsebuje
  St.sestavin <- apply(sestavine[-1], 1, sum)
  
  #V katere kategorije vse jih delimo
  kategorije <- c("below average", "average", "above average")
  razvrstitev <- character(length(St.sestavin))
  razvrstitev[St.sestavin>3] <- "above average"
  razvrstitev[St.sestavin==2|St.sestavin==3] <- "average"
  razvrstitev[St.sestavin<2] <- "below average"
  Razvrstitev <- factor(razvrstitev, levels = kategorije, ordered = TRUE)
  return(data.frame(JED = Jed, BEEF.PATTY, SHREDDED.LETTUCE, CHEESE, PICKLE.SLICES, REGULAR.BUN, MCCHICKEN.PATTY, ST.SESTAVIN.S.SEZNAMA = St.sestavin, RAZVRSTITEV = Razvrstitev))
  detach(sestavine)
}


ustvari.nutrition <- function() {
  attach(nutrition)
  
  Serving.Size..g. <- gsub("[( g)]", "", Serving.Size..g., ignore.case=TRUE)
  Serving.Size..g. <- as.numeric(Serving.Size..g.)
  
  #Število kalorij glede na maso jedi
  Razmerje <- Calories / Serving.Size..g.
  
  kategorije <- c("najmanjse", "nizko", "srednje", "visoko", "najvecje")
  razvrstitev <- character(length(Razmerje))
  razvrstitev[Razmerje>(mean(Razmerje)+sd(Razmerje))] <- "visoko"
  razvrstitev[Razmerje<=(mean(Razmerje)+sd(Razmerje))&Razmerje>=(mean(Razmerje)-sd(Razmerje))] <- "srednje"
  razvrstitev[Razmerje<(mean(Razmerje)-sd(Razmerje))] <- "nizko"
  razvrstitev[max(Razmerje)] <- "najvecje"
  razvrstitev[min(Razmerje)] <- "najmanjse"
  
  Razvrstitev <- factor(razvrstitev, levels = kategorije, ordered = TRUE)
  
  return(data.frame(Jed, Serving.Size..g., Calories, Calories.From.Fat, Total.Fat..g., Part.of.Daily.Value, Saturated.Fat..g., Part.of.Daily.Value.1, Trans.Fat..g., Cholesterol..mg., Part.of.Daily.Value.2, Sodium..mg., Part.of.Daily.Value.3, Carbohydrates..g., Part.of.Daily.Value.4, Dietary.Fiber..g., Part.of.Daily.Value.5, Sugars..g., Protein..g., Vitamin.A, Vitamin.C, Calcium, Iron, Calories.per.Serving.Size = Razvrstitev))
  detach(nutrition)
}

sestavine <- ustvari.sestavine()
nutrition <- ustvari.nutrition()