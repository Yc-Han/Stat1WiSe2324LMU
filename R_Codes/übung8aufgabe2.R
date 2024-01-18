feinstaub <- c(20, 27, 22, 20, 22, 20, 19, 20, 17, 27, 23, 27, 22, 27, 25)
# sortieren
feinstaub <- sort(feinstaub)
feinstaub
# arithmetisches Mittel
mean(feinstaub)
# Median
median(feinstaub)
# 0.25, Median, und 0.75 Quantil
quantils <- quantile(feinstaub, probs = c(0.25, 0.5, 0.75), type = 2)
quantils
# unterschiedliche Arten der Berechnung des Quantils
# wir verwenden hier die Methode aus der Vorlesung
# Typ 2: inverse Verteilungsfunktion
# default: Typ 7: linear interpolation

# Histogramm aus Berechnung
library(knitr)
tabler <- data.frame("X" = c("Klasse", "Breite", "Häufigkeit", "Wahrscheinlichkeit", "Höhe"),
                     "1" = c("[17, 19.5]", 2.5, 2, "2/15", "4/75"),
                     "2" = c("(19.5, 22]", 2.5, 7, "7/15", "14/75"),
                     "3" = c("(22, 24.5]", 2.5, 1, "1/15", "2/75"),
                     "4" = c("(24.5, 27]", 2.5, 5, "1/3", "2/15"))
## Breite x Höhe = Wahrscheinlichkeit
knitr::kable(tabler, format = "markdown")

hist(feinstaub, breaks = c(17, 19.5, 22, 24.5, 27), xlim = c(16, 28), xlab = "PM10-Konzentration",
     ylab = "Klassenhöhe", freq = F, ylim = c(0.00, 0.20), 
     right = T, include.lowest = T)

# Histogramm mit 4 Klassen
hist(feinstaub, breaks = 4) # falsch!
# Berechnung immer noch erforderlich
mini <- min(feinstaub)
maxi <- max(feinstaub)
break.points <- seq(mini, maxi, length.out = 5)

hist(feinstaub, breaks = break.points, xlim = c(16, 28), xlab = "PM10-Konzentration",
     ylab = "Klassenhöhe", freq = FALSE, ylim = c(0.00, 0.20), 
     right = T, include.lowest = TRUE)
# freq = F -> relative Häufigkeiten

# Charakterisierung der Verteilung:
# bimodale Verteilung

boxplot(feinstaub, horizontal = T, xlab =  "PM10-Konzentration",
        main = "Boxplot der PM10-Konzentrationen")


## Achtung: Quantile Type von boxplot() in baseR nicht definierbar. (default = 7)!!!
library(qboxplot)
qboxplot(data.frame(feinstaub), qtype = 2, horizontal = T, xlab =  "PM10-Konzentration",
         main = "Boxplot der PM10-Konzentrationen\nmit Quantile Type 2")

# Nachteile:
# Es kann nicht zwischen einer uni- und einer multimodalen Verteilung unterschieden werden.
# Die Häufung der Werte um 20 und 27 ist nicht sichtbar.

# ergänzen Mittelwert, shaped rhombus
points(mean(feinstaub), 1, type = "p", pch = 18, col = "red")

### In welchem Fall ist der obere Whisker in einem einfachen Boxplot nicht vorhanden?
# 0.75-Qunatile = Maximum

### Für welche Datensituation besteht der einfache Boxplot nur aus einem horizontalen Strich?
# alle Quantile sind gleich


# Extra >> Wie kann man den Nachteil vermeiden?

# >>> Der Violin-Plot <<<

library(ggplot2)
ggplot(data.frame(feinstaub, Dichte = ""), aes(x = Dichte, y = feinstaub)) +
  geom_violin(scale = "width", adjust = 0.75, width = 0.5) +
  coord_flip() +
  scale_y_continuous(breaks = seq(17, 27, 1)) +
  theme_bw() +
  labs(x = "PM10-Konzentration", y = "Dichte") +
  geom_boxplot(width = 0.1, fill = "white", color = "black", alpha = 0.5)


# ecdf

# F(x) = 0 für x < 17
# F(x) = 1/15 für 17 <= x < 19
# F(x) = 2/15 für 19 <= x < 20
# F(x) = 6/15 für 20 <= x < 22
# F(x) = 9/15 für 22 <= x < 23
# F(x) = 10/15 für 23 <= x < 25
# F(x) = 11/15 für 25 <= x < 27
# F(x) = 1 für x >= 27

plot(ecdf(feinstaub), main = "Empirische Verteilungsfunktion\nder PM10-Konzentrationen",
     xlab = "PM10-Konzentration")

#Struktur der Verteilung:
  
#  Die Bimodalität der Verteilung zeigt sich durch die großen Sprünge bei x = 20 und x = 27.
