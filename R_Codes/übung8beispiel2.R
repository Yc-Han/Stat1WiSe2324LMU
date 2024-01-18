library(ggplot2)
library(moments)

set.seed(123)
# Beta-Verteilung mit positiver Schiefe
data <- rbeta(1000, 2, 5)

# Berechnen von Erwartungswert, Varianz, Exzess-Kurtosis und Schiefe
erwartungswert <- mean(data)
varianz <- var(data)
exz.kurtosis <- kurtosis(data) - 3
schiefe <- skewness(data)

momente <- data.frame(erwartungswert, varianz, exz.kurtosis, schiefe)
momente

# Visualisierung durch Dichteplot und Boxplot
ggplot() +
  geom_density(aes(x = data)) +
  labs(title = "Linkssteile Verteilung", x = "Werte", y = "")

ggplot() +
  geom_boxplot(aes(x = data)) +
  labs(title = "Linkssteile Verteilung", x = "Werte", y = "")
