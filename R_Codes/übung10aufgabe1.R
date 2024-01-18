#### Aufgabe 1 ####
d <- c(3300, 9000, 2700, 6500, 3600, 1500, 1000, 1500,
       1100, 2200, 1900, 2600, 1800, 4700, 3100, 2700)

sort(d)

# mean and quartiles
mean(d)
quantile(d, probs = c(0.25, 0.5, 0.75), type = 2)

# modified boxplot
boxplot(d, horizontal = TRUE, main = "Boxplot of d")

# histogram
hist(d, breaks = 10, main = "Histogram of d")

# ecdf
plot(ecdf(d), main = "ECDF of d")

