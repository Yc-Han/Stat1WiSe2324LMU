simulate_sales <- function(n, lambda, alpha, beta) {
  daily_customers <- rpois(n, lambda = lambda)
  # rgamma is vectorized, so we can hand over a different <shape> for each day:
  daily_sales <- rgamma(n = n, shape = daily_customers * alpha, rate = beta)
  daily_sales
}

set.seed(133)
sales <- simulate_sales(n = 10000, lambda = 120, alpha = 10, beta = 1)
mean(sales < 1000) # Wahrscheinlichkeit, dass der Tagesumsatz unter 1000 liegt












### Bonus ###
pnorm(10000, mean = 120 * 10, sd = sqrt(120 * 10 * 11)) # Zenraler Grenzwertsatz
# graphischer Vergleich
plot(density(sales), main = "Dichte des simulierten Tagesumsatzes")
curve(dnorm(x, m = 1200, sd = sqrt(13200)), col = 2, lty = 2, add = TRUE)