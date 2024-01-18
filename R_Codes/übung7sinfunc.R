# Definieren der Parameter
a <- 2 * pi
c <- 1 / (2 * a)

# Definieren der Funktion f_Xa
f_Xa <- function(x) {
  if (x >= -a && x <= a) {
    return(c * (1 + sin(x)))
  } else {
    return(0)
  }
}

# Erstellen einer Sequenz von Werten im Intervall [-(a + 0.1), a + 0.1]
x_values <- seq(-a - 0.1, a + 0.1, by = 0.01)

# Berechnen der Funktionswerte
y_values <- sapply(x_values, f_Xa)

png("sinfunc2.png", res = 300, width = 6, height = 4, units = 'in')

# Zeichnen der Funktion
plot(x_values, y_values, type = "l", col = "blue", xlab = "x", ylab = "f_Xa(x)",
     main = "Plot of f_Xa(x) for a = 2*pi", xaxt = 'n')

axis(1, at = seq(-2*pi, 2*pi, by = pi), 
     labels = c("-2π", "-π", "0", "π", "2π"))

x_fill_left <- seq(-1.5*pi, 0, by = 0.01)
y_fill_left <- sapply(x_fill_left, f_Xa)
polygon(c(-1.5*pi, x_fill_left, 0), c(0, y_fill_left, 0), col = "lightblue", border = NA)

# Bereich unter der Kurve füllen rechts von x = 0
x_fill_right <- seq(0, 1.5*pi, by = 0.01)
y_fill_right <- sapply(x_fill_right, f_Xa)
polygon(c(0, x_fill_right, 1.5*pi), c(0, y_fill_right, 0), col = "lightgreen", border = NA)

# Vertikale Linien zeichnen
abline(v = -1.5*pi, col = "red", lwd = 2)
abline(v = 1.5*pi, col = "red", lwd = 2)

# Texte hinzufügen
text(-0.75*pi, 0.1, "P(-1.5π < X < 0)", col = "darkblue")
text(0.75*pi, 0.1, "P(0 < X < 1.5π)", col = "darkgreen")


dev.off()
