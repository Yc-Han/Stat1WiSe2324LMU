
## ----vorbereitung--------------------------------------
library(ggplot2)
library(dplyr)
# Daten einlesen
sleep <- read.csv("nhanes_sleep1718.csv")

# Wertelabels zuweisen
wertelabels <- list(
  snore_freq = c("nie", "selten", "gelegentlich", "häufig"),
  snort_freq = c("nie", "selten", "gelegentlich", "häufig"),
  sleepy_freq_day = c("nie", "selten", "manchmal", "oft", "fast immer")
)

# Gehe jede Variable durch und weise die Wertelabels zu
for (variable in names(wertelabels)) {
  # Beginne bei 0 für die Levels, da die Zählung der Codes bei 0 beginnt
  # Benutze "ordered" für ordinale Variablen
  sleep[[variable]] <- ordered(sleep[[variable]], 
                              levels = 0:(length(wertelabels[[variable]]) - 1), 
                              labels = wertelabels[[variable]])
}

sleep$sex <- factor(sleep$sex, 
                    levels = c(1, 2), 
                    labels = c("männlich", "weiblich"))

head(sleep)



## ----barplot-a---------------------------------------------------
table_sleepy <- prop.table(table(sleep$sleepy_freq_day))
table_sleepy |> round(2) # pipeline operator: |>
barplot(table_sleepy, 
        main="Relative Häufigkeiten von 'sleepy_freq_day'",
        xlab="sleepy_freq_day", ylab="Relative Häufigkeit")


## ----barplot-b---------------------------------------------------
# Stichprobe nach Geschlecht unterteilen und Größe ermitteln
maenner <- subset(sleep, sex == "männlich")
frauen <- subset(sleep, sex == "weiblich")
cat("Anzahl der männlichen Befragten:", nrow(maenner), "\n")
cat("Anzahl der weiblichen Befragten:", nrow(frauen), "\n")
# oder:
table(sleep$sex)

# Säulendiagramm für die relativen Häufigkeiten von sleepy_freq_day für weibliche Befragte
frauen_sleepy <- prop.table(table(frauen$sleepy_freq_day))
# base-R plot
barplot(frauen_sleepy, 
        main="Relative Häufigkeiten von 'sleepy_freq_day' bei Frauen", 
        xlab="sleepy_freq_day", ylab="relative Häufigkeit")

# mit ggplot2: (weight = 1/n damit rel. häufigkeiten geplotttet werden)
ggplot(frauen) + 
  geom_bar(aes(x = sleepy_freq_day, weight = 1/nrow(frauen))) 


## ----optimal-----------------------------------------------------
# Version 1: bedingten Hfgk von Geschlecht|Schnarchen
ggplot(sleep) + 
  # gestapeltes Balkendiagramm der bedingten Hfgk von Geschlecht|Schnarchen
  geom_bar(aes(x = snort_freq, fill = sex), position = "fill") + 
  # Referenzlinie: insgesamter Anteil der Frauen:
  geom_hline(yintercept = mean(sleep$sex == "weiblich"), linetype = 2) + 
  # schöne Labels & captions:
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Häufigkeit von Apnoe", y = "Prozent", fill = "Geschlecht",
       caption = "Schwarze Linie: Gesamtanteil Frauen in Stichprobe.
                  Datenquelle: NHANES 2017/18") +
  theme_minimal()

# Version 2 (besser): bedingten Hfgk von Schnarchen|Geschlecht

ggplot(sleep) + 
  # gestapeltes Balkendiagramm der bedingten Hfgk von Schnarchen|Geschlecht
  geom_bar(aes(fill = snort_freq, x = sex), alpha = .8, position = "fill") + 
  # snort_freq ist ordinal, also *sequentielle* farbskala: 
  scale_fill_viridis_d() + 
  # horizontale balken mit horizontalen textlabels sind besser lesbar:
  coord_flip() +
  # schöne Labels & captions:
  scale_y_continuous(labels = scales::percent) +
  labs(fill = "Häufigkeit von Apnoe", y = "Prozent", x = "Geschlecht",
       caption = "Datenquelle: NHANES 2017/18") +
  theme_minimal() 

