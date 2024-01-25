######################### Abgabe 2 #########################

#################### Aufgabe 3: Programmieren in R ####################
### Aufgabe 3.1 Simulation: Wie wahrscheinlich ist es, dass beim Wichteln unter 10 Personen mindestens eine Person das eigene Geschenk bekommt?

iterations <- 1000   #Anzahl der Simulationen
n <- 10   #Anzahl der Personen
geschenke <- 1:n   #Anzahl der Geschenke = Anzahl der Personen
counter <- 0   #Zaehler fuer die Faelle, in denen mindestens eine Person ihr eigenes Geschenk erhaelt

for (i in 1:iterations) {
  zuordnung <- sample(geschenke)     #Zufaellige Zuordnung der Geschenke
  if (any(zuordnung == geschenke))   
  {
    counter <- counter + 1
  }
}
wahrscheinlichkeit <- counter / iterations   #berechnet die relative Haeufigkeit der Faelle, in denen mindestens eine Person ihr eigenes Geschenk erhaelt
wahrscheinlichkeit   #gibt die Wahrscheinlichkeit/das Ergebnis an


### Aufgabe 3.2 & 3.3: Von der Simulation zu einer Funktion 

wichtel_unglueck <- function(n, k, iterationen = 1000) {   #n = Anzahl der Personen & k = Anzahl Personen, die ihr eigens mitgebrachtes Geschenk erhalten
  if (!is.numeric(n) | !is.numeric(k) | !is.numeric(iterationen) | n<0 | k<0 | iterationen<0) {   #Nichtnegativit?tsbedingung & nur numerische Variablen werden akzeptiert
    stop("error")
  }                                                         
  geschenke <- 1:n
  counter <- 0                                   #Zaehler der Ungluecksfaelle
  for (i in 1:iterationen) {                     #Durchfuehrung der Simulation
    zuordnung <- sample(geschenke)               #Zufaellige Zuordnung der Geschenke
    if (sum(zuordnung == geschenke) >= k) {      #Ueberprueft, ob mindestens k Personen ihr eigenes Geschenk ziehen
      counter <- counter + 1                     #falls ja, addiere eins drauf
    }
  }
  wahrscheinlichkeit <- counter / iterationen    #berechnet die relative Haeufigkeit der Ungluecksfaelle, indem die Ungluecksfaelle durch die Anzahl an Iterationen geteilt werden
  return(wahrscheinlichkeit)                     #Wiedergabe des Ergebnisses
}

wichtel_unglueck(10, 1)

### Aufgabe 3.4: Testfaelle

library(testthat)

# Testfall 1: Wahrscheinlichkeit = 0, wenn n<k

test_that("Mehr Ungluecksfaelle als Teilnehmer", {
  result <- wichtel_unglueck(4, 5, 10)
  expect_equal(result, 0)
})

# Testfall 2: Fuer n = 2 & k = 1 liegt die Wahrscheinlichkeit zwischen 0.45 und 0.55, wenn die Anzahl an Iterationen gro? genug ist

test_that("Die Wahrscheinlichkeit liegt zwischen 0.45 und 0.55", {
  result <- wichtel_unglueck(2, 1, 10000)
  expect_true(result >= 0.45 & result <= 0.55)
})  

# Testfall 3: Fehlermeldung, wenn eine Variable nicht numerisch ist

test_that("Fehlermeldung, wenn Variablen nicht numerisch", {
  expect_error(wichtel_unglueck("abc", 1, 10))
  expect_error(wichtel_unglueck(10, "abc", 10))
  expect_error(wichtel_unglueck(10, 1, "abc"))
})

# Testfall 4: Fehlermeldung, wenn eine Variable negativ ist

test_that("Fehlermeldung bei negativer Variable", {
  expect_error(wichtel_unglueck(-10, 1, 10))
  expect_error(wichtel_unglueck(10, -1, 10))
  expect_error(wichtel_unglueck(10, 1, -10))
})

### Aufgabe 3.5: Aufbereitung "bikeshare" Datensatz

data.frame <- read.csv(
  file = "Capital_bikeshare_data_2022_with_NAs.csv",
  header = TRUE,
  sep = ",",
  dec = "."
)

# Filtern der Daten nach der Station "Washington & Independence Ave SW/HHS"

filtered.data <- subset(data.frame, station == "Washington & Independence Ave SW/HHS")   
table(filtered.data$station)

# Datensatz auf NAs ueberpruefen

anyNA(filtered.data)
sum(!complete.cases(filtered.data))
filtered.data <- filtered.data[complete.cases(filtered.data), ]
anyNA(filtered.data)
sum(!complete.cases(filtered.data))


# Datensatz auf Datenanomalien ueberpruefen

range(filtered.data$wind_speed)     # -1.0 ist kein plausibler Wert fuer die Windgeschwindigkeit
id <- which(filtered.data$wind_speed == -1.00)
filtered.data <- filtered.data[-id, ]  # entfernt den Wert aus dem Datensatz
range(filtered.data$wind_speed)

range(filtered.data$mean_temperature)

range(filtered.data$max_temperature)

range(filtered.data$min_temperature)

range(filtered.data$precipitation)

range(filtered.data$count)

range(filtered.data$snowfall)

range(filtered.data$snow_depth)

range(filtered.data$date)


#################### Aufgabe 4: Visualisieren in R ####################
### Aufgabe 4.1: 

library(ggplot2)
library(dplyr)
library(ggthemes)

head(data.frame)
summary(data.frame)

head(filtered.data)
summary(filtered.data)

ggplot(filtered.data) +
  geom_point(aes(x = mean_temperature, y = count), col = "purple") +
  xlab("mittlere Temperatur") +
  ylab("Anzahl ausgeliehener Fahrr?der") +
  ggtitle("Zusammenhang zwischen Anzahl ausgeliehener Fahrr?der & der Temperatur") +
  theme_bw()

ggplot(filtered.data) +
  geom_point(aes(x = precipitation, y = count), col = "red") +
  xlab("Niederschlagsmenge") +
  ylab("Anzahl ausgeliehener Fahrr?der") +
  ggtitle("Zusammenhang zwischen Anzahl ausgeliehener Fahrr?der & der Niederschlagsmenge") +
  theme_bw()

ggplot(filtered.data) +
  geom_point(aes(x = wind_speed, y = count), col = "brown") +
  xlab("Windgeschwindigkeit") +
  ylab("Anzahl ausgeliehener Fahrr?der") +
  ggtitle("Zusammenhang zwischen Anzahl ausgeliehener Fahrr?der & der Windgeschwindigkeit") +
  theme_bw()

filtered.data$date <- as.Date(filtered.data$date, format = "%Y-%m-%d")

ggplot(filtered.data) +
  geom_line(aes(x = date, y = count), col = "blue") +
  xlab("Datum") +
  ylab("Anzahl ausgeliehener Fahrr?der") +
  ggtitle("Zusammenhang zwischen Anzahl ausgeliehener Fahrr?der & der Zeit") +
  theme_bw()

### alternativ auch moeglich, aber mehr Aufwand 

ggplot(data = filter(data.frame, station == "Washington & Independence Ave SW/HHS")) +
  geom_point(aes(x = mean_temperature, y = count), col = "purple") +
  xlab("mittlere Temperatur") +
  ylab("Anzahl ausgeliehener Fahrr?der") +
  ggtitle("Zusammenhang zwischen Anzahl ausgeliehener Fahrr?der & der Temperatur") +
  theme_bw()


### Aufgabe 4.2:

ggplot(data = filter(filtered.data, precipitation == 0)) +
  geom_point(aes(x = mean_temperature, y = count), col = "red") +
  xlab("mittlere Temperatur") +
  xlim(0, 80) +
  ylab("Anzahl ausgeliehener Fahrr?der") +
  ylim(0, 200) +
  ggtitle("Zusammenhang zwischen Anzahl ausgeliehener Fahrr?der & der Temperatur (kein Regen)") +
  theme_bw()

ggplot(data = filter(filtered.data, precipitation > 0)) +
  geom_point(aes(x = mean_temperature, y = count), col = "red") +
  xlab("mittlere Temperatur") +
  xlim(0, 80) +
  ylab("Anzahl ausgeliehener Fahrr?der") +
  ylim(0, 200) +
  ggtitle("Zusammenhang zwischen Anzahl ausgeliehener Fahrr?der & der Temperatur (Regen)") +
  theme_bw()


### Aufgabe 4.3:

ggplot(filtered.data) +
  geom_histogram(aes(x = count, y = after_stat(density)), col = "black", fill = "yellow") +
  xlab("Anzahl ausgeliehener Fahrr?der") +
  ggtitle("Verteilung der Anzahl ausgeliehener Fahrraeder") +
  theme_bw()

ggplot(filtered.data) +
  geom_histogram(aes(x = mean_temperature, y = after_stat(density)), col = "black", fill = "yellow") +
  xlab("mittlere Temperatur") +
  ggtitle("Verteilung der mittleren Temperatur") +
  theme_bw()

ggplot(filtered.data) +
  geom_histogram(aes(x = precipitation, y = after_stat(density)), col = "black", fill = "yellow") +
  xlab("Niederschlag") +
  ggtitle("Verteilung der Niederschlagsmenge") +
  theme_bw()

ggplot(filtered.data) +
  geom_histogram(aes(x = wind_speed, y = after_stat(density)), col = "black", fill = "yellow") +
  xlab("Windgeschwindigkeit") +
  ggtitle("Verteilung der Windgeschwindigkeit") +
  theme_bw() 
  

### Aufgabe 4.4:

library(gridExtra)
range(filtered.data$date)

filtered.data$date <- as.Date(filtered.data$date)
filtered.data$Jahreszeit <- NA
filtered.data$Jahreszeit[which(filtered.data$date <= as.Date("2022-06-20") & filtered.data$date >= as.Date("2022-03-20"))] <- "Fruehling"
filtered.data$Jahreszeit[which(filtered.data$date <= as.Date("2022-09-22") & filtered.data$date >= as.Date("2022-06-21"))] <- "Sommer"
filtered.data$Jahreszeit[which(filtered.data$date <= as.Date("2022-11-30") & filtered.data$date >= as.Date("2022-09-23"))] <- "Herbst"
filtered.data$Jahreszeit[which(filtered.data$date <= as.Date("2022-03-19") & filtered.data$date >= as.Date("2022-01-01"))] <- "Winter"


fruehling <- ggplot(filtered.data[filtered.data$Jahreszeit=="Fruehling",]) +
  geom_density(aes(x = count), col = "dark green") +
  xlab("Anzahl ausgeliehener Fahrraeder") +
  xlim(0, 150) +
  ggtitle("Fruehling") +
  theme_bw()

sommer <- ggplot(filtered.data[filtered.data$Jahreszeit=="Sommer",]) +
  geom_density(aes(x = count), col = "blue") +
  xlab("Anzahl ausgeliehener Fahrraeder") +
  xlim(0, 150) +
  ylim(0, 0.020) +
  ggtitle("Sommer") +
  theme_bw()

herbst <- ggplot(filtered.data[filtered.data$Jahreszeit=="Herbst",]) +
  geom_density(aes(x = count), col = "purple") +
  xlab("Anzahl ausgeliehener Fahrraeder") +
  xlim(0, 150) +
  ggtitle("Herbst") +
  theme_bw()

winter <- ggplot(filtered.data[filtered.data$Jahreszeit=="Winter",]) +
  geom_density(aes(x = count), col = "red") +
  xlab("Anzahl ausgeliehener Fahrraeder") +
  xlim(0, 150) +
  ylim(0, 0.020) +
  ggtitle("Winter") +
  theme_bw()

grid.arrange(fruehling, sommer, herbst, winter, nrow = 2, ncol = 2)


### Aufgabe 4.5:

library(plotly)

Meine_Grafik <- plot_ly(data = filtered.data, x = ~mean_temperature, y = ~wind_speed, z = ~count,
                        type = "scatter3d", mode = "markers", marker = list(size = 5, opacity = 0.5), color = ~count,
                        text = ~paste("Datum:", date, "<br>Anzahl ausgeliehener Fahrraeder:", count, "<br>Windgeschwindigkeit:", wind_speed,
                                      "<br>Niederschlag:", precipitation, "<br>Mittlere Temperatur:", mean_temperature),
                        hoverinfo = "text")

Meine_Grafik %>% layout(scene = list(xaxis = list(title = "Mittlere Temperatur"),
                                     yaxis = list(title = "Windgeschwindigkeit"),
                                     zaxis = list(title = "Anzahl ausgeliehener Fahrraeder")))









