##### EUR/SGD Zeitreihenanalyse #####

# Packages installieren, falls noch nicht vorhanden
install.packages("tseries")
install.packages("forecast")
install.packages("ggplot2")

# Packages laden
library(tseries)
library(forecast)
library(ggplot2)

##### Datenvorbereitung ##### 

# Alle Objekte aus dem Arbeitsspeicher löschen 
rm(list = ls())

# Arbeitsverzeichnis setzen 
setwd("/Users/dennispruss/Documents/02_Karriere/01_pruess_xyz/01_Projects/002_EURSGD Zeitreihenanalyse")

# Datensatz einlesen 
sgd <- read.csv(file = "EURSGD_weekly.csv", header = TRUE, sep = ",", dec = ".")

# Irrelevante Spalten entfernen
sgd <- sgd[, -c(2, 3, 4, 6, 7)]

# Überprüfung, ob die Datei fehlende Werte enthält 
anyNA(sgd)

# Daten für die Analyse bereit?
head(sgd)
str(sgd) 

# Datum in geeignetes Format umwandeln 
sgd$Date <- as.Date(sgd$Date, format="%Y-%m-%d")

# Konvertieren der Daten in ein Zeitreihenobjekt
sgd_ts <- ts(sgd$Close, start = c(2004, 1), frequency = 52)

##### Identifikationsphase ##### 

# Plot erstellen 
autoplot(sgd_ts) + 
  ggtitle("EUR/SGD Zeitreihe 2004 - 2023") + 
  xlab("Zeit") + 
  ylab("Wechselkurs")

# Dickey-Fuller-Test anwenden 
adf.test(sgd_ts)

# Differenzierung
sgd_ts_diff <- diff(sgd_ts)

# Dickey-Fuller-Test erneut anwenden 
adf.test(sgd_ts_diff)

# ACF und PACF berechnen 
tsdisplay(sgd_ts_diff)

# Erkennung von Trends
sgd_ts_dec <- decompose(sgd_ts_diff)
plot(sgd_ts_dec, xlab = "Zeit")

# Erstellung Plot Saisonalität
ggseasonplot(sgd_ts_diff) +
  ggtitle("Saisonalität der EUR/SGD Wechselkursdifferenzen") +
  theme(legend.position = "none")

##### Schätzphase ##### 

# ARIMA Modell erstellen  
arima_model <- auto.arima(sgd_ts,
                          d = 1,
                          D = 1, 
                          stepwise = FALSE,
                          approximation = FALSE,
                          trace = TRUE)

# Übersicht der Parameter 
summary(arima_model)

##### Diagnosephase ##### 

# Residuen analysieren
residuals <- resid(arima_model)
tsdisplay(residuals)

# Histogramm der Residuen
hist(residuals, 
     breaks = 30, 
     main = "Histogramm der Residuen",
     xlab = "Residuen",
     ylab = "Häufigkeit",
     col = "lightblue")

# QQ-Plot der Residuen
qqnorm(residuals)

##### Einsatzphase ##### 

# Vorhersagen generieren für 52 Wochen
forecast_sgd <- forecast(arima_model, h = 52) 

# Prognose visualisieren 
autoplot(forecast_sgd) +
  ggtitle("Prognose EUR/SGD-Kurs für das nächste Jahr") +
  ylab("Wechselkurs") +
  xlab("Zeit")

# Prognose basierend auf den letzten 5 Jahren visualisieren
autoplot(forecast_sgd, include = 260) +
  ggtitle("Prognose EUR/SGD-Kurs 2024 (basierend auf den letzten 5 Jahren") +
  ylab("Wechselkurs") +
  xlab("Zeit")
