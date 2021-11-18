###########################
###   R-PRØVE 2, 2018   ###
###########################

# Working directory:
# setwd("~/OneDrive/Statsvitenskap - UiO/2. semester/STV1020/R-seminar 1/scripts")

# Informasjon om datasettet:
# Datasettet "pinochet.csv" er hentet fra github-mappen for STV1020. "Pinochet" inneholder 2700 obs. og 8 var.

# Relevante pakker:
library(haven)
library(dplyr)
library(ggplot2)
library(ggthemes)

### OPPGAVE 1 ###
pinochet <- read.csv("https://raw.githubusercontent.com/langoergen/stv1020R/master/data/pinochet.csv", stringsAsFactors = FALSE)
View(pinochet)
  # Jeg velger her å bruke read.csv() for å lese RAW-filen fra github i stedet for å lese .Rdata- eller .csv-filen fra wd.
  # Jeg bruker View() for å gjøre meg kjent med datasettet (kan også gjøres fra Global Environment).

### OPPGAVE 2 ###
summary(is.na(pinochet$vote))
  # Vha. den logiske testen is.na() sjekker jeg hvor mange observasjoner som er missing, altså NA.
  # Jeg bruker summary() for å se resultatet med en gang. Svaret er 168.

### OPPGAVE 3 ###
hist(pinochet$income, xlab = "Inntekt", ylab = "Frekvens", main = "Histogram av inntekt")
  # Enkelt histogram laget med Rs innebygde funksjon hist().
ggplot(pinochet, aes(x = income)) +
  geom_histogram(binwidth = 10000) +
  theme_classic() +
  labs(title = "Histogram av inntekt", x = "Inntekt", y = "Frekvens")
  # Histogram laget vha. ggplot()-pakken i stedet. binwidth er satt til 10000 av visuelle hensyn, og jeg
  # har valgt theme_classic()

### OPPGAVE 4 ###
pinochet$decided_vote <- ifelse(pinochet$vote == "Y", 1, NA)
pinochet$decided_vote <- ifelse(pinochet$vote == "N", 0, pinochet$decided_vote)
table(pinochet$decided_vote, pinochet$vote)
  # Omkoding av variabel ved bruk av den logiske testen ifelse(). 

### OPPGAVE 5 ###
cor1 <- cor

### OPPGAVE 6 ###
lreg1 <- lm(statusquo ~ age + education + sex + income, data = pinochet)
summary(lreg1)
  # Forventet effekt av "age" er 0.004492. Jeg ser av summary at 119 observasjoner er fjernet fra regresjonen
  # som følge av "missingness".

### OPPGAVE 7 ###
ggplot(pinochet, aes(x = age, y = statusquo)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Alder", y = "Status Quo", title = "Scatterplot, 'alder' og 'statusquo'")

### OPPGAVE 8 ###
pinochet$income_log <- log(pinochet$income)
  # Oppretter ny variabel ved å gjøre en enkel log-transformasjon, log(), 
  # av den eksisterende variabelen pinochet$income.
lreg2 <- lm(statusquo ~ age + education + sex + income_log, data = pinochet)
summary(lreg2)
  # Den estimerte effekten til alder er her 0.004588, og er bare så vidt større enn effekten vi så i lreg1.

### OPPGAVE 9 ###
