###########################
##    R-PRØVE 1, 2018    ##
###########################

# INFORMASJON:
# Datasettet labour.csv er hentet fra github/langoergen, og består opprinnelig av 753 obs. og 9 var.

# RELEVANTE PAKKER:
library(dplyr)
library(ggplot2)
library(haven)
library(ggthemes)

### Oppgave 1 ###
labour <- read.csv("https://raw.githubusercontent.com/langoergen/stv1020R/master/data/labour.csv", stringsAsFactors = FALSE)
  # Jeg har her valgt å hente datasettet direkte fra nettsiden vha. read.csv i
  # stedet for å hente Rdata.-filen lokalt i wd.
View(labour)
  # Jeg bruker View() for å få en bedre oversikt over datasettet (kan også
  # gjøres fra environment).


### Oppgave 2 ###
summary(labour$lfp) 
  # Jeg ser av summary at 428 av respondentene (gifte kvinner) deltar i
  # "arbeidsstyrken". Dette tallet kunne jeg også ha funnet ved bruk av table(),
  # filter() (fra dplyr) eller logiske tester som ifelse() og which().


### Oppgave 3 ###
labour$lfp.d <- ifelse(labour$lfp == "yes", 1, 0)
  # Jeg omkoder vha. den logiske testen ifelse(). Hvis observasjoner i labour$lfp har verdien "yes" får de verdien 1 i lfp.d.
  # Hvis observasjoner har verdien "no", får de verdien 0 i lfp.d.
table(labour$lfp.d, labour$lfp)
  # Jeg kontrollerer at omkodingen er gjort riktig vha. table()


### Oppgave 4 ###
# a) Opprett et datasett = kvinner som gikk på college med færre enn 3 barn i alderen 6-18 år.
college <- subset(labour, labour$wc == "yes" & labour$k618 < 3)
  # Jeg bruker her subset() for å filtrere ut "yes" på wc = "college attendance"
  # og "< 3" på k618 = "ant. barn 6-18 år".

# b) Opprett et datasett = kvinner som ikke gikk på college med flere enn 3 barn i alderen 6-18 år.
no.college <- subset(labour, labour$wc == "no" & labour$k618 >= 3)
  # Jeg bruker igjen subset(), denne gangen for å hente observasjoner med "no"
  # på wc = "college attendance" og ">= 3" på k618 = "ant. barn 6-18 år"

# Spørsmål: Hva er medianinntekt for kvinnenes familier i de to nye datasettene? 
  # "Inntekt" måles via variabelen "inc" = "family income exclusive of wife's income"

# Medianinntekt "college":
median(college$inc)
  # Svar: 21.47 (kan også hentes vha. summary() eller str(), men obs! avrunding)

# Medianinntekt "no.college":
median(no.college$inc)
  # Svar: 16.29 (kan også hentes vha. summary() eller str(), men obs! avrunding)


### Oppgave 5 ###
cor.test(labour$lwg, labour$age)
  # Svar: 0.012. Konfidensintervall: 95%, signifikansnivå: 5%. Resultatet er ikke signifikant.
  # Eldre kvinner tjener mest.


### Oppgave 6 ###
ggplot(labour, aes(x = hc, y = lwg)) +
  geom_boxplot() +
  labs(x = "Husband's college attendance", y = "Log expected wage rate") + 
  theme_bw()
  # Boxplottet viser at familier der mannen har gått på college har høyere forventet inntekt.


### Oppgave 7 ###
reg1 <- lm(lwg ~ wc + k5 + k618 + age, data = labour)
summary(reg1)
