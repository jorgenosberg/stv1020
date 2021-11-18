############################
### MANDAG PRØVE 1, 2019 ###
############################

### FORBEREDELSER ###
library(dplyr)
library(ggplot2)
library(ggthemes)
library(car)
library(moments)

### REMOVAL ###
rm(list =ls())

### WORKING DIRECTORY ###
getwd()
# setwd("/Users/jorgenosberg/OneDrive/Statsvitenskap - UiO/2. semester/STV1020/R-seminar 1/scripts")

### OPPGAVE 1 ###
free1 <- read.csv("https://raw.githubusercontent.com/langoergen/stv1020R/master/prover_2019/free1.csv", stringsAsFactors = FALSE)
View(free1)
  # Jeg laster inn datasettet direkte vha. read.csv() og URLen til raw-filen (GitHub). 
  # Jeg bruker View() for å åpne datasettet og bli bedre kjent med det.

### OPPGAVE 2 ###
median(free1$self)
median(free1$Kay)
median(free1$Michael)
median(free1$Bob)
median(free1$Vito)
median(free1$Sonny)
  # Jeg beregner median-verdien for alle variablene hvis verdier er gitt vha. frihetsindeksen.
  # Jeg ser at det kun er free1$Sonny som har en median-verdi tilsvarende 5 = "Not free at all"
table(is.na(free1))
table(complete.cases(free1))
  # Vha. den logiske testen is.na() ser jeg at det totalt er 10 tilfeller av missing data (NA).
  # Jeg bruker complete.cases() for å sjekke nøyaktig antall observasjoner med missing på minst én
  # variabel. Resultatet er 9. Dermed har 1 av observasjonene missing på 2 variabler.

### OPPGAVE 3 ###
ggplot(free1, aes(x = area, y = self)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Område", y = "Egenvurdert frihet", title = "Boxplot, 'area' og 'self'")
  # Boxplottet viser at medianverdien for egenvurdert frihet er lik i Eastasia og Eurasia (= 4).
  # Dette er høyere enn medianverdien i Oceana.

### OPPGAVE 4 ###
free1_Eurasia <- free1 %>%
  filter(area == "Eurasia")

free1_Eastasia <- free1 %>%
  filter(area == "Eastasia")

free1_Oceana <- free1 %>%
  filter(area == "Oceana")
  # Jeg bruker filter()-funksjonen fra pakken dplyr for å velge kun de observasjonene som 
  # oppfyller kriteriet jeg har valgt (f.eks. area == "Eurasia"). Jeg bruker assign (<-)
  # for å samle disse observasjonene i individuelle datasett.

View(free1_Eurasia)
table(free1_Eurasia$area)
View(free1_Eastasia)
table(free1_Eastasia$area)
View(free1_Oceana)
table(free1_Oceana$area)
  # Jeg bruker View() og table() for å sjekke datasettene og variabelen "area". Kodingen
  # gir riktig resultat. Tre unike datasett med observasjoner fra de tre ulike områdene.

mean(free1_Eastasia$self)
mean(free1_Eurasia$self)
mean(free1_Oceana$self)
  # Jeg bruker mean() for å sjekke gjennomsnittsverdien på variabelen "self" = "egenvurdert frihet".
  # Lavere tall = høyere egenvurdert frihet. Oceana har høyest egenvurdert frihet (2.89 vs. 4.00 og 3.66)

### OPPGAVE 5 ###
cor1 <- cor(free1[c(1:3, 5:10)], use = "complete.obs")
  # Jeg oppretter korrelasjonen vha. cor()-funksjonen, og bruker c() for å velge alle variablene
  # unntatt area. Jeg lagrer korrelasjonsmatrisen som et objekt vha. assign (<-).
View(cor1)
  # Jeg ser av matrisen at Pearson's R mellom "self" og "educ" er estimert til -0.12719775. 
  # Dette indikerer en negativ sammenheng mellom de to variablene.
cor.test1 <- cor.test(free1$self, free1$educ)
  # Korrelasjonstesten cor.test() viser det samme estimatet som i matrisen, bare avrundet (-0.1313075).
cor.test1$p.value
  # P-verdien gitt i korrelasjonstesten er 0.005534, og resultatet kan derfor sies å være signifikant. (p < 0.05).

### OPPGAVE 6 ###
ggplot(free1, aes(x = age, y = educ, col = area)) +
  geom_point() +
  facet_wrap(~ self) +
  theme_bw() +
  labs(x = "Alder", y = "Utdanning", col = "Område", title = "Scatterplot, 'age', 'educ' og 'area' etter verdi på 'self'")
  # Jeg lager plottet vha. ggplot2-funksjoner. aes() for mapping, der col = area gir farger etter variabelen "area".
  # facet_wrap() muliggjør oppstykking etter verdien på variabelen "self" = "egenvurdert frihet".

### OPPGAVE 7 ###
free1$educ1 <- ifelse(free1$educ >= 5 & free1$age >= 45, 1, 0)
  # Jeg koder ny variabel vha. den logiske testen ifelse(). Kriteriene settes til >= 5 (educ) og
  # >= 45 (age). Variabelen er dikotom (binær), og de som får TRUE på kriteriene gis verdien 1, mens
  # alle andre gis verdien 0.
table1 <- table(free1$sex, free1$educ1)
View(table1)
  # Jeg lager en enkel tabell vha. table() som viser at det er 14 menn og 11 kvinner som har
  # verdien 1 på den nye variabelen educ1.

### OPPGAVE 8 ###
reg1 <- lm(formula = self ~ age + sex + as.factor(area) + educ, data = free1)
summary(reg1)
  # Den forventede effekten av økning på variabelen "age" er lik -0.007396. Med en p-verdi på
  # 0.03458 kan effekten sies å være statistisk signifikant.

### OPPGAVE 9 ###
free1$sum_fc <- free1$Kay + free1$Michael + free1$Bob + free1$Vito + free1$Sonny / 5 
  # Jeg koder ny variabel som et enkelt regnestykke -> summen av de fem fiktive karakterene delt på fem.
reg2 <- lm(formula = sum_fc ~ age + sex + area + educ, data = free1)
summary(reg2)
  # Jeg gjennomfører ny regresjon, lm(), med den nye variabelen "sum_fc" som avhengig variabel. 
  # I dette tilfellet er den estimerte effekten av en enhets økning i alder lik 0.013118.
  # P-verdien er 0.14597, og resultatet kan ikke sies å være signifikant.

### OPPGAVE 10 ###
free2 <- free1[complete.cases(free1[c("sex", "age", "educ", "area", "self", "Kay",
                                      "Michael", "Bob", "Vito", "Sonny", "educ1", "sum_fc")]),]
  # Jeg oppretter det nye datasettet vha. assign (<-) og den logiske testen complete.cases() brukt på free1.
View(free2)
table(is.na(free2))
  # Jeg sjekker om det nye datasettet er riktig vha. View() og is.na().
reg3 <- lm(formula = self ~ age + I(age^2) + educ + sex + area, data = free2)
summary(reg3)
  # Den forventede effekten av en enhets økning i alder blir 0.0155512 for "age" og -0.0002496
  # for "age" når kvadrert.
free2$resid <- resid(reg3)
  # Jeg henter restleddene vha. resid()-funksjonen. 
ggplot(free2, aes(x = resid)) +
  geom_histogram(bins = 60) +
  theme_bw() +
  labs(x = "Residualer", title = "Restleddenes fordeling")
  # Jeg lager et enkelt histogram av residualenes fordeling vha. ggplot() og geom_histogram().
  # Bins = 60 for syns skyld.
reg3_std_resid <- rstandard(reg3)
qqnorm(reg3_std_resid, plot.it = TRUE, xlab = "Normalverdier", 
       ylab = "Standardiserte residualer", main = "Residualenes fordeling")
qqline(reg3_std_resid)
  # For å bedre visualisere fordelingen av residualene lager jeg også et "normal probability plot" 
  # vha. qqnorm() og qqline(). Dette plottet viser at restleddene kanskje kan sies å være 
  # tilnærmet normalfordelte, men at de likevel er nokså skjeve.

