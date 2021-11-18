############################
### FREDAG PRØVE 1, 2018 ###
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
work <- read.csv("https://raw.githubusercontent.com/langoergen/stv1020R/master/data/work.csv", stringsAsFactors = FALSE)
View(work)
  # Jeg åpner datasettet direkte fra raw-filen på nett (GitHub) ved bruk av read.csv()-funksjonen.
  # Jeg bruker View() for å gjøre meg kjent med datasettet.

### OPPGAVE 2 ###
str(work)
  # Vha. str() ser jeg at variablene "occupation" og "type" er i klassen character (chr), 
  # mens "income", "education" og "prestige" er i klassen integer (int).

### OPPGAVE 3 ###
max(work$prestige)
min(work$prestige)
  # Jeg finner den høyeste (97) og laveste (3) verdien vha. max() og min().
which(work$prestige == max(work$prestige))
which(work$prestige == min(work$prestige))
  # Kan enkelt indekseres ved å bruke den logiske testen which() for å hente radnummeret (observasjonen) 
  # som matcher kriteriene (== max()/== min()). Kan også indekseres gradvis vha. eliminasjonsmetoden og 
  # numeriske verdier, f.eks. > 90, > 95 el. < 10, < 5.

### OPPGAVE 4 ###
ggplot(work, aes(x = income, y = education)) +
  geom_point(aes(col = type)) +
  theme_bw() +
  labs(x = "Inntekt", y = "Utdanning", col = "Type", title = "Scatterplot, 'income', 'education' og 'type'")
  # Jeg lager scatterplottet vha. ggplot() og geom_point(). aes()-funksjonen bestemmer mapping, der col =
  # fargefordelingen basert på variabelen "type".

### OPPGAVE 5 ###
cor(work[, 3:5], use = "complete.obs")
  # Det er variabelen "education" som korrelerer sterkest med "prestige". 0.8519156 education-prestige vs. 
  # 0.8378014 income-prestige.

### OPPGAVE 6 ###
work$type2 <- work$type
work$type2 <- ifelse(work$occupation == "policeman", "prof", work$type2)
work$type2 <- ifelse(work$occupation == "cook", "prof", work$type2)
table(work$type2)
  # Jeg omkoder vha ifelse(), og sjekker med table() før og etter for å kontrollere den nye variabelen.
  # Omkodingen ble riktig -> De to observasjonene som var "cook" og "policeman" er nå inkludert i 
  # type = "prof".

### OPPGAVE 7 ###
ggplot(work, aes(x = type, y = prestige)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Type", y = "Prestisje", title = "Boxplot, 'type' og 'prestige'")
  # Vi ser av boxplottet at det er gruppen wc (= White Collar) som har minst variasjon i prestisje.
  # Det er i denne oppgaven spurt etter variasjon i inntekt, men boxplottet skal vise "type" og "prestige",
  # så det best egnede svaret på oppgaven er noe uklart. 

### OPPGAVE 8 ###
reg1 <- lm(prestige ~ income + education + as.factor(type), data = work)
summary(reg1)
  # Estimert effekt av en enhets økning i "income" er lik 0.59755, og tilsier en relativt sterk positiv sammenheng.
  # Dvs. at høyere inntekt tilsier mer prestisjefylt oppfatning av eget yrke.
  # Estimert effekt av en enhets økning i "education" er lik 0.34532, og viser en mindre sterk, men fortsatt 
  # tydelig tilstedeværende, positiv sammenheng. Dvs. at høyere utdanning påvirker egen oppfatning av eget yrke.
  # Dette impliserer at penger er viktigere for prestisjefølelse enn utdanning.
  # R^2 = 0.9131. Justert R^2 = 0.9044. 

### OPPGAVE 9 ###
work$fit.val <- fitted.values(reg1)
work$resid <- resid(reg1)
  # Jeg ekstraherer fitted values og residualer fra regresjonsmodellen vha. fitted.values() og resid().
influencePlot(reg1, scale=10,  
              xlab = "Hat-Values", ylab = "Studentized Residuals", id = TRUE)
  # Jeg ser på innflytelsesrike verdier vha. influencePlot() fra car.
influencePlot(reg1, scale=10,  
              xlab = "Hat-Values", ylab = "Studentized Residuals", id = list(method = "identify"))
work$stud.resid <- rstudent(reg1)
work$hat.val <- hatvalues(reg1)
  # Lager nye variabler med studentiserte verdier og hat-verdier. 
max(work$stud.resid)
  # Finner den høyeste og nest høyeste
