#############################
### TIRSDAG PRØVE 1, 2017 ###
#############################

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
tillit <- read.csv("https://raw.githubusercontent.com/langoergen/stv1020R/master/data/tillit.csv", stringsAsFactors = FALSE)
View(tillit)

### OPPGAVE 2 ###
ggplot(tillit, aes(x = stemt, y = tillit_politikere)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Stemt", y = "Tillit til politikere", title = "Boxplot, 'stemt' - 'tillit_politikere'")
  # Jeg ser av plottet at det er de som ikke hadde stemmerett som har høyest median på variabelen tillit_politikere.

### OPPGAVE 3 ###
tillit$stemt[tillit$stemt == "Hadde ikke stemmerett"] <- NA
table(tillit$stemt)
  # Jeg omkoder vha. assign-funksjonen (<-) og et enkelt kriterium (== "Hadde ikke stemmerett").
  # Jeg kontrollerte variabelen med table() før og etter omkoding, og ser at det ble riktig.

### OPPGAVE 4 ###
table(is.na(tillit$tillit_politikere))
  # Jeg finner antall NA-verdier vha. den logiske testen is.na(). Jeg bruker table() for å se
  # summen med en gang. Svaret er 13 NA-verdier på var. tillit_politikere.

### OPPGAVE 5 ###
ggplot(tillit, aes(x = tillit_politikere)) +
  geom_density() +
  theme_bw() +
  labs(x = "Indeks", y = "Tillit til politikere", title = "Densityplot, fordeling av 'tillit_politikere'")

ggplot(tillit, aes(x = tillit_politikere)) +
  geom_bar() +
  theme_bw() +
  labs(x = "Indeks", y = "Tillit til politikere", title = "Densityplot, fordeling av 'tillit_politikere'")
  # Vha. både et densityplott og en barchart ser jeg at fordelingen på tillit_politikere
  # er mer venstreskjev enn den er normalfordelt.

### OPPGAVE 6 ###
cor(tillit[c(1, 3, 5)], use = "complete.obs")
cor.test(tillit$lykke, tillit$inntekt_husholdning)
  # Jeg tester korrelasjonen mellom lykke og inntekt_husholdning.
  # P-verdien er veldig lav (1.337e-10), så resultatet er signifikant.

### OPPGAVE 7 ###
ggplot(tillit, aes(x = lykke, y = tillit_politikere)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Lykke", y = "Tillit til politikere")
  # Regresjonslinjen estimerer en betraktelig økning på variabelen tillit_politikere ved økning
  # på variabelen lykke. Ikke veldig overraskende - jo lykkeligere man er, jo mer fornøyd er man
  # med politikerne = større tillit til dem.

### OPPGAVE 8 ###
reg1 <- lm(tillit_politikere ~ lykke + as.factor(kjonn) + inntekt_husholdning, data = tillit, na.action = "na.exclude")
summary(reg1)
  # Ja! Den lineære regresjonsmodellen viser at sammenhengen mellom lykke og tillit_politikere er
  # statistisk signifikant, også når en kontrollerer for kjønn og inntekt_husholdning.

### OPPGAVE 9 ###
tillit$resid <- resid(reg1)
tillit$stdresid <- rstandard(reg1)

ggplot(tillit, aes(x = resid)) +
  geom_density() +
  theme_bw() +
  labs(x = "Restledd", y = "Tetthet", title = "Fordeling av restledd")
  # Fordelingen av restleddene er tilnærmet normalfordelt, men noe venstreskjev. Fordi normalfordelte
  # restledd er en av betingelsene for gyldig regresjon, er resultatene fra regresjonen mindre pålitelige.
  
qqnorm(tillit$stdresid, ylab = "Standardiserte restledd", clab = "Normale verdier", 
       main = "Normal probability plot")
qqline(tillit$stdresid)
  # Et normal probability plot produsert vha. qqplot viser enda tydeligere potensielle skjevheter mellom
  # regresjon og faktiske observasjoner.


