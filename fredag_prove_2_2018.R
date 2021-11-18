############################
### FREDAG PRØVE 2, 2018 ###
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
wages_df <- read.csv("https://raw.githubusercontent.com/langoergen/stv1020R/master/data/wages.csv", stringsAsFactors = FALSE)
View(wages_df)
  # Jeg åpner "wages" med View() for å bli kjent med datasettet. Kan også gjøres fra Global Environment.

### OPPGAVE 2 ###
table(wages_df$language)
table(wages_df$language, wages_df$sex)
  # Jeg ser av table() at det er 497 av respondentene som snakker "French". 262 er kvinner, 235 er menn.

### OPPGAVE 3 ###
table(is.na(wages_df$wages))
table(complete.cases(wages_df))
  # Jeg ser av den logiske testen is.na() at 3278 har missing på variabelen "wages".
  # Vha. table(complete.cases()) ser jeg at det totalt er 3438 observasjoner med missing
  # på minst én variabel.

### OPPGAVE 4 ###
hist(wages_df$wages, xlab = "Timelønn", ylab = "Frekvens", main = "Histogram av 'wages'")
  # Enkelt histogram laget med Rs native-funksjon hist().
ggplot(wages_df, aes(x = wages)) +
  geom_histogram(bins = 20) +
  theme_bw() +
  labs(x = "Timelønn", y = "Frekvens", title = "Histogram av 'wages'")
  # Annet histogram laget med ggplot(). bins = 20 for syns skyld.
wages_df$wages_log <- log(wages_df$wages)
  # Ny variabel opprettet v. logtransformasjon av wages$wages.

hist(wages_df$wages_log, xlab = "Timelønn", ylab = "Frekvens", main = "Histogram av 'wages_log'")
  # Enkelt histogram laget med Rs native-funksjon hist().
ggplot(wages_df, aes(x = wages_log)) +
  geom_histogram(bins = 20) +
  theme_bw() +
  labs(x = "Timelønn", y = "Frekvens", title = "Histogram av 'wages_log'")
  # Annet histogram laget med ggplot(). bins = 20 for syns skyld.
  # De to histogrammene av wages_log er mest normalfordelt.

### OPPGAVE 5 ###
wages_mo50 <- subset(wages_df, sex == "Male" & age >= 50)
wages_wu50 <- subset(wages_df, sex == "Female" & age < 50)
  # Oppretter to nye datasett vha. subset() - menn over 50 og kvinner under 50.
median(wages_mo50$wages, na.rm = T)
median(wages_wu50$wages, na.rm = T)
  # Jeg ser av mean() at medianlønnen for menn over 50 er 18.92.
  # For kvinner under 50 er den 12.03. Na.rm = TRUE pga. missing values.

### OPPGAVE 6 ###
ggplot(wages_df, aes(x = age, y = wages)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Alder", y = "Timelønn", title = "Scatterplot, 'age' og 'wages'")
  # Regresjonslinje tegnet inn vha. ggplots geom_smooth(). Method = "linear model" = "lm".

### OPPGAVE 7 ###
cor1 <- cor(wages_df[, 1:3], use = "complete.obs")
cor1
  # Lager korrelasjonsmatrise vha. cor(). use = "complete.obs" for å slette missing values.
  # Matrisen viser bl.a. at det er en negativ sammenheng mellom "age" og "education".
cor.test(wages_df$education, wages_df$age)

### OPPGAVE 8 ###
lreg1 <- lm(wages ~ education + age + sex + as.factor(language), data = wages_df)
summary(lreg1)
  # Regresjonsanalysen, lm(), viser at forventet effekt av økt education på wages er positiv, ca. 0.92. 
  # Økning med 1 år. Resultatet er sterkt statistisk signifikant "***". 

### OPPGAVE 9 ###
lreg2 <- lm(wages ~ age + sex + education + as.factor(language) + age * sex, data = wages_df)
summary(lreg2)
  # 3438 obs. fjernet pga. missingness. 
  # Estimert forskjell på kvinne 20 og mann 20: 
(-1.36213 * 1 + 0.12986 * 1 * 20 + 0.19012 * 20) 
  # Mann 20 = 5.03747
(0.19012 * 20) 
  # Kvinne 20 = 3.8024
(5.03747 - 3.8024) 
  # Differanse = 1.23507
vif(lreg2)
  # Funksjonen vif() fra car-pakken viser at det er "sex" og 
  # samspillsleddet "age:sex" som har høyest multikolinearitet.

### OPPGAVE 10 ###
wages_df$cohort <- ifelse(wages_df$age<30, 1, NA)
wages_df$cohort <- ifelse(wages_df$age<40 & wages_df$age>=30, 2, wages_df$cohort)
wages_df$cohort <- ifelse(wages_df$age<50 & wages_df$age>=40, 3, wages_df$cohort)
wages_df$cohort <- ifelse(wages_df$age<60 & wages_df$age>=50, 4, wages_df$cohort)
wages_df$cohort <- ifelse(wages_df$age>= 60, 5, wages_df$cohort)
table(wages_df$cohort)
  # Koding av ny variabel vha. ifelse().

wages_df %>%
  group_by(sex, cohort) %>%
  summarize(med_wage = median(wages, na.rm = T),
            mean_wage = mean(wages, na.rm = T),
            med_ed = median(education, na.rm = T),
            mean_ed = mean(education, na.rm =T))
  # Aggregering utført vha. dplyr-pakken, group_by(). 

ggplot(wages_df, aes(x = education, y = wages, col = as.factor(sex))) + 
  geom_point() +
  theme_hc() +
  facet_wrap(~cohort) +
  labs(x = "Utdanning", y = "Timelønn", col = "Kjønn", title = "Scatterplot, 'education', 'wages' og 'sex'")
