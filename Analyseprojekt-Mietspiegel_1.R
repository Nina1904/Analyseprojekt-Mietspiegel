rm(list = ls())
ls()
setwd("/Users/nikolinamarjanovic/Desktop")

library(readxl)    
library(dplyr)    
library(ggplot2)    
library(tidyr)    
library(readxl)
library(e1071) 
library(scales)

excel_sheets("Beispiel Mietspiegel - korrigiert.xlsx")

data <- read_excel("Beispiel Mietspiegel - korrigiert.xlsx", sheet = "Daten")

str(data)   
colSums(is.na(data))

sapply(data, class)
# alle Variablen sind nummeric

# 1. Nettomiete (nm)
summary(data$nm)
sd(data$nm, na.rm=TRUE)
skewness(data$nm, na.rm=TRUE)
kurtosis(data$nm, na.rm=TRUE)
qqnorm(data$nm)
qqline(data$nm, col="red")

quantile(data$nm, probs = c(0.25, 0.5, 0.75), na.rm=TRUE)
sort(data$nm, decreasing = TRUE)[1:3] 
sort(data$nm, decreasing = FALSE)[1:3]

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(data$nm)

# Histogram
ggplot(data, aes(x = nm)) +
  geom_histogram(binwidth = 50, fill = "steelblue", color = "white") +
  labs(title = "Verteilung der Nettomiete", x = "Nettomiete (EUR)", y = "Häufigkeit")

# Boxplot
ggplot(data, aes(y = nm)) +
  geom_boxplot(fill = "tomato", color = "black") +
  labs(title = "Boxplot der Nettomiete", y = "Nettomiete (EUR)")

# 2. Wohnfläche 
summary(data$wfl)

sd(data$wfl, na.rm=TRUE)
skewness(data$wfl, na.rm=TRUE)
kurtosis(data$wfl, na.rm=TRUE)
qqnorm(data$wfl)
qqline(data$wfl, col="red")
quantile(data$wfl, probs = c(0.25, 0.5, 0.75), na.rm=TRUE)
sort(data$wfl, decreasing = TRUE)[1:3]  
sort(data$wfl, decreasing = FALSE)[1:3] 

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(data$wfl)


# Histogram
ggplot(data, aes(x = wfl)) +
  geom_histogram(binwidth = 5, fill = "darkseagreen", color = "white") +
  labs(title = "Verteilung der Wohnfläche", x = "Wohnfläche (m²)", y = "Häufigkeit")

# Boxplot
ggplot(data, aes(y = wfl)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplot der Wohnfläche", y = "Wohnfläche (m²)")


# 3. Zimmer


summary(data$rooms)
sum(is.na(data$rooms))

sd(data$rooms, na.rm=TRUE)
skewness(data$rooms, na.rm=TRUE)
kurtosis(data$rooms, na.rm=TRUE)
quantile(data$rooms, probs = c(0.25, 0.5, 0.75), na.rm=TRUE)
sort(data$rooms, decreasing = TRUE)[1:3] 
sort(data$rooms, decreasing = FALSE)[1:3] 

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(data$rooms)



ggplot(data, aes(x = factor(rooms, levels = 1:6))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Anzahl der Zimmer", x = "Anzahl der Zimmer", y = "Anzahl")

ggplot(data, aes(y = rooms)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Anzahl der Zimmer", y = "Anzahl der Zimmer", x = "")



# 4. Baujahr

summary(data$bj)
sum(is.na(data$bj))

sd(data$bj, na.rm=TRUE)
skewness(data$bj, na.rm=TRUE)
kurtosis(data$bj, na.rm=TRUE)
quantile(data$bj, probs = c(0.25, 0.5, 0.75), na.rm=TRUE)
sort(data$bj, decreasing = TRUE)[1:3]  
sort(data$bj, decreasing = FALSE)[1:3]
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(data$bj)


# Histogram
ggplot(data, aes(x = bj)) +
  geom_histogram(binwidth = 5, fill = "darkseagreen", color = "white") +
  labs(title = "Baujahr", x = "Jahr", y = "Häufigkeit")

# Boxplot
ggplot(data, aes(y = bj)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Baujahr", y = "Baujahr",  x = "")


# 5. Wohnlage

data$lage3 <- with(
  data,
  ifelse(wohnbest == 1, "Beste",
         ifelse(wohngut == 1, "Gut", "Normal"))
)


data$lage3 <- factor(data$lage3, levels = c("Normal", "Gut", "Beste"), ordered = TRUE)


print(table(data$lage3))
print(round(prop.table(table(data$lage3)) * 100, 1))

ggplot(data, aes(x = lage3, fill = lage3)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Normal" = "lightgreen",
                               "Gut"    = "yellow",
                               "Beste"  = "skyblue")) +
  labs(title = "Absolute Häufigkeit der Wohnlage",
       x = "Lage",
       y = "Anzahl") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(data, aes(x = lage3, fill = lage3)) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))),
           color = "black") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("Normal" = "lightgreen",
                               "Gut"    = "yellow",
                               "Beste"  = "skyblue")) +
  labs(title = "Relative Häufigkeit der Wohnlage",
       x = "Lage",
       y = "Anteil in Prozent") +
  theme_minimal() +
  theme(legend.position = "none")


# 6.Warmwasserversorgung

data$ww <- ifelse(data$ww0 == 1, 0, 1)
print(table(data$ww))

print(table(factor(data$ww, levels = c(0,1), labels = c("Nein","Ja"))))
print(round(prop.table(table(factor(data$ww, levels = c(0,1), labels = c("Nein","Ja")))) * 100, 1))

sum(is.na(data$ww))


ggplot(data, aes(x = factor(ww, levels = c(0,1), labels = c("Nein","Ja")),
                 fill = factor(ww, levels = c(0,1), labels = c("Nein","Ja")))) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  labs(title = "Absolute Häufigkeit der Warmwasserversorgung",
       x = "",
       y = "Anzahl") +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(data, aes(x = factor(ww, levels = c(0,1), labels = c("Nein","Ja")),
                 fill = factor(ww, levels = c(0,1), labels = c("Nein","Ja")))) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Relative Häufigkeit der Warmwasserversorgung",
       x = "",
       y = "Anteil in Prozent") +
  theme_minimal() +
  theme(legend.position = "none")

# 7. Znetralheitzung 

data$zh <- ifelse(data$zh0 == 1, 0, 1)
print(table(data$zh))

print(table(factor(data$zh, levels = c(0,1), labels = c("Nein","Ja"))))
print(round(prop.table(table(factor(data$zh, levels = c(0,1), labels = c("Nein","Ja")))) * 100, 1))

sum(is.na(data$zh))


ggplot(data, aes(x = factor(zh, levels = c(0,1), labels = c("Nein","Ja")),
                 fill = factor(zh, levels = c(0,1), labels = c("Nein","Ja")))) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  labs(title = "Absolute Häufigkeit der Zentralheizung",
       x = "",
       y = "Anzahl") +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(data, aes(x = factor(zh, levels = c(0,1), labels = c("Nein","Ja")),
                 fill = factor(zh, levels = c(0,1), labels = c("Nein","Ja")))) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Relative Häufigkeit der Zentralheizung",
       x = "",
       y = "Anteil in Prozent") +
  theme_minimal() +
  theme(legend.position = "none")

# 8. Gekacheltes Badezimmer

data$badkach <- ifelse(data$badkach0 == 1, 0, 1)

print(table(factor(data$badkach, levels = c(0,1), labels = c("Nein","Ja"))))
print(round(prop.table(table(factor(data$badkach, levels = c(0,1), labels = c("Nein","Ja")))) * 100, 1))
sum(is.na(data$badkach))


ggplot(data, aes(x = factor(badkach, levels = c(0,1), labels = c("Nein","Ja")),
                 fill = factor(badkach, levels = c(0,1), labels = c("Nein","Ja")))) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  labs(title = "Absolute Häufigkeit des gekachelten Badezimmers",
       x = "",
       y = "Anzahl") +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(data, aes(x = factor(badkach, levels = c(0,1), labels = c("Nein","Ja")),
                 fill = factor(badkach, levels = c(0,1), labels = c("Nein","Ja")))) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Relative Häufigkeit des gekachelten Badezimmers",
       x = "",
       y = "Anteil in Prozent") +
  theme_minimal() +
  theme(legend.position = "none")

# 9. Besondere Zusatzausstattung im Bad

print(table(factor(data$badextra, levels = c(0,1), labels = c("Nein","Ja"))))
print(round(prop.table(table(factor(data$badextra, levels = c(0,1), labels = c("Nein","Ja")))) * 100, 1))

sum(is.na(data$badextra))


ggplot(data, aes(x = factor(badextra, levels = c(0,1), labels = c("Nein","Ja")),
                 fill = factor(badextra, levels = c(0,1), labels = c("Nein","Ja")))) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  labs(title = "Absolute Häufigkeit: Besondere Zusatzausstattung im Bad",
       x = "",
       y = "Anzahl") +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(data, aes(x = factor(badextra, levels = c(0, 1), labels = c("Nein","Ja")),
                 fill = factor(badextra, levels = c(0, 1), labels = c("Nein","Ja")))) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Relative Häufigkeit: Besondere Zusatzausstattung im Bad",
       x = "",
       y = "Anteil in Prozent") +
  theme_minimal() +
  theme(legend.position = "none")



# 10. Gehobene Küche

print(table(factor(data$kueche, levels = c(0,1), labels = c("Nein","Ja"))))
print(round(prop.table(table(factor(data$kueche, levels = c(0,1), labels = c("Nein","Ja")))) * 100, 1))

sum(is.na(data$kueche))

ggplot(data, aes(x = factor(kueche, levels = c(0,1), labels = c("Nein","Ja")),
                 fill = factor(kueche, levels = c(0,1), labels = c("Nein","Ja")))) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  labs(title = "Absolute Häufigkeit die gehobene Küche",
       x = "",
       y = "Anzahl") +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(data, aes(x = factor(kueche, levels = c(0, 1), labels = c("Nein","Ja")),
                 fill = factor(kueche, levels = c(0, 1), labels = c("Nein","Ja")))) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Relative Häufigkeit die gehobene Küche",
       x = "",
       y = "Anteil in Prozent") +
  theme_minimal() +
  theme(legend.position = "none")
