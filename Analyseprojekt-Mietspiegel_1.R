rm(list = ls())
ls()
setwd("/Users/nikolinamarjanovic/Desktop")

library(readxl)    
library(dplyr)    
library(ggplot2)    
library(tidyr)    
library(readxl)
library(e1071) 

excel_sheets("Daten zum Müncher Mietpreisspiegel.xlsx")

data <- read_excel("Daten zum Müncher Mietpreisspiegel.xlsx", sheet = "Daten")

str(data)   
colSums(is.na(data))

sapply(data, class)
data$nm <- as.numeric(data$nm) 

data <- subset(data, select = -kueche)

sapply(data, class)

# 1. Nettomiete (nm)
summary(data$nm)
sum(is.na(data$nm))  
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

summary(data$nm[data$nm <= 0])  

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
sum(is.na(data$wfl))

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

summary(data$wfl[data$wfl <= 0]) 


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

summary(data$rooms[data$rooms <= 0])

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

summary(data$bj[data$bj < 1800 | data$bj > 2025])

# Histogram
ggplot(data, aes(x = bj)) +
  geom_histogram(binwidth = 5, fill = "darkseagreen", color = "white") +
  labs(title = "Baujahr", x = "Jahr", y = "Häufigkeit")

# Boxplot
ggplot(data, aes(y = bj)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Baujahr", y = "Baujahr",  x = "")



# 5. Wohngut

library(ggplot2)
library(scales)


print(table(factor(data$wohngut, levels = c(0,1), labels = c("Nein","Ja"))))
print(round(prop.table(table(factor(data$wohngut, levels = c(0,1), labels = c("Nein","Ja")))) * 100, 1))

sum(is.na(data$wohngut))

ggplot(data, aes(x = factor(wohngut, levels = c(0,1), labels = c("Nein","Ja")),
                 fill = factor(wohngut, levels = c(0,1), labels = c("Nein","Ja")))) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Nein" = "red", "Ja" = "green3")) +
  labs(title = "Wohnungen in guter Wohnlage",
       x = "",
       y = "Anzahl") +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(data, aes(x = factor(wohngut, levels = c(0,1), labels = c("Nein","Ja")),
                 fill = factor(wohngut, levels = c(0,1), labels = c("Nein","Ja")))) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Wohnungen in guter Wohnlage",
       x = "Gute Wohnlage",
       y = "Anteil in Prozent") +
  theme_minimal() +
  theme(legend.position = "none")


# 6.Wohnbest

print(table(factor(data$wohnbest, levels = c(0,1), labels = c("Nein","Ja"))))
print(round(prop.table(table(factor(data$wohnbest, levels = c(0,1), labels = c("Nein","Ja")))) * 100, 1))

sum(is.na(data$wohnbest))

ggplot(data, aes(x = factor(wohnbest, levels = c(0,1), labels = c("Nein","Ja")),
                 fill = factor(wohnbest, levels = c(0,1), labels = c("Nein","Ja")))) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  labs(title = "Wohnungen in bester Wohnlage",
       x = "",
       y = "Anzahl") +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(data, aes(x = factor(wohnbest, levels = c(0,1), labels = c("Nein","Ja")),
                 fill = factor(wohnbest, levels = c(0,1), labels = c("Nein","Ja")))) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Wohnungen in bester Wohnlage",
       x = "Beste Wohnlage",
       y = "Anteil in Prozent") +
  theme_minimal() +
  theme(legend.position = "none")

# 7.Warmwasserversorgung

print(table(factor(data$ww0, levels = c(1,0), labels = c("Nein","Ja"))))
print(round(prop.table(table(factor(data$ww0, levels = c(1,0), labels = c("Nein","Ja")))) * 100, 1))

sum(is.na(data$ww0))

ggplot(data, aes(x = factor(ww0, levels = c(1,0), labels = c("Nein","Ja")),
                 fill = factor(ww0, levels = c(1,0), labels = c("Nein","Ja")))) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  labs(title = "Warmwasserversorgung",
       x = "",
       y = "Anzahl") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(data, aes(x = factor(ww0, levels = c(1, 0), labels = c("Nein","Ja")),
                 fill = factor(ww0, levels = c(1, 0), labels = c("Nein","Ja")))) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Warmwasserversorgung",
       x = "Warmwasserversorgung",
       y = "Anteil in Prozent") +
  theme_minimal() +
  theme(legend.position = "none")

# 8. Znetralheitzung 

print(table(factor(data$zh0, levels = c(1,0), labels = c("Nein","Ja"))))
print(round(prop.table(table(factor(data$zh0, levels = c(1,0), labels = c("Nein","Ja")))) * 100, 1))

sum(is.na(data$zh0))

ggplot(data, aes(x = factor(zh0, levels = c(1,0), labels = c("Nein","Ja")),
                 fill = factor(zh0, levels = c(1,0), labels = c("Nein","Ja")))) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  labs(title = "Znetralheitzung ",
       x = "",
       y = "Anzahl") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(data, aes(x = factor(zh0, levels = c(1, 0), labels = c("Nein","Ja")),
                 fill = factor(zh0, levels = c(1, 0), labels = c("Nein","Ja")))) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Zentralheitzung ",
       x = "Zentralheitzung ",
       y = "Anteil in Prozent") +
  theme_minimal() +
  theme(legend.position = "none")

# 9. Gekacheltes Badezimmer


print(table(factor(data$badkach0, levels = c(1,0), labels = c("Nein","Ja"))))
print(round(prop.table(table(factor(data$badkach0, levels = c(1,0), labels = c("Nein","Ja")))) * 100, 1))

sum(is.na(data$badkach0))

ggplot(data, aes(x = factor(badkach0, levels = c(1,0), labels = c("Nein","Ja")),
                 fill = factor(badkach0, levels = c(1,0), labels = c("Nein","Ja")))) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  labs(title = "Gekacheltes Badezimmer",
       x = "",
       y = "Anzahl") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(data, aes(x = factor(badkach0, levels = c(1, 0), labels = c("Nein","Ja")),
                 fill = factor(badkach0, levels = c(1, 0), labels = c("Nein","Ja")))) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Gekacheltes Badezimmer",
       x = "Gekacheltes Badezimmer",
       y = "Anteil in Prozent") +
  theme_minimal() +
  theme(legend.position = "none")

# 10. Besondere Zusatzausstattung im Bad


print(table(factor(data$badextra, levels = c(0,1), labels = c("Nein","Ja"))))
print(round(prop.table(table(factor(data$badextra, levels = c(0,1), labels = c("Nein","Ja")))) * 100, 1))

sum(is.na(data$badextra))

ggplot(data, aes(x = factor(badextra, levels = c(0,1), labels = c("Nein","Ja")),
                 fill = factor(badextra, levels = c(0,1), labels = c("Nein","Ja")))) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  labs(title = "Gekacheltes Badezimmer",
       x = "",
       y = "Anzahl") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(data, aes(x = factor(badextra, levels = c(0, 1), labels = c("Nein","Ja")),
                 fill = factor(badextra, levels = c(0, 1), labels = c("Nein","Ja")))) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), color = "black") +
  scale_fill_manual(values = c("Nein" = "yellow", "Ja" = "skyblue")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Gekacheltes Badezimmer",
       x = "Besondere Zusatzausstattung im Bad",
       y = "Anteil in Prozent") +
  theme_minimal() +
  theme(legend.position = "none")


