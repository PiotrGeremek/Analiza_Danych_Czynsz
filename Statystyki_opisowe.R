install.packages(c("gtsummary", "arsenal", "qwraps2", "summarytools", "table1", "glue","dplyr", "moments"))
library(gtsummary)
library(glue)
library(arsenal)
library(qwraps2)
library(summarytools)
library(table1)
library(dplyr)
library(moments)
library(tidyverse)
### Statystyki opisowe

dane_do_wiz <- read.csv("oczyszczone_dane.csv")

dane_do_statystyk <- dane_do_wiz %>%
  mutate(  
    hasParkingSpace = ifelse(hasParkingSpace == "yes", TRUE, FALSE),  
    hasBalcony = ifelse(hasBalcony == "yes", TRUE, FALSE),  
    hasElevator = ifelse(hasElevator == "yes", TRUE, FALSE),  
    hasSecurity = ifelse(hasSecurity == "yes", TRUE, FALSE),
    hasStorageRoom = ifelse(hasStorageRoom == "yes", TRUE, FALSE)
  ) %>%
  select(-contains("_imp"))

dane_do_statystyk$id <- as.factor(dane_do_statystyk$id)

categorical_vars <- c("city", "type", "ownership", "buildingMaterial", "condition")

dane_do_statystyk[categorical_vars] <- lapply(dane_do_statystyk[categorical_vars], as.factor)

numeric_vars <- c("squareMeters", "rooms", "floor", "floorCount", "buildYear", 
                  "latitude", "longitude", "centreDistance", "poiCount", 
                  "schoolDistance", "clinicDistance", "postOfficeDistance", 
                  "kindergartenDistance", "restaurantDistance", "collegeDistance", 
                  "pharmacyDistance", "price")

dane_do_statystyk[numeric_vars] <- lapply(dane_do_statystyk[numeric_vars], as.numeric)


#### W sumie zastanawiam się czy wcześniej nie powinniśmy porobić tych zmian, na pewno można je opisać we fragmencie dot. data wranglingu (gdzieś przeczytałem, że id jako factor ma sens, ale ostatecznie w tym przypadku chyba średnio)

# Podstawowe statystyki opisowe
summary(dane_do_statystyk)
summary(dane_do_statystyk$city) ## tu doadaje bo w normalnym summary gowno widac

# Tabela podsumowująca z pakietu gtsummary
dane_do_statystyk %>% 
  tbl_summary(include = -id) ### jakbym zostawił id to bym miał w tabeli, ale nie chce mieć
### tu pytanie czy nie usunąć jakiś innych rzeczy, możemy to przegadać

# Tabela podsumowująca z pakietu arsenal
summary_table <- tableby(city ~ ., data = dane_do_statystyk %>% select(-id))
summary(summary_table)
### to jest zajebiście nieczytelne bo w konsoli ale moze sie przyda 
summary(summary_table, format = "markdown")  # dla Markdowna, podono lepiej wypada
## 2/10 ten arsenal
# Podsumowanie z qwraps2
our_summary <- list(
  "Liczba pokoi" = list("Średnia" = ~ mean(rooms, na.rm = TRUE),
                        "Mediana" = ~ median(rooms, na.rm = TRUE),
                        "Min" = ~ min(rooms, na.rm = TRUE),
                        "Max" = ~ max(rooms, na.rm = TRUE)),
  "Cena" = list("Średnia" = ~ mean(price, na.rm = TRUE),
                "Mediana" = ~ median(price, na.rm = TRUE),
                "Min" = ~ min(price, na.rm = TRUE),
                "Max" = ~ max(price, na.rm = TRUE))
)
summary_table <- summary_table(dane_do_statystyk, our_summary)
print(summary_table)
### tu nic wybornego, sa lepsze wybory jak na moje oko
# Tabela podsumowująca z summarytools
dfSummary(dane_do_statystyk %>% select(-id), max.distinct.values = 15) ### to jest całkiem spoko, dodalem to 15 zeby byly widoczne wszystkie miasta

# Tabela opisowa z table1
table1(~ rooms + price + squareMeters | city, data = dane_do_statystyk)
### cooler 
print(table1(~ rooms + price + squareMeters | city, data = dane_do_statystyk), method = "browser")




# Robię funkcje żeby zmieniać statystyki względem miast w zależności od róznych zmiennych (ogólnie przy 1-6 jak przy pokojach to troche czytelnosc kuleje ale podoba mi sie ze od razu robi te dane dla wszystkich miast, jak dla mnie giga rzecz, polecam, jak cos chcesz to po prostu zmieniasz 2 argument funkcji kalkulacje_stat)


kalkulacje_stat <- function(data, group_var) {
  data %>%
    group_by(city, {{ group_var }} ) %>%
    summarise(
      n = n(),
      mean = mean(price, na.rm = TRUE),
      median = median(price, na.rm = TRUE),
      sd = sd(price, na.rm = TRUE),
      var = var(price, na.rm = TRUE),
      coeff_var = (sd / mean) * 100,
      skewness = skewness(price, na.rm = TRUE),
      kurtosis = kurtosis(price, na.rm = TRUE),
      IQR = IQR(price, na.rm = TRUE),
      sx = IQR / 2,
      coeff_varx = (sx / median) * 100,
      min = min(price, na.rm = TRUE),
      max = max(price, na.rm = TRUE),
      q10 = quantile(price, 0.1, na.rm = TRUE),
      q25 = quantile(price, 0.25, na.rm = TRUE),
      q50 = quantile(price, 0.5, na.rm = TRUE),
      q75 = quantile(price, 0.75, na.rm = TRUE),
      q95 = quantile(price, 0.95, na.rm = TRUE)
    ) %>%
    rename(group_value = {{ group_var }} )
}


print(kalkulacje_stat(dane_do_statystyk, type), n = 75)