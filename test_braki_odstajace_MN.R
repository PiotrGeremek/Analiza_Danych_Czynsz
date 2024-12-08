wynajem_pdst <- read.csv("apartments_rent_pl_2024_06.csv")

#Instalowanie wymaganych pakietów do wizualizacji braków
install.packages(c("tidyverse", "dlookr", "editrules", "VIM", "deducorrect", "ISLR","naniar","visdat","Amelia","validate"))

#Ładowanie instalowanych pakietów
library(tidyverse)
library(dlookr)
library(editrules)
library(VIM)
library(deducorrect)
library(ISLR) 
library(naniar)
library(visdat)
library(ggplot2)
library(mice)
library(finalfit)
library(Amelia)



#Ile wierszy jest kompletnych (5778)
sum(complete.cases(wynajem_pdst))
#Ile % wierszy jest kompletnych (ok 65%)
nrow(wynajem_pdst[complete.cases(wynajem_pdst), ])/nrow(wynajem_pdst)*100 

is.special <- function(x){
  if (is.numeric(x)) !is.finite(x) else is.na(x)
}

sapply(wynajem_pdst, is.special)

for (n in colnames(wynajem_pdst)){
  is.na(wynajem_pdst[[n]]) <- is.special(wynajem_pdst[[n]])
}
summary(wynajem_pdst)
#Najwiecej NA w buildYeat (2249) i floor (1053)

#Ile mamy braków danych (3602)
n_miss(wynajem_pdst)

#Ile % komorek to NA (1,45%)
prop_miss(wynajem_pdst)

#Zamieniamy puste wartosći na "NA"
wynajem_pdst[wynajem_pdst == ''] <- NA
print(wynajem_pdst)

#Teraz mamy 15762 brakow danych
n_miss(wynajem_pdst)

#Czyli ok 6,36%
prop_miss(wynajem_pdst)

#Zwraca ile brakow danych dla konkretnych zmeinnych
miss_var_summary(wynajem_pdst)
# Condicion 72% brakow danych - ale mamy tylko "premium" - mozemy zalozyc ze te braki to 
# "not premium" i zmaienic na binarne 0,1 

#Wedlug wieszy - ile obserwacji w kazdym z wierszy jest brakujacych (np. tylko w 1 wierszu jest 8 brakujacyhc obserwacji)
wynajem_pdst %>% 
  miss_case_table()

#Wizualizacje
vis_miss(wynajem_pdst)
vis_miss(wynajem_pdst, sort = TRUE) #braki posortowane
gg_miss_fct(wynajem_pdst, fct = city) #braki w zaleznosci od miasta
gg_miss_upset(wynajem_pdst, 
              nsets = 10) #wspolwystepowanie NA
gg_miss_upset(wynajem_pdst)
vis_dat(wynajem_pdst)
missmap(wynajem_pdst)
md.pattern(wynajem_pdst)


# Obserwacje odstjace - boxploty
boxplot(wynajem_pdst$squareMeters)
boxplot(wynajem_pdst$rooms)
boxplot(wynajem_pdst$floor)
boxplot(wynajem_pdst$floorCount)
boxplot(wynajem_pdst$buildYear)
boxplot(wynajem_pdst$latitude)
boxplot(wynajem_pdst$longitude)
boxplot(wynajem_pdst$centreDistance)
boxplot(wynajem_pdst$poiCount)
boxplot(wynajem_pdst$schoolDistance)
boxplot(wynajem_pdst$clinicDistance)
boxplot(wynajem_pdst$postOfficeDistance)
boxplot(wynajem_pdst$kindergartenDistance)
boxplot(wynajem_pdst$restaurantDistance)
boxplot(wynajem_pdst$collegeDistance)
boxplot(wynajem_pdst$pharmacyDistance)
boxplot(wynajem_pdst$price)

# Sprawdzam jeszcze odstajace dane za pomoca min max
summary(wynajem_pdst)


## MUSIMY ZROBIC IMPUTACJE DANYCH!!! - to jako rozdzial (powinnismy miec znaleznione, narysowane i uzupelnione dane)
## no i po imputacji te reguly i bledy zaminiec na NA
# ewentualnie wtorna imputacja potem


# reguly - to co pokazuje na zajeciach
library(tidyverse)
library(dlookr)
library(editrules) #reguly
library(VIM)
library(validate)
attach(wynajem_pdst)

# jak wiele regul mozemy pisac w pliku tekstoweym i zalaczyc lub pisac tutaj

# reguly: (przed imputacja)
reguly <- editset(c(
  "price>0",
  "pharmacyDistance>0",
  "collegeDistance>0",
  "restaurantDistance>0",
  "kindergartenDistance>0",
  "postOfficeDistance>0",
  "clinicDistance>0",
  "schoolDistance>0",
  "floorCount>=0",
  "centreDistance>0",
  "floor>=0",
  "rooms>0",
  "squareMeters>0",
  "buildYear>0",
  "poiCount>=0",
  "latitude>=0",
  "longitude>=0"
))

# walidacja i wykresik
summary(violatedEdits(reguly,wynajem_pdst))
bledy <- violatedEdits(reguly,wynajem_pdst)
plot(bledy)

#mamy 0 błedów - nic nie musimy zamieniac na NA, nie robimy ponownej imputacji
# jesli by byly bledy
# dane[localizeErrors(reguly, dane$adapt)] <- NA

# Sprawdzamy jeszcze czy np w city nie mamy jakis dziwnych miast, innaczej zapisanych itd
unique(wynajem_pdst$city)


# Zamieniami w condition puste na "non_premium+
# Przed tym mielismy 15762 NA
wynajem_pdst$condition[is.na(wynajem_pdst$condition)] <- "non_premium"
print(wynajem_pdst)

## Teraz jeszcze raz sprawdzamy braki danych
n_miss(wynajem_pdst)
# Teraz mamy tylko 9433

#Czyli ok 3,8% zamiast 6,36%
prop_miss(wynajem_pdst)

miss_var_summary(wynajem_pdst)
# buildingMaterial 39,6% brakow danych, buildYear 25,4%, type 20,7%, floor 11,9% ... 

#Wedlug wieszy - ile obserwacji w kazdym z wierszy jest brakujacych (np. tylko w 1 wierszu jest 8 brakujacyhc obserwacji)
wynajem_pdst %>% 
  miss_case_table()

#Wizualizacje
vis_miss(wynajem_pdst)
vis_miss(wynajem_pdst, sort = TRUE) #braki posortowane
gg_miss_fct(wynajem_pdst, fct = city) #braki w zaleznosci od miasta
gg_miss_upset(wynajem_pdst, 
              nsets = 10) #wspolwystepowanie NA
gg_miss_upset(wynajem_pdst)
vis_dat(wynajem_pdst)
missmap(wynajem_pdst)
md.pattern(wynajem_pdst)

# PRZY ROBIENIU JUZ MARKDOWN - wszystkie biblioteki dodac i wczytac gdzies na starcie!!!!

#impuracja brakow danych
library(VIM)
czyste_dane <- hotdeck(wynajem_pdst)
print(czyste_dane)

# Czy na pewno nie mamy brakow danych - NIE MAMY
n_miss(czyste_dane)
vis_miss(czyste_dane)



# sprawdzic jak tam dane i dalej zmieniac yes no na 1 0, kategorie jakies tez moze na cos
# Ogolnie miec te dane ladne czyszte na kolejen zajecia na wizualizacje!!


