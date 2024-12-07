wynajem_pdst <- read.csv("apartments_rent_pl_2024_06.csv")

#Instalowanie wymaganych pakietów do wizualizacji braków
install.packages(c("tidyverse", "dlookr", "editrules", "VIM", "deducorrect", "ISLR","naniar","visdat","Amelia"))

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

