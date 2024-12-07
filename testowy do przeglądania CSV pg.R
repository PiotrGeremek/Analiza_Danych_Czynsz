wynajem_pdst <- read.csv("apartments_rent_pl_2024_06.csv")

#Instalowanie wymaganych pakietów do wizualizacji braków
install.packages(c("naniar", "visdat", "ggplot2", "mice", "finalfit", "Amelia"))

#Ładowanie instalowanych pakietów
library(naniar)
library(visdat)
library(ggplot2)
library(mice)
library(finalfit)

View(wynajem_pdst)

#Wizualizacja braków

vis_miss(wynajem_pdst)
vis_dat(wynajem_pdst)
missing_pattern(wynajem_pdst)
missmap(wynajem_pdst)
gg_miss_upset(wynajem_pdst)

