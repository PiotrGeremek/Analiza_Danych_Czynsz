install.packages("ggstatsplot")
library(ggstatsplot)

# Load the data
dane_do_statystyk<- read.csv("dane_do_statystyk.csv")

library(ggstatsplot)
library(ggplot2)
library(dplyr)
library(viridis)

# Load dataset
df <- dane_do_statystyk  # Assuming the dataset is named `dane_do_statystyk`

# ANOVA: czy cena się różni między miastami? #Between group/condition comparisons
ggbetweenstats(
  data = df,
  x = city,
  y = price,
  type = "parametric",
  title = "czy cena się różni między miastami",
  package = "viridis",
  palette = "plasma"
)

# t-test: czy posiadanie windy wpływa na cenę mieszkania? #Distribution of a numeric variable
gghistostats(
  data = df,
  x = price,
  grouping.var = hasElevator,
  title = "czy posiadanie windy wpływa na cenę mieszkania"
)

# Korelacja: cena vs metraż mieszkania #Correlation between two variables
ggscatterstats(
  data = df,
  x = squareMeters,
  y = price,
  title = "cena vs metraż mieszkania"
)

#Czy materiał, z którego zbudowano budynek, wpływa na cenę mieszkania?

gghistostats(
  data = dane_do_statystyk,
  x = price,
  grouping.var = buildingMaterial,
  title = "Czy materiał, z którego zbudowano budynek, wpływa na cenę mieszkania?"
)

#Wpływ stanu mieszkania na cenę
ggbetweenstats(
  data = dane_do_statystyk,
  x = condition,
  y = price,
  type = "parametric",
  title = "Wpływ stanu mieszkania na cenę"
)