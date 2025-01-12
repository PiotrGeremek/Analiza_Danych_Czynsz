dane_do_wiz <- read.csv("oczyszczone_dane.csv")
library(ggplot2)
library(GGally)
library(dplyr)
library(plotly)

## Powodzenia z przeklikiwaniem

## Histogram naszej najważniejszej zmiennej (ceny)
ggplot(dane_do_wiz, aes(x = price)) +
  geom_histogram(binwidth = 500, fill = "blue", color = "black") +
  labs(title = "Histogram ceny mieszkań", x = "Cena", y = "Liczba mieszkań")

## Limitowanie osi x do 12500
ggplot(dane_do_wiz, aes(x = price)) +
  geom_histogram(binwidth = 500, fill = "blue", color = "black") +
  xlim(0, 12500) +
  labs(title = "Histogram ceny mieszkań", x = "Cena", y = "Liczba mieszkań")

## Boxploty pokazujące zależność ceny od liczby pokoi (potencjał na usunięcie takich co mają 6 i innych wartości skrajnych) 
ggplot(dane_do_wiz, aes(x = as.factor(rooms), y = price)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Cena a liczba pokoi", x = "Liczba pokoi", y = "Cena")

## Cena w zależności od posiadanego balkonu (wydaje sie nijak istotne)
ggplot(dane_do_wiz, aes(x = as.factor(hasBalcony), y = price)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Cena a dostępność balkonu", x = "Czy jest balkon?", y = "Cena")

## Boxplot ceny (tak dla hecy) [czy ograniczyć dane do 7500? 13000?]
ggplot(dane_do_wiz, aes(x = "", y = price)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot ceny mieszkań", x = "", y = "Cena") +
  coord_flip()

## Scatterplot z powierzchni (jakoś gównianie to wygląda, zrobić może jakiś model liniowy MNNK?)
ggplot(dane_do_wiz, aes(x = squareMeters, y = price)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "Cena a powierzchnia mieszkania", x = "Powierzchnia (m2)", y = "Cena")

## Cena a typ budynku
ggplot(dane_do_wiz, aes(x = type, y = price)) +
  geom_boxplot(fill = "pink") +
  labs(title = "Cena a typ budynku", x = "Typ budynku", y = "Cena") +
  coord_flip()

## Cena w zależności (znowu jakoś mało jasne, ale wnioski da się wyciągnąć[niby]) 
ggplot(dane_do_wiz, aes(x = centreDistance, y = price)) +
  geom_point(color = "purple", alpha = 0.6) +
  labs(title = "Cena a odległość od centrum", x = "Odległość od centrum (km)", y = "Cena")

## Cena w zależności od szerokości geograficznej (chyba lepiej po prostu od miasta zrobić)
ggplot(dane_do_wiz, aes(x = longitude, y = latitude, color = price)) +
  geom_point(alpha = 0.6) +
  scale_color_gradient(low = "yellow", high = "red") +
  labs(title = "Mapa cen mieszkań", x = "Długość geograficzna", y = "Szerokość geograficzna")

## Jak się rozkłada cena w zależności od typu budynku
ggplot(dane_do_wiz, aes(x = price)) +
  geom_histogram(binwidth = 500, fill = "blue", color = "black", alpha = 0.6) +
  facet_wrap(~ type) +
  labs(title = "Rozkład cen według typu budynku", x = "Cena", y = "Liczba mieszkań")

## Cena w zależności od liczby pięter (może zrobić tu jakąś średnią per piętro?)
ggplot(dane_do_wiz, aes(x = floorCount, y = price)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  labs(title = "Cena a liczba pięter w budynku", x = "Liczba pięter", y = "Cena")

## Parkingowo
ggplot(dane_do_wiz, aes(x = as.factor(hasParkingSpace), y = price)) +
  geom_boxplot(fill = "cyan") +
  labs(title = "Cena a dostępność parkingu", x = "Czy jest parking?", y = "Cena") +
  coord_flip()

##  Stan mieszkania vs cena
ggplot(dane_do_wiz, aes(x = condition, y = price)) +
  geom_boxplot(fill = "salmon") +
  labs(title = "Cena a stan mieszkania", x = "Stan mieszkania", y = "Cena") + 
  coord_flip()

## Cena a materiał budynku
ggplot(dane_do_wiz, aes(x = buildingMaterial, y = price)) +
  geom_boxplot(fill = "grey") +
  labs(title = "Cena a materiał budynku", x = "Materiał budynku", y = "Cena") +
  coord_flip()

## Cena a odległość od najbliższej szkoły
ggplot(dane_do_wiz, aes(x = schoolDistance, y = price)) +
  geom_point(color = "orange", alpha = 0.6) +
  labs(title = "Cena a odległość do szkoły", x = "Odległość do szkoły (km)", y = "Cena")

## Cena w zależności od miasta w którym się znajdują (pretekst do usunięcia tych ponad 12000)
ggplot(dane_do_wiz, aes(x = city, y = price)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "Cena mieszkań w zależności od miasta",
    x = "Miasto",
    y = "Cena"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Cena a balkon
ggplot(dane_do_wiz, aes(x = as.factor(hasBalcony), y = price)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Cena a dostępność balkonu", x = "Czy jest balkon?", y = "Cena") +
  coord_flip()

## Density rozkładu cen mieszkań według typu budynku)
ggplot(dane_do_wiz, aes(x = price, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Rozkład cen mieszkań według typu budynku",
    x = "Cena",
    y = "Gęstość",
    fill = "Typ budynku"
  ) +
  theme_minimal()

## Nie podoba mi się to w ogóle, postaram się zrobić coś podobnego, ale ze średnią ceną i na liniowym wykresie
ggplot(dane_do_wiz, aes(x = rooms, y = price, color = condition)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Cena a liczba pokoi (kolor wg stanu mieszkania)",
    x = "Liczba pokoi",
    y = "Cena",
    color = "Stan mieszkania"
  ) +
  theme_minimal()

## TU ROBIĘ TE RZECZY (i to już dla mnie się podoba)

### Obliczenie średniej ceny dla każdej liczby pokoi
avg_price_by_rooms <- dane_do_wiz %>%
  group_by(rooms) %>%
  summarise(mean_price = mean(price, na.rm = TRUE))

### Wykres liniowy
ggplot(avg_price_by_rooms, aes(x = rooms, y = mean_price)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  labs(
    title = "Średnia cena mieszkań w zależności od liczby pokoi",
    x = "Liczba pokoi",
    y = "Średnia cena"
  ) +
  theme_minimal()

## Cena w zależności od odległości od centrum w podziale na miasta (naajs stufffff)
ggplot(dane_do_wiz, aes(x = centreDistance, y = price, color = city)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Cena a odległość od centrum w różnych miastach",
    x = "Odległość od centrum (km)",
    y = "Cena",
    color = "Miasto"
  ) +
  theme_minimal()

## Boxploty w zależności od posiadania balkonu, parkingu, windy
ggplot(dane_do_wiz, aes(x = interaction(hasBalcony, hasParkingSpace, hasElevator), y = price)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Cena a cechy mieszkania (balkon, parking, winda)",
    x = "Cecha mieszkania",
    y = "Cena"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Tu akurat pewna propozycja chatuGPT w kontekście korelacji, jak to czytać idk xd
library(GGally)

ggpairs(dane_do_wiz[, c("price", "squareMeters", "rooms", "centreDistance", "schoolDistance")],
        title = "Macierz korelacji zmiennych ilościowych")

## Jak w różnych miastach wygląda dostępność parkingów
ggplot(dane_do_wiz, aes(x = city, fill = hasParkingSpace)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Dostępność parkingu w różnych miastach",
    x = "Miasto",
    y = "Procent mieszkań",
    fill = "Czy jest parking"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Tu machnięcie w zależności średniej ceny od liczby POI 
avg_price_poi <- dane_do_wiz %>%
  group_by(poiCount) %>%
  summarise(mean_price = mean(price, na.rm = TRUE))

ggplot(avg_price_poi, aes(x = poiCount, y = mean_price)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Średnia cena w zależności od liczby punktów POI",
    x = "Liczba punktów POI",
    y = "Średnia cena"
  ) +
  theme_minimal()

## Średnia cena a rok budowy
avg_price_year <- dane_do_wiz %>%
  group_by(buildYear) %>%
  summarise(mean_price = mean(price, na.rm = TRUE))

ggplot(avg_price_year, aes(x = buildYear, y = mean_price)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "orange", size = 2) +
  labs(
    title = "Średnia cena w zależności od roku budowy",
    x = "Rok budowy",
    y = "Średnia cena"
  ) +
  theme_minimal()

## Jak w ogóle wygląda rozkład POI
ggplot(dane_do_wiz, aes(x = poiCount)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(
    title = "Rozkład liczby punktów POI",
    x = "Liczba punktów POI",
    y = "Liczba nieruchomości"
  ) +
  theme_minimal()

## coś mi tu nie wyszło but its honest work
dane_do_wiz_POI <- dane_do_wiz %>%
  mutate(poi_bins = cut(poiCount, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, Inf), labels = c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100", "100+")))

ggplot(dane_do_wiz_POI, aes(x = poi_bins, y = price)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Cena w zależności od liczby punktów POI (przedziały)",
    x = "Liczba punktów POI (przedziały)",
    y = "Cena"
  ) +
  theme_minimal()

## Cena od powierzchni plus linia trednu
ggplot(dane_do_wiz, aes(x = squareMeters, y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Cena w zależności od powierzchni mieszkania (z linią trendu)",
    x = "Powierzchnia mieszkania (m²)",
    y = "Cena"
  ) +
  theme_minimal()

## Boxploty dla różnych przedziałów metrażu
dane_do_wiz_m2 <- dane_do_wiz %>%
  mutate(size_bins = cut(squareMeters, breaks = c(0, 30, 60, 90, 120, Inf),
                         labels = c("0-30", "31-60", "61-90", "91-120", "120+")))

ggplot(dane_do_wiz_m2, aes(x = size_bins, y = price)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Cena w przedziałach powierzchni mieszkania",
    x = "Przedziały powierzchni (m²)",
    y = "Cena"
  ) +
  theme_minimal()

## Cena od powierzchni plus linia trednu z podziałem na miasta
ggplot(dane_do_wiz, aes(x = squareMeters, y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  facet_wrap(~city) +
  geom_smooth(method = "lm", color = "red", se = TRUE)
  labs(
    title = "Cena w zależności od powierzchni mieszkania dla różnych miast",
    x = "Powierzchnia mieszkania (m²)",
    y = "Cena"
  ) +
  theme_minimal()
  
## Boxploty w cena vs piętro
  ggplot(dane_do_wiz, aes(x = factor(floor), y = price)) +
    geom_boxplot(fill = "lightblue") +
    labs(
      title = "Cena w zależności od piętra",
      x = "Piętro",
      y = "Cena"
    ) +
    theme_minimal()
  
## Średnia cena na piętro
  avg_price_floor <- dane_do_wiz %>%
    group_by(floor) %>%
    summarise(mean_price = mean(price, na.rm = TRUE))
  
  ggplot(avg_price_floor, aes(x = floor, y = mean_price)) +
    geom_line(color = "darkgreen", size = 1) +
    geom_point(color = "orange", size = 2) +
    labs(
      title = "Średnia cena w zależności od piętra",
      x = "Piętro",
      y = "Średnia cena"
    ) +
    theme_minimal()
  
## Całkiem ciekawy boxplot cena vs piętro przez typ nieruchomości
  ggplot(dane_do_wiz, aes(x = factor(floor), y = price, color = type)) +
    geom_boxplot() +
    labs(
      title = "Cena w zależności od piętra (z podziałem na typ nieruchomości)",
      x = "Piętro",
      y = "Cena",
      color = "Typ nieruchomości"
    ) +
    theme_minimal()
  
  ## Rozkład występowania pięter Histogram
  ggplot(dane_do_wiz, aes(x = factor(floor))) +
    geom_histogram(stat = "count", fill = "skyblue", color = "black") +
    labs(
      title = "Liczba mieszkań na piętrze",
      x = "Piętro",
      y = "Liczba mieszkań"
    ) +
    theme_minimal()
  
## Tu odległość od szkoły a cena + linia trendu
  ggplot(dane_do_wiz, aes(x = schoolDistance, y = price)) +
    geom_point(alpha = 0.5, color = "purple") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(
      title = "Cena w zależności od odległości do szkoły",
      x = "Odległość do szkoły",
      y = "Cena"
    ) +
    theme_minimal()
  
  ## Mało jasne bo średnio widać te większe odległości
  ggplot(dane_do_wiz, aes(x = price, fill = cut(pharmacyDistance, breaks = 5))) +
    geom_histogram(binwidth = 1200, color = "black", alpha = 0.7) +
    labs(
      title = "Rozkład cen w zależności od odległości do apteki",
      x = "Cena",
      y = "Liczba mieszkań",
      fill = "Przedziały odległości do apteki"
    ) +
    theme_minimal()

  ## Cena a odgległość od przedszkola (plus linia z 95% przedziałem ufności)
  ggplot(dane_do_wiz, aes(x = kindergartenDistance, y = price)) +
    geom_point(alpha = 0.5, color = "green") +
    geom_smooth(method = "loess", color = "red") +
    labs(
      title = "Cena w zależności od odległości do przedszkola",
      x = "Odległość do przedszkola",
      y = "Cena"
    ) +
    theme_minimal()
   
  ## odległość do college
  ggplot(dane_do_wiz, aes(x = collegeDistance, y = price)) +
    geom_point(alpha = 0.5, color = "blue") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(
      title = "Cena w zależności od odległości do college'a",
      x = "Odległość do college'a",
      y = "Cena"
    ) +
    theme_minimal()
  
  ## Heatmap'a korelacji zmiennych (pogadać o tym jaki jest sens tworzenia niektórych wykresów)
  ggcorr(
    data = dane_do_wiz %>% select(where(is.numeric)),
    method = c("pairwise.complete.obs", "pearson"),
    label = TRUE
  ) +
    theme(
      axis.text.x = element_text(hjust = 1),  # Przesunięcie etykiet na osi X
      axis.text.y = element_text(hjust = 1)   # Przesunięcie etykiet na osi Y
    )
  