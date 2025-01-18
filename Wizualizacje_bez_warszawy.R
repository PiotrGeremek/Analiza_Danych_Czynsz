dane_do_wiz <- read.csv("oczyszczone_dane.csv")
library(ggplot2)
library(GGally)
library(dplyr)
library(plotly)

## nie zrobiliśmy tego a powinniśmy - zmiana wartości yes i no, na 1 i 0

dane_do_wiz <- dane_do_wiz %>%
  mutate(
    hasParkingSpace = ifelse(hasParkingSpace == "yes", 1, 0),
    hasBalcony = ifelse(hasBalcony == "yes", 1, 0),
    hasElevator = ifelse(hasElevator == "yes", 1, 0),
    hasSecurity = ifelse(hasSecurity == "yes", 1, 0),
    hasStorageRoom = ifelse(hasStorageRoom == "yes", 1, 0)
  )
#####################################

dane_do_wiz_bez_wwa <- dane_do_wiz %>%
  filter(city != "warszawa")

## Powodzenia z przeklikiwaniem

## Histogram naszej najważniejszej zmiennej (Ceny wynajmu) 
ggplot(dane_do_wiz_bez_wwa, aes(x = price)) +
  geom_histogram(binwidth = 500, fill = "blue", color = "black") +
  labs(title = "Histogram Ceny wynajmu mieszkań", x = "Cena wynajmu", y = "Liczba mieszkań")

## Boxploty pokazujące zależność Ceny wynajmu od liczby pokoi (potencjał na usunięcie takich co mają 6 i innych wartości skrajnych) 
ggplot(dane_do_wiz_bez_wwa, aes(x = as.factor(rooms), y = price)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Cena wynajmu a liczba pokoi", x = "Liczba pokoi", y = "Cena wynajmu")
##można tu przy okazji machnąć summary dla tych co mają 6 pokoi (n = 65) kodem - dane_do_wiz_bez_wwa %>% filter(rooms == 6) %>% summary()


## Boxplot Ceny wynajmu (tak dla hecy) [czy ograniczyć dane do 7500? 13000?]
ggplot(dane_do_wiz_bez_wwa, aes(x = "", y = price)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot Ceny wynajmu mieszkań", x = "", y = "Cena wynajmu") +
  coord_flip()

## Scatterplot z powierzchni  + trend liniowy 
ggplot(dane_do_wiz_bez_wwa, aes(x = squareMeters, y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Cena wynajmu w zależności od powierzchni mieszkania (trend liniowy)",
    x = "Powierzchnia mieszkania (m²)",
    y = "Cena wynajmu"
  ) +
  theme_minimal()

## Cena wynajmu od powierzchni plus linia trednu ale alfa = 0,05
ggplot(dane_do_wiz_bez_wwa, aes(x = squareMeters, y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  labs(
    title = "Cena wynajmu w zależności od powierzchni mieszkania (z linią trendu)",
    x = "Powierzchnia mieszkania (m²)",
    y = "Cena wynajmu"
  ) +
  theme_minimal()

## Cena wynajmu a typ budynku
ggplot(dane_do_wiz_bez_wwa, aes(x = type, y = price)) +
  geom_boxplot(fill = "pink") +
  labs(title = "Cena wynajmu a typ budynku", x = "Typ budynku", y = "Cena wynajmu") +
  coord_flip()

## Cena wynajmu w zależności od odległości od centrum 
ggplot(dane_do_wiz_bez_wwa, aes(x = centreDistance, y = price)) +
  geom_point(color = "purple", alpha = 0.3) +
  geom_smooth(method = "loess", color = "blue", se = TRUE) +
  labs(title = "Cena wynajmu a odległość od centrum", x = "Odległość od centrum (km)", y = "Cena wynajmu")

## Cena wynajmu w zależności od szerokości geograficznej (chyba lepiej po prostu dla konkretnych miast zrobić)
ggplot(dane_do_wiz_bez_wwa, aes(x = longitude, y = latitude, color = price)) +
  geom_point(alpha = 0.6) +
  scale_color_gradient(low = "yellow", high = "red") +
  labs(title = "Mapa cen mieszkań", x = "Długość geograficzna", y = "Szerokość geograficzna")

## Jak się rozkłada Cena wynajmu w zależności od typu budynku (ale histogram)
ggplot(dane_do_wiz_bez_wwa, aes(x = price)) +
  geom_histogram(binwidth = 500, fill = "blue", color = "black", alpha = 0.6) +
  facet_wrap(~ type) +
  labs(title = "Rozkład cen według typu budynku", x = "Cena wynajmu", y = "Liczba mieszkań")

## Cena wynajmu w zależności od liczby pięter (jest od razu z jakąś wartością średnią)
ggplot(dane_do_wiz_bez_wwa, aes(x = as.factor(floorCount), y = price)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(title = "Cena wynajmu a liczba pięter w budynku", x = "Liczba pięter", y = "Cena wynajmu")

## Balkonowo
ggplot(dane_do_wiz_bez_wwa, aes(x = as.factor(hasBalcony), y = price)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Cena wynajmu a dostępność balkonu", x = "Czy jest balkon?", y = "Cena wynajmu") +
  coord_flip()

## Parkingowo
ggplot(dane_do_wiz_bez_wwa, aes(x = as.factor(hasParkingSpace), y = price)) +
  geom_boxplot(fill = "cyan") +
  labs(title = "Cena wynajmu a dostępność parkingu", x = "Czy jest parking?", y = "Cena wynajmu") +
  coord_flip()

## Windowo
ggplot(dane_do_wiz_bez_wwa, aes(x = as.factor(hasElevator), y = price)) +
  geom_boxplot(fill = "cyan") +
  labs(title = "Cena wynajmu a dostępność windy", x = "Czy jest winda?", y = "Cena wynajmu") +
  coord_flip()

##Ochronowo
ggplot(dane_do_wiz_bez_wwa, aes(x = as.factor(hasSecurity), y = price)) +
  geom_boxplot(fill = "cyan") +
  labs(title = "Cena wynajmu a istnienie ochrony", x = "Czy jest ochrona?", y = "Cena wynajmu") +
  coord_flip()

##Piwniocowo(?)
ggplot(dane_do_wiz_bez_wwa, aes(x = as.factor(hasStorageRoom), y = price)) +
  geom_boxplot(fill = "cyan") +
  labs(title = "Cena wynajmu a dostępność piwnicy", x = "Czy jest piwnica?", y = "Cena wynajmu") +
  coord_flip()


##  Stan mieszkania vs Cena wynajmu
ggplot(dane_do_wiz_bez_wwa, aes(x = condition, y = price)) +
  geom_boxplot(fill = "salmon") +
  labs(title = "Cena wynajmu a stan mieszkania", x = "Stan mieszkania", y = "Cena wynajmu") + 
  coord_flip()

## Cena wynajmu a materiał budynku
ggplot(dane_do_wiz_bez_wwa, aes(x = buildingMaterial, y = price)) +
  geom_boxplot(fill = "grey") +
  labs(title = "Cena wynajmu a materiał budynku", x = "Materiał budynku", y = "Cena wynajmu") +
  coord_flip()

## Cena wynajmu a odległość od najbliższej szkoły
ggplot(dane_do_wiz_bez_wwa, aes(x = schoolDistance, y = price)) +
  geom_point(color = "green", alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  labs(title = "Cena wynajmu a odległość do szkoły", x = "Odległość do szkoły (km)", y = "Cena wynajmu")

## Cena wynajmu w zależności od miasta w którym się znajdują
ggplot(dane_do_wiz_bez_wwa, aes(x = city, y = price)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "Cena wynajmu mieszkań w zależności od miasta",
    x = "Miasto",
    y = "Cena wynajmu"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Density rozkładu cen mieszkań według typu budynku) (y jest chyba w dziesiętnych)
ggplot(dane_do_wiz_bez_wwa, aes(x = price, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Rozkład cen mieszkań według typu budynku",
    x = "Cena wynajmu",
    y = "Gęstość",
    fill = "Typ budynku"
  ) +
  theme_minimal()

## Nie podoba mi się to w ogóle, postaram się zrobić coś podobnego, ale ze średnią ceną i na liniowym wykresie
ggplot(dane_do_wiz_bez_wwa, aes(x = rooms, y = price, color = condition)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Cena wynajmu a liczba pokoi (kolor wg stanu mieszkania)",
    x = "Liczba pokoi",
    y = "Cena wynajmu",
    color = "Stan mieszkania"
  ) +
  theme_minimal()

## TU ROBIĘ TE RZECZY (i to już dla mnie się podoba)

### Obliczenie średniej Ceny wynajmu dla każdej liczby pokoi
avg_price_by_rooms <- dane_do_wiz_bez_wwa %>%
  group_by(rooms) %>%
  summarise(mean_price = mean(price, na.rm = TRUE))

### Wykres liniowy
ggplot(avg_price_by_rooms, aes(x = rooms, y = mean_price)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  labs(
    title = "Średnia Cena wynajmu mieszkań w zależności od liczby pokoi",
    x = "Liczba pokoi",
    y = "Średnia Cena wynajmu"
  ) +
  theme_minimal()

## Cena wynajmu w zależności od odległości od centrum w podziale na miasta (naajs stufffff) (trochę czytelność tu kwiczy, zmienić kolory?)
ggplot(dane_do_wiz_bez_wwa, aes(x = centreDistance, y = price, color = city)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Cena wynajmu a odległość od centrum w różnych miastach",
    x = "Odległość od centrum (km)",
    y = "Cena wynajmu",
    color = "Miasto"
  ) +
  theme_minimal()

## Boxploty w zależności od posiadania balkonu, parkingu, windy, ochrony i piwnicy
ggplot(dane_do_wiz_bez_wwa, aes(x = interaction(hasBalcony, hasParkingSpace, hasElevator, hasSecurity, hasStorageRoom), y = price)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Cena wynajmu a cechy mieszkania (balkon, parking, winda, ochrona, piwnica)",
    x = "Cecha mieszkania",
    y = "Cena wynajmu"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### tu raczej po prostu widać, że praktycznie żadna kombinacja tych rzeczy jakoś diametralnie nie wpływa na cenę

## Jak w różnych miastach wygląda dostępność parkingów (tu można zrobić więcej tych wykresów z innymi zmiennymi has...)
ggplot(dane_do_wiz_bez_wwa, aes(x = city, fill = as.factor(hasParkingSpace))) +
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

## Tu machnięcie w zależności średniej Ceny wynajmu od liczby POI 
avg_price_poi <- dane_do_wiz_bez_wwa %>%
  group_by(poiCount) %>%
  summarise(mean_price = mean(price, na.rm = TRUE))

ggplot(avg_price_poi, aes(x = poiCount, y = mean_price)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Średnia Cena wynajmu w zależności od liczby punktów POI",
    x = "Liczba punktów POI",
    y = "Średnia Cena wynajmu"
  ) +
  theme_minimal()

## Średnia Cena wynajmu a rok budowy
avg_price_year <- dane_do_wiz_bez_wwa %>%
  group_by(buildYear) %>%
  summarise(mean_price = mean(price, na.rm = TRUE))

ggplot(avg_price_year, aes(x = buildYear, y = mean_price)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "orange", size = 2) +
  labs(
    title = "Średnia Cena wynajmu w zależności od roku budowy",
    x = "Rok budowy",
    y = "Średnia Cena wynajmu"
  ) +
  theme_minimal()

## Jak w ogóle wygląda rozkład POI
ggplot(dane_do_wiz_bez_wwa, aes(x = poiCount)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(
    title = "Rozkład liczby punktów POI",
    x = "Liczba punktów POI",
    y = "Liczba nieruchomości"
  ) +
  theme_minimal()

## coś mi tu nie wyszło but its honest work (możesz to jakoś próbować naprawić)
dane_do_wiz_bez_wwa_POI <- dane_do_wiz_bez_wwa %>%
  mutate(poi_bins = cut(poiCount, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, Inf), labels = c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100", "100+")))

ggplot(dane_do_wiz_bez_wwa_POI, aes(x = poi_bins, y = price)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Cena wynajmu w zależności od liczby punktów POI (przedziały)",
    x = "Liczba punktów POI (przedziały)",
    y = "Cena wynajmu"
  ) +
  theme_minimal()


## Boxploty dla różnych przedziałów metrażu
dane_do_wiz_bez_wwa_m2 <- dane_do_wiz_bez_wwa %>%
  mutate(size_bins = cut(squareMeters, breaks = c(0, 30, 60, 90, 120, Inf),
                         labels = c("0-30", "31-60", "61-90", "91-120", "120+")))

ggplot(dane_do_wiz_bez_wwa_m2, aes(x = size_bins, y = price)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Cena wynajmu w przedziałach powierzchni mieszkania",
    x = "Przedziały powierzchni (m²)",
    y = "Cena wynajmu"
  ) +
  theme_minimal()

## Cena wynajmu od powierzchni plus linia trednu z podziałem na miasta (DOJEBANE JAK NA MOJE)
ggplot(dane_do_wiz_bez_wwa, aes(x = squareMeters, y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  facet_wrap(~city) +
  geom_smooth(method = "lm", color = "red", se = TRUE)
labs(
  title = "Cena wynajmu w zależności od powierzchni mieszkania dla różnych miast",
  x = "Powierzchnia mieszkania (m²)",
  y = "Cena wynajmu"
) +
  theme_minimal()

## Boxploty w Cena wynajmu vs piętro
ggplot(dane_do_wiz_bez_wwa, aes(x = factor(floor), y = price)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Cena wynajmu w zależności od piętra",
    x = "Piętro",
    y = "Cena wynajmu"
  ) +
  theme_minimal()

## Średnia Cena wynajmu na piętro
avg_price_floor <- dane_do_wiz_bez_wwa %>%
  group_by(floor) %>%
  summarise(mean_price = mean(price, na.rm = TRUE))

ggplot(avg_price_floor, aes(x = floor, y = mean_price)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "orange", size = 2) +
  labs(
    title = "Średnia Cena wynajmu w zależności od piętra",
    x = "Piętro",
    y = "Średnia Cena wynajmu"
  ) +
  theme_minimal()

## Całkiem ciekawy boxplot Cena wynajmu vs piętro przez typ nieruchomości
ggplot(dane_do_wiz_bez_wwa, aes(x = factor(floor), y = price, color = type)) +
  geom_boxplot() +
  labs(
    title = "Cena wynajmu w zależności od piętra (z podziałem na typ nieruchomości)",
    x = "Piętro",
    y = "Cena wynajmu",
    color = "Typ nieruchomości"
  ) +
  theme_minimal()

## Rozkład występowania pięter Histogram
ggplot(dane_do_wiz_bez_wwa, aes(x = factor(floor))) +
  geom_histogram(stat = "count", fill = "skyblue", color = "black") +
  labs(
    title = "Liczba ofert mieszkań na piętrze",
    x = "Piętro",
    y = "Liczba mieszkań"
  ) +
  theme_minimal()

## Tu odległość od szkoły a Cena wynajmu + linia trendu
ggplot(dane_do_wiz_bez_wwa, aes(x = schoolDistance, y = price)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(
    title = "Cena wynajmu w zależności od odległości do szkoły",
    x = "Odległość do szkoły",
    y = "Cena wynajmu"
  ) +
  theme_minimal()

## Mało jasne bo średnio widać te większe odległości
ggplot(dane_do_wiz_bez_wwa, aes(x = price, fill = cut(pharmacyDistance, breaks = 5))) +
  geom_histogram(binwidth = 1200, color = "black", alpha = 0.7) +
  labs(
    title = "Rozkład cen w zależności od odległości do apteki",
    x = "Cena wynajmu",
    y = "Liczba mieszkań",
    fill = "Przedziały odległości do apteki"
  ) +
  theme_minimal()

## Cena wynajmu a odgległość od przedszkola (plus linia z 95% przedziałem ufności)
ggplot(dane_do_wiz_bez_wwa, aes(x = kindergartenDistance, y = price)) +
  geom_point(alpha = 0.5, color = "green") +
  geom_smooth(method = "loess", color = "red") +
  labs(
    title = "Cena wynajmu w zależności od odległości do przedszkola",
    x = "Odległość do przedszkola",
    y = "Cena wynajmu"
  ) +
  theme_minimal()

## odległość do college
ggplot(dane_do_wiz_bez_wwa, aes(x = collegeDistance, y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(
    title = "Cena wynajmu w zależności od odległości do college'a",
    x = "Odległość do college'a",
    y = "Cena wynajmu"
  ) +
  theme_minimal()

## Heatmap'a korelacji zmiennych (pogadać o tym jaki jest sens tworzenia niektórych wykresów)
ggcorr(
  data = dane_do_wiz_bez_wwa %>% select(where(is.numeric)),
  method = c("pairwise.complete.obs", "pearson"),
  label = TRUE
) +
  theme(
    axis.text.x = element_text(hjust = 1),  # Przesunięcie etykiet na osi X
    axis.text.y = element_text(hjust = 1)   # Przesunięcie etykiet na osi Y
  )

### ciekawostki 1. nie ma żadnej (cor = 0) korelacji między rokiem budowy i dystansem do centrum, a ceną wynajmu)
### 2. generalnie poza rooms i squreMeters nie ma zmiennych o silnej czy to dodatniej czy to ujemnej korelacji względem ceny
### 3. zmienne kończące się na Distance są mocno ze sobą skorelowane - nie ma co brać ich wszystkich (restaurantDistance najbardziej względem ceny - czy jej może nie użyć jako reprezentatywnej do jakiś wykresów)

## zrobić parę oddzielnych wykresów dla miast bez warszawy + oddzielnie warszawy
## case 6 pokojowych mieszkań
## do każdego wykresu zrobić stat summary ??? 
## co gdyby walnąć factor z zmiennej miasto i tak to wyrzeźbić?

summary(dane_do_wiz_bez_wwa)