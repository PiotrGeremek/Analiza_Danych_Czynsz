library(ggplot2)
library(hrbrthemes)
library(plotly)
install.packages(plotly)
library(ISLR)
data("Credit")

# Utwórz wykres ratingu (histogram):
ggplot(Credit, aes(x=Rating)) + 
  geom_histogram(binwidth=50, fill="blue", color="black") +
  labs(title="Histogram ratingu", x="Rating", y="Liczba obserwacji") +
  theme_ipsum()
# Wniosek "malutko ludzi z dobrym ratigniem kredytowym"

# Teraz zeby zakolorować srudentow i nie studentow
ggplot(Credit, aes(x=Rating,fill=Student)) + 
  geom_histogram(binwidth=50, color="black") +
  labs(title="Histogram ratingu", x="Rating", y="Liczba obserwacji") +
  theme_ipsum()

# Teraz dla płci
ggplot(Credit, aes(x=Rating,fill=Gender)) + 
  geom_histogram(binwidth=50, color="black") +
  labs(title="Histogram ratingu", x="Rating", y="Liczba obserwacji") +
  theme_ipsum()

# Teraz osobno single i nie single
ggplot(Credit, aes(x=Rating,fill=Gender)) + 
  geom_histogram(binwidth=50, color="black") +
  labs(title="Histogram ratingu", x="Rating", y="Liczba obserwacji") +
  theme_ipsum() +
  facet_grid(~Married)

# Tu jakies motywy cos tam
ggplot(Credit, aes(x=Rating,fill=Gender)) + 
  geom_histogram(binwidth=50, color="black") +
  labs(title="Histogram ratingu", x="Rating", y="Liczba obserwacji") +
  theme_ipsum() +
  theme_modern_rc() +
  facet_grid(~Married)

# Utwórz wykres ratingu (boxplot)
ggplot(Credit, aes(x=Married, y=Rating)) +
  geom_boxplot() +
  labs(title="Boxplot ratingu", x="Stan cywilny", y="Rating") +
  theme_classic()

# Wykresik jest ulepszamy kolorki
ggplot(Credit, aes(x=Married, y=Rating, fill=Gender)) +
  geom_boxplot() +
  labs(title="Boxplot ratingu", x="Stan cywilny", y="Rating") +
  theme_classic()

# Dodajemy facet grid
ggplot(Credit, aes(x=Married, y=Rating, fill=Gender)) +
  geom_boxplot() +
  labs(title="Boxplot ratingu", x="Stan cywilny", y="Rating") +
  theme_classic() +
  facet_grid(~Ethnicity)

# Jeśli chcemy żeby był interaktywny (taki bajer)
# wykres <- ggplot(Credit, aes(x=Married, y=Rating, fill=Gender)) +
#  geom_boxplot() +
#  labs(title="Boxplot ratingu", x="Stan cywilny", y="Rating") +
#  theme_classic() +
#  facet_grid(~Ethnicity)
# ggplotly(wykres)

# Dodajemy wiolinowy wykresik i rpzezroczysty robimy
ggplot(Credit, aes(x=Married, y=Rating, fill=Gender)) +
  geom_violin(alpha=0.2) +
  geom_boxplot() +
  labs(title="Boxplot ratingu", x="Stan cywilny", y="Rating") +
  theme_classic() +
  facet_grid(~Ethnicity)

# Jakis jego boxplot zawiły
ggplot(Credit, aes(x=Married, y=Rating)) +
  geom_violin(aes(alpha=0.2)) +
  geom_boxplot(width=0.5) +
  labs(title="Boxplot ratingu", x="Stan cywilny", y="Rating") +
  theme_classic() +
  facet_grid(Gender~Ethnicity) +
  coord_flip()

# Utwórz wykres ratingu (rozrzutu) w zależności od limitu na karcie:
ggplot(Credit, aes(x=Limit, y=Rating)) +
  geom_point() 

# Teraz zeby nie bylo za prosto to pododajemy rozne bajery
# Podziel zmienną Age na 3 przedziały wiekowe co 20 lat:
Credit$AgeGroup <- cut(Credit$Age, breaks=c(20,40,60,80,100), 
                       labels=c("20-40", "40-60", "60-80", ">80"))
ggplot(Credit, aes(x=Limit, y=Rating)) + 
  geom_point() +
  labs(title="Wykres ratingu w zależności od limitu", x="Limit", y="Rating") +
  theme_classic() +
  facet_grid(~AgeGroup)

#Inne pomysly jego
ggplot(Credit, aes(x=Income, y=Rating, size=Cards)) + 
  geom_point() +
  labs(title="Wykres ratingu w zależności od limitu", x="Limit", y="Rating") +
  theme_classic() +
  facet_grid(~AgeGroup)

ggplot(Credit, aes
       (x=Income, y=Rating, size=Cards)) + 
  geom_point() +
  labs(title="Wykres ratingu w zależności od limitu", x="Limit", y="Rating") +
  theme_classic() +
  facet_grid(Gender~AgeGroup)

# Utwórz wykres dla liczby kart w zależności od grupy wiekowej (dwie zmienne jakościowe)
ggplot(Credit, aes(x=AgeGroup, fill=Cards)) + 
  geom_bar(position="dodge") +
  labs(title="Liczba kart w zależności od grupy wiekowej", x="Grupa wiekowa", y="Liczba kart") +
  theme_classic()

# Podziel rating na przedziałki:
Credit$RatingGroup <- cut(Credit$Rating, breaks=c(0,200,400,600,800,1000), 
                          labels=c("0-200", "200-400", "400-600", "600-800", "800-1000"))

# Utwórz wykres dla liczby kard (Cards) w zależności od grupy wiekowej (AgeGroup):
ggplot(Credit, aes(x=AgeGroup, fill=RatingGroup)) + 
  geom_bar(position="stack") +
  labs(title="Liczba kart w zależności od grupy wiekowej", x="Grupa wiekowa", y="Liczba kart") +
  theme_classic()

ggplot(Credit, aes(x=AgeGroup, fill=RatingGroup)) + 
  geom_bar(position="dodge") +
  labs(title="Liczba kart w zależności od grupy wiekowej", x="Grupa wiekowa", y="Liczba kart") +
  theme_classic()

ggplot(Credit, aes(x=AgeGroup, fill=RatingGroup)) + 
  geom_bar(position="dodge") +
  labs(title="Liczba kart w zależności od grupy wiekowej", x="Grupa wiekowa", y="Liczba kart") +
  theme_classic() +
  facet_grid(Gender~Married)

