# install.packages("readr")
# library(readr)
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("writexl")
#install.packages("scales")
#install.packages("rmarkdown")
library (dplyr)
library(ggplot2)
library(scales)
library(writexl)
library(rmarkdown)

# Import danych z plików CSV

# Standardowy import:
# kraje_1 = read.table("kraje_makro_1.csv", header = TRUE, sep = ",", dec = ".")
# kraje_2 = read.table("kraje_makro_2.csv", header = TRUE, sep = ",", dec = ".")
# nie zadziałał, pliki zawierały dodatkowe znaki które uniemożliwiały poprawny import danych, dlatego skorzystałam z LLM w celu napisania kodu, który naprawi i przygotuje pliki do importu.

txt <- readLines("kraje_makro_1.csv", encoding = "UTF-8")
txt <- gsub("\\\\", "", txt)
txt <- gsub('""', '"', txt)
txt <- sub('^"(.*)"$', '\\1', txt)
kraje_1 <- read.csv(text = txt, sep = ",", header = TRUE, quote = "\"", stringsAsFactors = FALSE)
kraje_1 <- kraje_1 %>% select(-X)
txt2 <- readLines("kraje_makro_2.csv", encoding = "UTF-8")
txt2 <- gsub("\\\\", "", txt2)
txt2 <- gsub('""', '"', txt2)
txt2 <- sub('^"(.*)"$', '\\1', txt2)
kraje_2 <- read.csv(text = txt2, sep = ",", header = TRUE, quote = "\"", stringsAsFactors = FALSE)
kraje_2 <- kraje_2 %>% select(-X)
rm(txt, txt1, txt2, x)

kraje <- left_join(kraje_1, kraje_2, by = c("Kod" = "Country_Code"))
kraje <- left_join(kraje_1, kraje_2, by = c("Panstwo" = "Country_Name"))
kraje <- kraje %>%
  rename(
    Region = World_region,
    Urbanizacja_proc. = Urban.population....of.total.,
    Internet_proc. = Internet.access....of.total.
  )

# 1.Tworzenie nowej zmiennej Populacja_w_mln w dplyr: ----
kraje = kraje %>%
  mutate(Populacja_mln = Populacja / 1e6)

# Równoważny kod w base R:
# kraje$Populacja_mln = kraje$Populacja / 1e6


# 1e6 to zapis miliona w R (1 razy 10 do potęgi 6)
# 1e9  = 1 000 000 000 (miliard)
# 1e12 = 1 000 000 000 000 (bilion)


# 2. Tworzenie nowej zmiennej PKB_per_capita w dplyr: ----
kraje = kraje %>%
  mutate(PKB_per_capita = PKB / Populacja)

# Równoważny kod w base R:
# kraje$PKB_per_capita = kraje$PKB / kraje$Populacja


# filter() – wybieranie wierszy ----
# select() – wybieranie kolumn ----

# 3. Wyświetl kraje, w których % poziom urbanizacji jest większy niż 50----
kraje %>%
  filter(Urbanizacja_proc. > 50)

# Równoważny kod w base R:
# kraje[kraje$Urbanizacja_proc. > 50, ]


# 4. Wyświetl tylko dane pokazujące zmienne Panstwo, Region, PKB, Populacja_mln ----
kraje %>%
  select(Panstwo, Region, PKB, Populacja_mln)

# Równoważny kod w base R:
# kraje[, c("Panstwo", "Region", "PKB", "Populacja_mln")]

# arrange() – sortowanie ----

# 5. Posortuj kraje według przyrostu populacji rosnąco ----
kraje %>%
  arrange(Przyrost_populacji)


# 6.  Posortuj kraje według przyrostu populacji malejąco ----
kraje %>%
  arrange(desc(Przyrost_populacji))

# Równoważny kod w base R:
# kraje[order(kraje$Przyrost_populacji), ]  # rosnąco
# kraje[order(kraje$Przyrost_populacji, decreasing = TRUE), ]  # malejąco


# 7.Wybierz kraje z PKB większym niż 1 bilion, posortuj je rosnąco względem PKB i wyświetl nazwę państwa, PKB i PKB per capita. Ile jest takich krajów? ----
kraje %>%
  filter(PKB > 1e12) %>%
  arrange(PKB) %>%
  select(Panstwo, PKB, PKB_per_capita)


# Równoważny kod w base R:

# Krok 1: Filtrowanie
# kraje_filtr = kraje[kraje$PKB > 1e12, ]

# Krok 2: Sortowanie
# kraje_sort = kraje_filtr[order(kraje_filtr$PKB), ]

# Krok 3: Wybór kolumn
# kraje_sort[, c("Panstwo", "PKB", "PKB_per_capita")]

# Wniosek: dplyr jest bardziej czytelny przy wielu operacjach.



# 8.Wybierz kraje z regionu Afryki Subsaharyjskiej, wybierz zmienne Panstwo, PKB_per_capita, Populacja_mln, Urbanizacja, a następnie posortuj malejąco po PKB per capita ----
kraje %>%
  filter(Region == "Sub-Saharan Africa") %>%
  select(Panstwo, PKB_per_capita, Populacja_mln, Urbanizacja_proc.) %>%
  arrange(desc(PKB_per_capita))


# Równoważny kod w base R:
# Krok 1: Filtrowanie i wybór kolumn
#kraje_reg = kraje[kraje$Region == "Sub-Saharan Africa", c("Panstwo", "PKB_per_capita", "Populacja_mln", "Urbanizacja_proc.")]

# Krok 2: Sortowanie
#kraje_reg[order(kraje_reg$PKB_per_capita, decreasing = TRUE), ]


# group_by() – grupowanie ----
# summarise() - obliczanie wartości zagregowanych (np. średnich, sum) ----

# 9. Wyświetl tylko te kraje, które są bogatsze niż średnia regionu ----
bogate = kraje %>%
  group_by(Region) %>%
  filter(PKB_per_capita > mean(PKB_per_capita, na.rm = TRUE))



# Równoważny kod w base R:

# bogate = kraje[kraje$PKB_per_capita > ave(kraje$PKB_per_capita, kraje$Region, 
                                         # FUN = mean, na.rm = TRUE), ]

# ave() liczy średnią wewnątrz grup i zwraca wektor tej samej długości co dane.



# 10. Znajdź największą wartość PKB per capita w całym zbiorze krajów ----
kraje %>%
  summarise(max_PKB_per_capita = max(PKB_per_capita, na.rm = TRUE))


# Równoważny kod w base R:

# max(kraje$PKB_per_capita, na.rm = TRUE)



# 11.Znajdź największą i najmniejszą wartość Populacji w mln w całym zbiorze krajów ----
kraje %>%
  summarise(
    min_populacja = min(Populacja_mln, na.rm = TRUE),
    max_populacja = max(Populacja_mln, na.rm = TRUE))


# Równoważny kod w base R:
#min(kraje$Populacja_mln, na.rm = TRUE)
#max(kraje$Populacja_mln, na.rm = TRUE)


# 12.Oblicz średnią populację w całym zbiorze krajów (jedna liczba dla całej ramki)----
kraje %>%
  summarise(srednia_populacja = mean(Populacja_mln, na.rm = TRUE))

# Równoważny kod w base R:
#mean(kraje$Populacja_mln, na.rm = TRUE)

# 13. Ile krajów jest w całym zbiorze danych?----
kraje %>%
  summarise(liczba_krajow = n())


# Równoważny kod w base R:

#nrow(kraje)



# 14. Policz, ile krajów jest w każdym regionie ----
kraje %>%
  group_by(Region) %>%
  summarise(liczba_krajow = n())


# Równoważny kod w base R:

#table(kraje$Region)



# 15. Dla każdego regionu świata: oblicz liczbę krajów (n), średni % dostęp do internetu i średni % poziom urbanizacji, a następnie posortuj regiony malejąco wg średniego % dostępu do internetu ----
kraje %>%
  group_by(Region) %>%
  summarise(
    liczba_krajow = n(),
    sredni_internet = mean(Internet_proc., na.rm = TRUE),
    srednia_urbanizacja = mean(Urbanizacja_proc., na.rm = TRUE)
  ) %>%
  arrange(desc(sredni_internet))


# Równoważny kod w base R:
#{
  #wynik = aggregate(cbind(Internet_proc., Urbanizacja_proc.) ~ Region,
                   # kraje, mean, na.rm = TRUE)
  #wynik$liczba_krajow = as.vector(table(kraje$Region)[wynik$Region])
  #colnames(wynik) = c("Region", "sredni_internet", "srednia_urbanizacja", "liczba_krajow")
  #wynik[order(-wynik$sredni_internet), ]
  #}


# UWAGA!
# Wszystkie zaprezentowane działania da się zrobić w base R (czystym R bez pakietów), 
# ale w wielu przykładach użycie funkcji z pakietu dplyr jest bardziej czytelne i szybsze.
# Posługuj się takim kodem, który jest dla Ciebie zrozumiały.

# 1. Prosty wykres punktowy: urbanizacja a PKB per capita ----
ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita)) +
  geom_point() +
  labs(
    title = "Urbanizacja a PKB per capita",
    x = "Urbanizacja (%)",
    y = "PKB per capita")



# 2. Zaawansowany wykres punktowy: urbanizacja a PKB per capita ----
ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita, color = Region)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Urbanizacja a PKB per capita",
    subtitle = "Czy bardziej zurbanizowane kraje są bogatsze?",
    x = "Urbanizacja (% ludności miejskiej)",
    y = "PKB per capita (USD, skala log)",
    color = "Region świata"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom")



# 3. Zaawansowany wykres punktowy: rozmiar gospodarki a populacja ----

ggplot(kraje, aes(x = Populacja_mln, y = PKB, size = PKB_per_capita, color = Region)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Skala gospodarki i demografia",
    x = "Populacja (mln, log10)",
    y = "PKB (USD, log10)",
    size = "PKB per capita"
  ) +
  theme_minimal()



# 4. Prosty wykres słupkowy: liczba krajów w regionach ----
ggplot(kraje, aes(x = Region)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(
    title = "Liczba krajów w regionach świata",
    x = "Region",
    y = "Liczba krajów"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5))


# 5. Zaawansowany wykres słupkowy poziomy: TOP 15 najbogatszych krajów ----
kraje %>%
  arrange(desc(PKB_per_capita)) %>%
  head(15) %>%
  ggplot(aes(x = reorder(Panstwo, PKB_per_capita), y = PKB_per_capita, fill = Region)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "TOP 15 najbogatszych krajów świata (2016)",
    subtitle = "PKB per capita w USD",
    x = NULL,
    y = "PKB per capita (USD)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10))


# 6. Wykres pudełkowy (boxplot): dostęp do internetu według regionów ----
ggplot(kraje, aes(x = reorder(Region, Internet_proc., FUN = median), 
                  y = Internet_proc., fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  coord_flip() +
  labs(
    title = "Dostęp do internetu według regionów świata",
    subtitle = "(punkty to poszczególne kraje)",
    x = NULL,
    y = "Dostęp do internetu (% populacji)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none")
# 7. Wykres pudełkowy (boxplot): przyrost populacji według regionów ----
# (mediana, rozrzut i obserwacje odstające)
ggplot(kraje, aes(x = Region, y = Przyrost_populacji)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_jitter(width = 0.15, alpha = 0.5) +
  coord_flip() +
  labs(
    title = "Tempo przyrostu populacji w regionach świata",
    subtitle = "(punkty to poszczególne kraje, linia przerywana = 0%)",
    x = "Region",
    y = "Przyrost populacji (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14))
# Zapisanie ramki danych do pliku CSV
write.csv(kraje, "kraje_analiza.csv") 
# Zapisanie ramki danych do pliku Excel wymaga pakietu writexl:
#install.packages("writexl")
#library(writexl)

write_xlsx(kraje, "kraje_wynik.xlsx")

# Zapisz wszystkie wykresy – prawe dolne okno, zakładka Plots:
# Export -> Save as image

# Niestety każdy wykres trzeba zapisać ręcznie
# nie ma funkcji do masowego eksportu wykresów.

