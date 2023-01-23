library(tidyverse)
library(here)
library(naniar)

# Laden der Daten
data_raw <- read_csv(here("src/data/GIS-data.csv"))

# Erstellen von zufaelligen NAs (zur Demonstration) 
set.seed(42)
data_na <- data_raw %>%
  mutate(across(
    .cols = everything(),
    .fns = ~ ifelse(row_number(.x) %in% sample(1:n(), size = (10 * n() / 100)), NA, .x))
    )

# Naniar Funktion
na_summary <- miss_summary(data_na)
print(na_summary)
colnames(na_summary)

# rel. Haeufigkeit: RH
# --------------------------------------------------------------------------- #

# miss_df_prop:       RH NAs im ganzen dataframe
# miss_var_prop:      RH an Variablen, in denen mind. 1 NA vorkommt
# miss_case_prop:     RH an Cases, in denen mind. 1 NA vorkommt
# miss_case_table:    Tabelle Anzahl an Cases mit 1, 2, 3, ..., n NAs
# miss_var_table:     Tabelle mit Anzahl an Variablen mit 1, 2, 3, .., n NAs
# miss_var_summary:   Zusammenfassung der NAs pro Variable (ueber alle Cases)
# miss_case_summary:  Zusammenfassung der NAs pro Case (ueber alle Variablen)

# --------------------------------------------------------------------------- #

# Ausgabe von allen Tabellen der Funktion miss_summary()
fun_names <- colnames(na_summary)
map(fun_names, ~pluck(na_summary, .x, 1))


# - Visualisierung ------------------------------------------------------------ #

# man kann die NAs auch nochmal visualisieren:
# -> das kann dabei unterstützen ob es irgendwelche systematischen NAs gibt (Missing Not At Random)

vis_miss(data_na) # hier sind die NAs unsystematisch 


# die Ausgabe von miss_case_summary koennen wir nutzen, um Cases mit mehr als 20% NAs auszuschließen
case_na_summary <- pluck(na_summary, "miss_case_summary", 1)
print(case_na_summary)
case_drop <- case_na_summary %>%
  filter(pct_miss >= 20) %>%  # NAs >= 20%
  pull(case)

length(case_drop) # Es werden 15 Participants ausgeschlossen


# Auschluss der Participants mit mehr als 20% NAs
data_exluced <- slice(data_na, -case_drop)

