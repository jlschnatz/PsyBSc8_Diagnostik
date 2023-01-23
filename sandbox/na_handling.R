library(tidyverse)
library(here)

data_raw <- read_csv(here("src/data/GIS-data.csv"))

view(data_raw)


#### Erstellen des Analyse Datensatzes mit einem Pointer ----

library(pointr)

#Erstellen von Pointer zu Subset mit Items
ptr("data_gis_item", "data_raw[7:27]")

colnames(data_gis_item) # Ausgabe der Spalten des Datensatzes


###Analyse von fehlenden Werten


##Achtung, falls es andere fehlende Werte als NA gibt, folgenden Schritt gehen.
##Benutzerdefiniert fehlende Werte (hier beispielsweise -9) werden umkodiert in systemdefiniert fehlende Werte (NA) f?r die Items
#data_raw[data_raw==-9] <- NA

#fehlende Werte in Items?
anyNA(data_gis_item)

# Anzahl der fehlenden Werte in Items
sum(is.na(data_gis_item)) 

#Anzahl absolut fehlender Werte je Fall in Items
rowSums(is.na(data_gis_item))

rows_unfiltered <- nrow(data_raw)

### f?r den Fall, dass fehlende Werte existieren
##Sinnvoll ist ein Ausschluss von F?llen mit mehr als 20% fehlender Werte in den Items des Fragebogens
##Ausschluss von F?llen, die (hier als Beispiel im Code) mehr als 2 fehlende Werte in Items haben. 
data_raw <- data_raw[rowSums(is.na(data_gis_item)) <= 2,]

#Wie viele F?lle wurden ausgeschlossen?
rows_unfiltered - nrow(data_raw)

#fehlende Werte in Items?
anyNA(data_gis_item)

# Anzahl der fehlenden Werte in Items
sum(is.na(data_gis_item)) 


#### Deskriptive Analysen ----

library(psych)

describe(data_gis_item) ## Items aus dem Datensatz werden inspiziert

describe(data_gis_raw$Age) ## demographische Variablen aus dem Gesamtdatensatz werden inspiziert

library(janitor)

tabyl(data_gis_raw$sex)

tabyl(data_gis_raw$sex, show_na = TRUE)

#### APA Tabellen ---- Hier k?nnen die Ergebnisse direkt in APA formatierten Tabellen abgespeichert werden.

library(sjPlot)

##Beispiel f?r kontinuierliche soziodemografische Variablen

descr_age <- describe(data_gis_raw$Age)
tab_df(x = descr_age)

tab_df(x = descr_age,
       file = "table_descr_age.doc")

##Beispiel f?r kategoriale soziodemografische Variablen

descr_sex <- tabyl(data_gis_raw$sex)
tab_df(descr_sex)

##Kontinuierliche Variablen nach kategorialen aufteilen

descr_age_by_sex <- describeBy(x = data_gis_raw$Age,
                               group = data_gis_raw$sex) 
print(descr_age_by_sex)

tab_dfs(
  x = descr_age_by_sex,
  titles = c("Weiblich","Maennlich"))


tab_dfs(
  x = list(descr_age, descr_sex), 
  titles = c("Descriptives of Age", "Descriptives of Sex"))

tab_dfs(
  x = list(descr_age, descr_sex), 
  titles = c("Descriptives of Age","Descriptives of Sex"),
  file = "descriptives_all.doc" # wieder als .doc abspeichern
)

#### Itemanalyse ----

library(sjmisc)
inverse_items <- c("GIS9","GIS16","GIS17","GIS18") 

data_gis_rec <- data_gis_item %>% 
  mutate(across(
    .cols = inverse_items, 
    .fns = ~rec(x = .x, rec = "0=4; 1=3; 2=2; 3=1; 4=0"),
    .names = "{col}_r")
  ) %>% 
  select(-inverse_items) 

colnames(data_gis_rec)

describe(data_gis_rec)

### Alternative Rekodierung

data_gis_rec<-rec(data_gis_item,data_gis_item[inverse_items],rec = "0=4; 1=3; 2=2; 3=1; 4=0")

colnames(data_gis_rec)

data_gis_rec<-rec(data_gis_item,data_gis_item[inverse_items],rec = "rev")

data_gis_rec<-select(data_gis_rec,!inverse_items)


#### Erste Itemanalyse ----

sjt.itemanalysis(
  df = data_gis_rec,
  factor.groups.titles = "Erste Itemanalyse"
)

##Zur Kontrolle der negativen Trennsch?rfen
sjt.itemanalysis(
  df = data_gis_item,
  factor.groups.titles = "Erste Itemanalyse"
)

# Soertierung der Variablen

col_order <- c(
  "GIS1","GIS2","GIS3","GIS4","GIS5","GIS6",
  "GIS7","GIS8","GIS9_r","GIS10", "GIS11",
  "GIS12","GIS13","GIS14","GIS15","GIS16_r",
  "GIS17_r","GIS18_r", "GIS19","GIS20","GIS21"
)

data_gis_rec2 <- select(data_gis_rec, all_of(col_order))


sjt.itemanalysis(
  df = data_gis_rec2,
  factor.groups.titles = "Desktiptive Ergebnisse der Itemanalyse (mit angepasster Reihenfolge)")

#### Ausschliessen der Items ----

drop_discrm <- c("GIS9_r", "GIS16_r","GIS17_r", "GIS18_r")

data_gis_final <- select(data_gis_rec2, -all_of(drop_discrm))

#### Itemanalyse Final ----

sjt.itemanalysis(
  df = data_gis_final,
  factor.groups.titles = "Finale Itemanalyse"
)

#### Reliabilit??t ----

omega_items <- omega(data_gis_final,
                     plot = FALSE)

omega_items$omega.tot

omega_items$alpha

#### Itemvarianz ----

item_var<-var(data_gis_final)

diag(item_var)















