#############################################
#                                           #
#               PsyBSc8 Script              #
#                Itemanalyse                #
#                                           #
#############################################


### Zun?chst laden wir unseren Datensatz, hierf?r gibt es zwei Packages
### Das Package "foreign" eignet sich besonders gut f?r SPSS-Dateien (.sav)
### Das Package "readr" ist besonders gut f?r CSV-Dateien (.csv)
### F?r unseren Beispieldatensatz ben?tigen wir das Package "readr"

library(readr)

data_gis_raw <- read_csv("data/GIS-data.csv")

# Hauptdatensatz


df2 <- na.omit(data_gis_raw) # Hier erstellen wir einen Datensatz, in der die Personen mit 
# missings aus dem Datensatz entfernt werden können. 


### F?r die Itemanalyse brauchen wir nur die Items ohne den soziodemographischen Angaben
### Daher erstellen wir einen subset der Items ohne soziodemographische Angaben

df.item<-subset(x = df2, select=c(7:27))  # die ersten sechs und die letzte Variable 
# sind soziodemographische Angaben

df.item2<-df.item # duplizieren den Datensatz, falls ein Fehler passiert, k?nnen wir somit
# immer zum urspr?nglichen Datensatz zur?ck

###########################################
####                                   ####
####        Deskriptive Analyse        ####
####           Hauptdatensatz          ####
####                                   ####
###########################################


### Bevor wir die Itemanalyse durchf?hren, wollen wir uns zun?chst ein wenig mit den Daten 
# vertraut machen
### Hierf?r ben?tigen wir zwei Packages:
### Das Package "psych" ist sehr wichtig hierf?r. Es beinhaltet sehr viele Funktionen und 
# Befehle, die auch f?r viele andere Analysen sehr hilfreich sind
### Das Packacge "janitor" ist eine bessere Alternative zum Basis-Befehl "table()" und
# hilft besonders bei H?ufigkeitstabellen

# WIR VERWENDEN DEN HAUPTDATENSATZ df2 (ohne missings)


library(psych)
library(janitor)

### Berechnung von Mittelwerten, Standardabweichungen etc.
### Hierf?r verwenden wir eine Funktion aus dem psych-Package

describe(df2) ### Dieser Befehl beschreibt den ganzen Datensatz aufeinmal

### F?r die Beschreibung von spezifischen Variablen z. B. Alter, wird die Variable direkt
# angesteuert

describe(df2$Age)

### Das die Geschlechtsvariable eine kategoriale Variable ist, ben?tigen wir keine 
# Mittelwerte oder Standardabweichungen

### Wir ben?tigen die relative und absolute H?ufigkeiten und gegebenenfalls die 
# kommulierten relativen H?ufigkeiten, falls es Missings gibt

### Hier benutzen wir einen Befehl aus dem janitor-Package

tabyl(df2$sex)

########################
#                      #
# Tipp f?r den Bericht #
########################

### F?r die Abschlussberichte, braucht ihr die ganzen deskriptiven Informationen
# in Tabellen und am Besten in der passenden Formatierung
### Hierf?r eignet sich das Package "sjPlot" besonders

### F?r die Tabellen ben?tigen wir einige Vorkehrungen

age<-describe(df2$Age) # wir erstellen nun ein Objekt f?r das Alter
### das erstellte Objekt hat eine Tabellenform und kann nun weiterverwendet werden

### Mit dem Befehl aus sjPlot kann nun eine Tabelle erstellt werden

library(sjPlot)

tab_df(age)

### Wir k?nnen hier unserer Tabelle auch einen Titel geben 
# und Fu?noten hinzuf?gen
### weitere Funktionen k?nnen mit "F1" erkundschaftet werden

### Die erstellte Tabelle kann auch direkt als Word-Dokument abgespeichert werden

tab_df(age, file = "descriptive_age.doc") # Endung .doc f?r Word

### Nun k?nnen Sie die Tabelle auch in Word bearbeiten und ggf. anpassen

### Der tabyl Befehl erstellt auch eine Tabelle

sex<-tabyl(df2$sex)

tab_df(sex)

### Auch diese Tabelle k?nnen wir direkt abspeichern


### Wir k?nnen mit dem psych Package auch eine Tabelle nach Gruppen erstellen
describeBy(df2$Age,df2$sex)

### F?r die Gruppierung ben?tigen wir eine kategoriale Variable

### Auch diese Tabelle k?nnen wir abspeichern

age.b.sex<-describeBy(df2$Age,df2$sex)

tab_dfs(age.b.sex,titles = c("Weiblich","Männlich")) ### man kann es auch als
# Word-Dokument abspeichern


### Es gibt auch die M?glichkeit mehrere Tabellen in ein Dokument zu packen

tab_dfs(list(age,sex), titles = c("Descriptives of Age","Descriptives of Sex"))

### Hier sollten wir drauf Achten, dass wir den Tabellen Titel geben, 
# aber es geht auch ohne

### Auch diese Tabellen k?nnen wir als Word-Dokumentabspeichern

tab_dfs(list(age,sex), titles = c("Descriptives of Age","Descriptives of Sex"),
        file="descriptives_all.doc")




###########################################
####                                   ####
####            Itemanalyse            ####
####                                   ####
####                                   ####
###########################################

# F?r die Itemanalyse ben?tigen wir den Datensatz in denen nur die Items vorhanden sind
# Wir verwenden Befehel aus den sjPlot Package. 

library(sjPlot)

sjt.itemanalysis(df.item2) # Er nimmt die Item chronologisch auf.

# Ergebnis anschauen --> Wichtige Indikatoren = Schwierigkeit, Trennsch?rfe und Alpha
# Irgendwas stimmt nicht. Wir haben invertierte Items, also erst invertieren!

### Aus der Theorie und w?hrend der Konstruktion wissen wir, welche Items wir invertiert
# formuliert haben

inv<-c("GIS9","GIS16","GIS17","GIS18") # Auswahl f?r die Befehle

### Zum invertieren verwenden wir das sjmisc Package

### Wir recodieren direkt in ein neues Dataframe

library(sjmisc)

df.inv <- rec(df.item2,df.item2[inv],rec = "0=4; 1=3; 2=2; 3=1; 4=0")

### nur wollen wir unsere nicht invertieren Items aus dem Datensatz entfernen
### Hierf?r ben?tigen wir das Package dplyr

library(dplyr)


df.sel<-select(df.inv,!inv) ### Wir selektieren alle Items au?er die nicht invertierten
# die in den values abgespeichert sind inv

### Nun k?nnen wir eine Itemanalyse durchf?hren

sjt.itemanalysis(df.sel) ### wir k?nnen die Tabelle direkt in Word abspeichern


sjt.itemanalysis(df.sel, file = "Itemanalyse.doc") ###

########### EXTRA TIPP ###################################

##  Es gibt eine M?glichkeit die Itemnamen anzeigen zu lassen
## hierf?r speichern wir die Itemanalyse als Objekt ab

i.analysis<-sjt.itemanalysis(df.sel)

### In der Liste findet sich die Tabelle die geplottet wird, wir erkennen,
# dass die Items als Zeilennamen abgespeichert sind

i.analysis[["df.list"]][[1]]

### diese Tabelle samt Zeilenamen k?nnen wir doch auch anders plotten!

tab_df(i.analysis[["df.list"]][[1]], show.rownames = T) ## Nun sehen wir die Zeilen Namen

### wir k?nnen diese Tabelle auch direkt abspeichern!

############# Extra Tipp 2 ###############################

# Wir k?nnen sogar die Sorierung direkt ver?ndern

# Hierf?r erstellen wir eine Objekt mit der korrekten Sorierung

col_order<-c("GIS1","GIS2","GIS3","GIS4","GIS5","GIS6",
             "GIS7","GIS8","GIS9_r","GIS10",
             "GIS11","GIS12","GIS13","GIS14","GIS15",
             "GIS16_r","GIS17_r","GIS18_r", "GIS19","GIS20",
             "GIS21") ### Wichtig die recodierten Items haben ein _r dran!

### Aus dem Datensatz df.sel erstellen wir ein zweites aber mit dem Befehl
# die neue Spaltenordnung zu nehmen

df.sel2<-df.sel[,col_order] 

sjt.itemanalysis(df.sel2)

#tab_df(i.analy2[["df.list"]][[1]], show.rownames = T) ### Somit haben wir nun alle Items
# in der korrekten Reihenfolge, auch diese Tabelle k?nnen wir abspeichern 

### Entfernt die Items, die eine schlechte Trennsch?rfe haben

df.fin<-subset(df.sel2, select = -c(GIS9_r,GIS16_r,GIS17_r,GIS18_r)) ## die rekodierten Items

sjt.itemanalysis(df.fin)   ### Finale Analyse mit besserer Reliabilit?t

####### McDonald's Omega

# aus dem psych Package den Befehl omega

omega(df.fin) 

## Eine Reihe von Anaylsen werden ausgegeben, wichtig hier ist jedoch nur
# das Omega Total im vergleich zu Cronbach's Alpha!




###########################################
####                                   ####
####            Itemvarianz            ####
####                                   ####
####                                   ####
###########################################


i_var<-var(df.sel2)
diag(i_var)



#############################################
#                                           #
#               PsyBSc8 Script              #
#       Explorative Faktorenanalyse         #
#                                           #
#############################################




###########################################
####                                   ####
####    Explorative Faktorenanalyse    ####
####                                   ####
###########################################

library(readr)

df<-read_csv("EFA_Data.csv")

df.item<-subset(df , select=c(3:22))

df.item2<-df.item

#### Identifaktion der Anzahl von Faktoren

#### Eigenwertkriterium 
## das liberalste Ma? der Entscheidung --> Es entstehen viele Faktoren

# Eigenwerte gr??ergleich 1 bilden eine Faktoren ab

# Erhalt der Eigenwerte ?ber 2 Wege 

library(sjPlot)

# 1. Weg 

eigen(cor(df.item2))$values ## Eigenwerte 

tab_df(as.data.frame(eigen(cor(df.item2))$values), show.rownames = T) # in Tabellenform

plot(eigen(cor(df.item2))$values) # Eigenwerteverlauf als Plot


# 2. Weg etwas mehr Informationen bez?glich aufgekl?rter Varianz
library(FactoMineR)

pca<-PCA(df.item2,graph = F) 

pca[["eig"]] ### Eigenwerte und Varianzaufkl?rung der m?glichen Faktoren

eigen<-as.data.frame(pca[["eig"]]) # Als Dataframe speichern

tab_df(eigen, show.rownames = T) # Tabelle

plot(eigen$eigenvalue) # Eigenwertsverlauf als Plot


## Das Eigenwertkriterium schl?gt uns 5 Faktoren vor 
## Aufgrund unserer theoretischen Herleitung sind es zu viele Faktoren
## wir m?ssen konservativer vorgehen




##########

library(psych)
#### Scree plot

## Ein visuelles deskriptives Kriterium zur Entscheidung der Faktoren
## Konservativer als das Eigenwertkriterium
## optischer Knick wird herangezogen 

scree(df.item2, factors = F, pc=F, hline = -1) # Plot ohne Knicklinie

scree(df.item2, factors = F, pc=F, hline = 1) #Plot mit Knicklinie

#### Aus dem Scree plot gehen 2 oder 3 Faktoren hervor

###########

### Parallelanalyse

## Konservatives Vorgehen
## Eigenwerte aus einer Faktoranalyse unseres Datensatzes, 
## werden mit (normalverteilten) Eigenwerten aus einer simulierten Faktoranalyse verglichen

fa.parallel(df.item2, fm="pa", fa="pc", main = "Parallelanalyse")

# Parllelanalyse zeigt je nach Simulation 2 Komponenten an, 
# wir entscheiden uns wegen der Theorie und der Parallelanalyse f?r 2 Komponenten


#######

## EFA

######

tab_pca(df.item2, nmbr.fctr = 2, rotation = "oblimin", fctr.load.tlrn = .2)

inv<-c("Item06","Item09","Item10","Item11","Item15","Item18","Item19")

library(sjmisc)
library(dplyr)
library(GPArotation)

df.inv<-rec(df.item2,df.item2[inv],rec = "0=6; 1=5; 2=4; 3=3; 4=2; 5=1; 6=0")

df.sel<-select(df.inv,!inv)

tab_pca(df.sel, nmbr.fctr = 1, rotation = "oblimin", fctr.load.tlrn = .2)
tab_fa(df.sel, nmbr.fctr = 2, rotation = "oblimin", fctr.load.tlrn = .2)

tab_fa(df.sel,nmbr.fctr = 1)
#tab_fa(df.sel1, nmbr.fctr = 2, rotation = "promax")

### Item Selektion nach EFA

## Zu niedrige oder Doppelladungen m?ssen entfernt werden 

col_order<-c("Item01","Item02","Item03","Item04","Item05","Item06_r",
             "Item07","Item08","Item09_r","Item10_r",
             "Item11_r","Item12","Item13","Item14","Item15_r","Item16",
             "Item17","Item18_r","Item19_r","Item20")


df.fin<-df.sel[,col_order] 

tab_pca(df.fin, nmbr.fctr = 2, rotation = "oblimin", fctr.load.tlrn = .2)

#### TESTWERT Analyse ####
ak<-c("Item01","Item04","Item06_r","Item08","Item10_r",
      "Item11_r","Item12","Item13","Item16","Item19_r")
hk<-c("Item02","Item03","Item05","Item07","Item09_r",
      "Item14","Item15_r","Item17","Item18_r","Item20")

df.ak<-select(df.fin,ak)
df.hk<-select(df.fin,!ak)

sjt.itemanalysis(df.ak)
sjt.itemanalysis(df.hk)

# Relibalit?t

omega(df.ak,nfactors = 1)

omega(df.hk,nfactors = 1)

#####
df.fin2<-df.fin

df.fin2$AK<-rowSums(df.fin2[,ak])

df.fin2$HK<-rowSums(df.fin2[,hk])

### Normalverteilung
library(ggplot2)
library(ggpubr)

ggplot(df.fin2, aes(AK))  + 
  geom_histogram(colour="black" ,binwidth = 1,
                 breaks = seq(18, 70, by = 1)) + 
  stat_function(fun = function(x) 
    dnorm(x, mean = mean(df.fin2$AK), sd = sd(df.fin2$AK)) * 1 *305,   # 1 =binwidth (Abst?nde) 305 = Observations
    color="skyblue2", size=1)+
  ggtitle("Histogram with normal distribution curve")+
  ylab("Frequency")+
  theme_pubr()

ggplot(df.fin2, aes(HK))  + 
  geom_histogram(colour="black" ,binwidth = 1,
                 breaks = seq(18, 70, by = 1)) + 
  stat_function(fun = function(x) 
    dnorm(x, mean = mean(df.fin2$AK), sd = sd(df.fin2$AK)) * 1 *305,   # 1 =binwidth (Abst?nde) 305 = Observations
    color="skyblue2", size=1)+
  ggtitle("Histogram with normal distribution curve")+
  ylab("Frequency")+
  theme_pubr()


















