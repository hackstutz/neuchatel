# Vorschläge für Analysen

## Aufstiege/Abstiege der Personen über die Zeit

## Subgruppenanalyse per reldist (Altersgruppen/Geschlecht/Haushalte und Kombinationen daraus)

## Migrationsströme nach OW und von OW weg (z.B. nach Schwyz), Steuerwettbewerb den OW verloren hat kann man hier abbilden

##############################################################################################

# Libraries

library(dplyr)
library(ggplot2)
library(foreign)
library(reldist)

### Obwalden ####################################
#################################################

# Pfad setzen
setwd("G:/stick/obwalden/")
load("obwalden_entschaerft.RData")

ow <- select(daten, PERSID, SEX, HEIMATORT, BERUF, ZIVILSTAND, NATIONALITAET, AUFENTHALTSBEW, STEUERJAHR, JAHRGANG, WOHNORT, ESATZBESTKOPF, VSATZBESTKOPF) %.%
  filter(STEUERJAHR==2011&ZIVILSTAND<2&SEX<3) %.%
  mutate(SEX=factor(SEX,labels=c("men","women")), ZIVILSTAND=factor(ZIVILSTAND, labels=c("unmarried","married")))

# Überblick Verteilungen
median_income <- median(log(ow$ESATZBESTKOPF),na.rm=TRUE)
abs_median <- data.frame(median_income)
sep_median <- as.data.frame(ow %.% group_by(ZIVILSTAND,SEX) %.% summarise(median_income=median(ESATZBESTKOPF,na.rm=TRUE)))
ggplot(ow,aes(x=log(ESATZBESTKOPF)))+geom_histogram(aes(y=..density..),binwidth=0.1)+facet_wrap(~ZIVILSTAND+SEX)

#+geom_vline(data=sep_median,aes(xintercept=median_income,group=NULL),colour="blue")
#,aes(x=median_income),colour="red")+geom_vline(sep_income,aes(x=median_income),colour="blue")


### Aargau ######################################
#################################################

setwd("G:/stick/daten_dta/")
ag <- select(read.dta("snat2007.dta"), gem, ziv, gebdat, kind, p600c, p800c) %.% mutate(alter_kat=cut(gebdat, c(-Inf,19020101, 19420101, 19520101,19620101,19720101,19820101,Inf)))
ggplot(ag,aes(x=log(p600c)))+geom_histogram(aes(y=..density..),binwidth=0.1)+facet_wrap(~gem)
ggplot(ag,aes(x=log(p600c)))+geom_histogram(aes(y=..density..),binwidth=0.1)+facet_wrap(~ziv)
ggplot(ag,aes(x=log(p600c)))+geom_histogram(aes(y=..density..),binwidth=0.1)+facet_wrap(~kind)
ggplot(ag,aes(x=log(p600c)))+geom_histogram(aes(y=..density..),binwidth=0.1)+facet_wrap(~alter_kat,ncol=1)
ggplot(ag,aes(x=log(p800c)))+geom_histogram(aes(y=..density..),binwidth=0.1)+facet_wrap(~alter_kat,ncol=1)


# Jura laden
setwd("G:/stick/jura/")
jr <- read.csv("taxdata_JU.csv")
jr <- filter(jr, PERIODE==2012) %.% 
  select(ZIVILSTAND, GEBURTSJAHR_P1,GEBURTSJAHR_P2, ANZAHL_KINDER,GESCHLECHT_P1,GESCHLECHT_P2,steuertotal,stbeink_BUND,STBVERM) %.%
  mutate(alter_kat=cut(GEBURTSJAHR_P1,c(-Inf, 1920, 1930, 1940, 1950, 1960, 1970, 1980,1990, 2000, Inf)))
# nach zivilstand
ggplot(jr[jr$stbeink_BUND>0&jr$ZIVILSTAND!="unbekannt",],aes(x=log(stbeink_BUND)))+geom_histogram(aes(y=..density..),binwidth=0.1)+facet_wrap(~ZIVILSTAND,ncol=1)
# nach geschlecht des hauptverdieners
ggplot(jr[jr$stbeink_BUND>0&jr$ZIVILSTAND=="ledig",],aes(x=log(stbeink_BUND)))+geom_histogram(aes(y=..density..),binwidth=0.1)+facet_wrap(~GESCHLECHT_P1,ncol=1)
# nach gemeinde
ggplot(jr[jr$stbeink_BUND>0,],aes(x=log(stbeink_BUND)))+geom_histogram(aes(y=..density..),binwidth=0.1)+facet_wrap(~BFS)
# vermögen über alle altersschichten
ggplot(jr[jr$stbeink_BUND>0,],aes(x=log(STBVERM)))+geom_histogram(aes(y=..density..),binwidth=0.1)+facet_wrap(~alter_kat,ncol=1)

#gender-gap
reldist(y=filter(jr,GESCHLECHT_P1==1&ZIVILSTAND=="ledig")$stbeink_BUND,yo=filter(jr,GESCHLECHT_P1==0&ZIVILSTAND=="ledig")$stbeink_BUND)



### Müll den man evtl später wieder braucht
library(RCurl)
jr <- read.csv("https://extrawgs.bfh.ch/projekte/000693/Jura/taxdata_JU.csv")

curl = getCurlHandle()
params <-
  list(
    'userAgent' = "Firefox/31.0",
    'curl'    = "Z2FprojekteZ2F000693Z2FBundZ2FFormsZ2FAllItems.aspx",
    'flags'   = "0",
    'forcedownlevel' = "0",
    'formdir' = "3",
    'rdoPblc' = "0",
    'rdoPrvt' = "4",
    'referrer' = "http://www.aplia.com",
    'username' = "ext-farr1",
    'password' = "9a3KfqaM"
  )
html = postForm('https://extrawgs.bfh.ch/CookieAuth.dll', .params = params, curl = curl, style="POST", .opts = list(ssl.verifypeer = FALSE))
html


