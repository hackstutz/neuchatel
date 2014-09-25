## program:     demoBS
## task:        Prepare Jura data
## project:     Inequality of income and wealth in switzerland
## subproject:  Inequality by Demographic Factors
## author:      Oliver Hümbelin
## date:        September2014

##
# librarys
library(foreign)
library(plyr)
library(dplyr)


##
# Daten (direkt ab sharePoint)
daten<-read.csv("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Valorisierung/Ungleichheitsworkshop Neuchatel 2014/Inequality by demographic factors/taxdata_JU.csv")

##
# Prepare variables

daten$ageP1<-(daten$PERIODE-daten$GEBURTSJAHR_P1)
daten$ageP2<-(daten$PERIODE-daten$GEBURTSJAHR_P2)


# Agegroups (according to age of dossier carrier)

daten$agegroup[daten$ageP1<26]<-1
daten$agegroup[daten$ageP1>25 & daten$ageP1<66 ]<-2
daten$agegroup[daten$ageP1>65]<-3

daten$agegroup<-factor(daten$agegroup,
                       levels=c(1,2,3),
                       labels=c("18-25","26-65","66+"))

# Agegroups - detailled

daten$alterskategorie[daten$ageP1<26]<-1
daten$alterskategorie[daten$ageP1>25 & daten$ageP1<36 ]<-2
daten$alterskategorie[daten$ageP1>35 & daten$ageP1<46 ]<-3
daten$alterskategorie[daten$ageP1>45 & daten$ageP1<56 ]<-4
daten$alterskategorie[daten$ageP1>55 & daten$ageP1<66 ]<-5
daten$alterskategorie[daten$ageP1>65 & daten$ageP1<76 ]<-6
daten$alterskategorie[daten$ageP1>75 & daten$ageP1<86 ]<-7
daten$alterskategorie[daten$ageP1>85 & daten$ageP1<96 ]<-8
daten$alterskategorie[daten$ageP1>95]<-9

daten$alterskategorie<-factor(daten$alterskategorie,
                       levels=c(1,2,3,4,5,6,7,8,9),
                       labels=c("18-25","26-35","36-45","46-55","56-65","66-75","76-85","86-95","95_+"))


# Housholdstructure

daten$household[daten$ZIVILSTAND=="verheiratet" & daten$ANZAHL_KINDER==0]<-1
daten$household[daten$ZIVILSTAND=="verheiratet" & daten$ANZAHL_KINDER>0]<-2
daten$household[daten$ZIVILSTAND=="ledig" 
                & daten$ANZAHL_KINDER==0 & daten$GESCHLECHT_P1==0]<-3
daten$household[daten$ZIVILSTAND=="geschieden/getrennt"
                & daten$ANZAHL_KINDER==0 & daten$GESCHLECHT_P1==0]<-3
daten$household[daten$ZIVILSTAND=="verwittwet"
                & daten$ANZAHL_KINDER==0 & daten$GESCHLECHT_P1==0]<-3
daten$household[daten$ZIVILSTAND=="ledig" 
                & daten$ANZAHL_KINDER==0 & daten$GESCHLECHT_P1==1]<-4
daten$household[daten$ZIVILSTAND=="geschieden/getrennt" 
                & daten$ANZAHL_KINDER==0 & daten$GESCHLECHT_P1==1]<-4
daten$household[daten$ZIVILSTAND=="verwittwet" 
                & daten$ANZAHL_KINDER==0 & daten$GESCHLECHT_P1==1]<-4
daten$household[daten$ZIVILSTAND=="ledig"
                & daten$ANZAHL_KINDER>1 & daten$GESCHLECHT_P1==0]<-5
daten$household[daten$ZIVILSTAND=="geschieden/getrennt"
                & daten$ANZAHL_KINDER>1 & daten$GESCHLECHT_P1==0]<-5
daten$household[daten$ZIVILSTAND=="verwittwet"
                & daten$ANZAHL_KINDER>1 & daten$GESCHLECHT_P1==0]<-5
daten$household[daten$ZIVILSTAND=="ledig"
                & daten$ANZAHL_KINDER>1 & daten$GESCHLECHT_P1==1]<-6
daten$household[daten$ZIVILSTAND=="geschieden/getrennt"
                & daten$ANZAHL_KINDER>1 & daten$GESCHLECHT_P1==1]<-6
daten$household[daten$ZIVILSTAND=="verwittwet"
                & daten$ANZAHL_KINDER>1 & daten$GESCHLECHT_P1==1]<-6

daten$household<-factor(daten$household,
                        levels=c(1,2,3,4,5,6),
                        labels=c("Married with Kid(s)","Married without Kid(s)",
                                 "Single man","Single women","Single dad","Single mom"))

# Jahre Steuerperiode 

daten$steuerjahr<-daten$PERIODE



##
# Subset datasets
datenju<-daten
save(datenju,file="P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Valorisierung/Ungleichheitsworkshop Neuchatel 2014/Inequality by demographic factors/Auswertungen/datenju.Rda")











