## program:     demoZH
## task:        Prepare Basel data
## project:     Inequality of income and wealth in switzerland
## subproject:  Inequality by Demographic Factors
## author:      Oliver Hümbelin
## date:        September2014

##
# librarys
library(foreign)
library(dplyr)


##
# Daten (direkt ab sharePoint)
daten<-read.dta("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Valorisierung/Ungleichheitsworkshop Neuchatel 2014/Inequality by demographic factors/taxdata_ZH_1991u2007_KV.dta")



##
# Prepare variables



# Agegroups

daten$ageP1<-(daten$STEUERJAHR-daten$GEBURTSJAHR)
daten$agegroup[daten$ageP1<26]<-1
daten$agegroup[daten$ageP1>25 & daten$ageP1<66 ]<-2
daten$agegroup[daten$ageP1>65]<-3
daten$agegroup<-factor(daten$agegroup,
                       levels=c(1,2,3),
                       labels=c("18-25","26-65","66+"))

# Housholdstructure


daten$GESCHLECHT_P1<-daten$GESCHLECHT
daten$anzahl_kinder<-daten$ANZAHL_KINDER

daten$household[daten$ZIVILSTAND==2 & daten$anzahl_kinder==0]<-1
daten$household[daten$ZIVILSTAND==2 & daten$anzahl_kinder>0]<-2
daten$household[daten$ZIVILSTAND==1 
                & daten$anzahl_kinder==0 & daten$GESCHLECHT_P1=="männlich"]<-3
daten$household[daten$ZIVILSTAND==3
                & daten$anzahl_kinder==0 & daten$GESCHLECHT_P1=="männlich"]<-3
daten$household[daten$ZIVILSTAND==3
                & daten$anzahl_kinder==0 & daten$GESCHLECHT_P1=="männlich"]<-3
daten$household[daten$ZIVILSTAND==1 
                & daten$anzahl_kinder==0 & daten$GESCHLECHT_P1=="weiblich"]<-4
daten$household[daten$ZIVILSTAND==3 
                & daten$anzahl_kinder==0 & daten$GESCHLECHT_P1=="weiblich"]<-4
daten$household[daten$ZIVILSTAND==3 
                & daten$anzahl_kinder==0 & daten$GESCHLECHT_P1=="weiblich"]<-4
daten$household[daten$ZIVILSTAND==1
                & daten$anzahl_kinder>1 & daten$GESCHLECHT_P1=="männlich"]<-5
daten$household[daten$ZIVILSTAND==3
                & daten$anzahl_kinder>1 & daten$GESCHLECHT_P1=="männlich"]<-5
daten$household[daten$ZIVILSTAND==3
                & daten$anzahl_kinder>1 & daten$GESCHLECHT_P1=="männlich"]<-5
daten$household[daten$ZIVILSTAND==1
                & daten$anzahl_kinder>1 & daten$GESCHLECHT_P1=="weiblich"]<-6
daten$household[daten$ZIVILSTAND==3
                & daten$anzahl_kinder>1 & daten$GESCHLECHT_P1=="weiblich"]<-6
daten$household[daten$ZIVILSTAND==3
                & daten$anzahl_kinder>1 & daten$GESCHLECHT_P1=="weiblich"]<-6

daten$household<-factor(daten$household,
                        levels=c(1,2,3,4,5,6),
                        labels=c("Married with Kid(s)","Married without Kid(s)",
                                 "Single man","Single women","Single dad","Single mom"))

datenzh<-daten
datenzh$steuerjahr<-datenzh$STEUERJAHR


save(datenzh,file="P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Valorisierung/Ungleichheitsworkshop Neuchatel 2014/Inequality by demographic factors/Auswertungen/datenzh.Rda")













