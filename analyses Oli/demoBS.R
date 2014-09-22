## program:     demoBS
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
daten<-read.dta("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Valorisierung/Ungleichheitsworkshop Neuchatel 2014/Inequality by demographic factors/taxdata_BS.dta")

##
# Prepare variables

# Agegroups

daten$agegroup[daten$alterskategorie=="00_26"]<-1
daten$agegroup[daten$alterskategorie=="26_35"]<-2
daten$agegroup[daten$alterskategorie=="36_45"]<-2
daten$agegroup[daten$alterskategorie=="46_55"]<-2
daten$agegroup[daten$alterskategorie=="56_65"]<-2
daten$agegroup[daten$alterskategorie=="66_75"]<-3
daten$agegroup[daten$alterskategorie=="76_85"]<-3
daten$agegroup[daten$alterskategorie=="86_95"]<-3
daten$agegroup[daten$alterskategorie=="95_+"]<-3

daten$agegroup<-factor(daten$agegroup,
                      levels=c(1,2,3),
                      labels=c("18-25","26-65","66+"))
# Housholdstructure

daten$household[daten$ZIVILSTAND=="verheiratet" & daten$anzahl_kinder==0]<-1
daten$household[daten$ZIVILSTAND=="verheiratet" & daten$anzahl_kinder>0]<-2
daten$household[daten$ZIVILSTAND=="ledig" 
                & daten$anzahl_kinder==0 & daten$GESCHLECHT_P1=="Mann"]<-3
daten$household[daten$ZIVILSTAND=="geschieden/getrennt"
                & daten$anzahl_kinder==0 & daten$GESCHLECHT_P1=="Mann"]<-3
daten$household[daten$ZIVILSTAND=="verwittwet"
                & daten$anzahl_kinder==0 & daten$GESCHLECHT_P1=="Mann"]<-3
daten$household[daten$ZIVILSTAND=="ledig" 
                & daten$anzahl_kinder==0 & daten$GESCHLECHT_P1=="Frau"]<-4
daten$household[daten$ZIVILSTAND=="geschieden/getrennt" 
                & daten$anzahl_kinder==0 & daten$GESCHLECHT_P1=="Frau"]<-4
daten$household[daten$ZIVILSTAND=="verwittwet" 
                & daten$anzahl_kinder==0 & daten$GESCHLECHT_P1=="Frau"]<-4
daten$household[daten$ZIVILSTAND=="ledig"
                & daten$anzahl_kinder>1 & daten$GESCHLECHT_P1=="Mann"]<-5
daten$household[daten$ZIVILSTAND=="geschieden/getrennt"
                & daten$anzahl_kinder>1 & daten$GESCHLECHT_P1=="Mann"]<-5
daten$household[daten$ZIVILSTAND=="verwittwet"
                & daten$anzahl_kinder>1 & daten$GESCHLECHT_P1=="Mann"]<-5
daten$household[daten$ZIVILSTAND=="ledig"
                & daten$anzahl_kinder>1 & daten$GESCHLECHT_P1=="Frau"]<-6
daten$household[daten$ZIVILSTAND=="geschieden/getrennt"
                & daten$anzahl_kinder>1 & daten$GESCHLECHT_P1=="Frau"]<-6
daten$household[daten$ZIVILSTAND=="verwittwet"
                & daten$anzahl_kinder>1 & daten$GESCHLECHT_P1=="Frau"]<-6

daten$household<-factor(daten$household,
                       levels=c(1,2,3,4,5,6),
                       labels=c("Married with Kid(s)","Married without Kid(s)",
                                "Single man","Single women","Single dad","Single mom"))

## Für Basel wäre es möglich mit dem Reineinkommen eine lange Zeitreihe abzubilden (1991 - 2011)
#replace reineinkommen = z739_reineinkommen if PERIODE>2004

daten$reineinkommen[daten$steuerjahr>2004]<-daten$z739_reineinkommen[daten$steuerjahr>2004]

##
# Subset datasets
datenbs1<-filter(daten,steuerjahr==c(1991,2003))
save(datenbs1,file="datenbs1.Rda")
datenbs2<-filter(daten,steuerjahr==c(2006,2011))
save(datenbs2,file="datenbs2.Rda")












