Inequality by Demographic Factors - Basel
========================================================



Load Library

```r
library(foreign,quietly=TRUE,warn.conflicts=FALSE)
library(plyr,quietly=TRUE,warn.conflicts=FALSE)
library(dplyr,quietly=TRUE,warn.conflicts=FALSE)
library(ggplot2,quietly=TRUE,warn.conflicts=FALSE)
library(ineq,quietly=TRUE,warn.conflicts=FALSE)
library(IC2,quietly=TRUE,warn.conflicts=FALSE)
```

Load Data 

```r
load("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Valorisierung/Ungleichheitsworkshop Neuchatel 2014/Inequality by demographic factors/Auswertungen/datenbs.Rda")
```


Achtung: 
* Einkommen sind nicht preisbereinigt. Vergleich der Mittelwerte über die Zeit sind damit nicht zu interpretieren
* Es wird die Ungleichheit zwischen Steuereinheiten und nicht zwischen Haushalten untersucht
* Es wurde keine Äquivalenzgewichtung vorgenommen


<br><br>
<br><br>

# Change of social structure and between group inequality

## Periode 1: 1991 to 2003

### 1991

**Agegroup and mean taxable income**

```r
prop.table(table(datenbs$agegroup[datenbs$steuerjahr==1991]))
```

```
## 
##  18-25  26-65    66+ 
## 0.1190 0.6259 0.2551
```

```r
by(datenbs$einkommen_steuerbar[datenbs$steuerjahr==1991],datenbs$agegroup[datenbs$steuerjahr==1991],mean)
```

```
## datenbs$agegroup[datenbs$steuerjahr == 1991]: 18-25
## [1] 22636
## -------------------------------------------------------- 
## datenbs$agegroup[datenbs$steuerjahr == 1991]: 26-65
## [1] 57268
## -------------------------------------------------------- 
## datenbs$agegroup[datenbs$steuerjahr == 1991]: 66+
## [1] 45275
```

**Housholdstructur and mean income**

```r
prop.table(table(datenbs$household[datenbs$steuerjahr==1991]))
```

```
## 
##    Married with Kid(s) Married without Kid(s)             Single man 
##               0.295989               0.141344               0.229389 
##           Single women             Single dad             Single mom 
##               0.321332               0.005724               0.006221
```

```r
by(datenbs$einkommen_steuerbar[datenbs$steuerjahr==1991],datenbs$household[datenbs$steuerjahr==1991],mean)
```

```
## datenbs$household[datenbs$steuerjahr == 1991]: Married with Kid(s)
## [1] 62937
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 1991]: Married without Kid(s)
## [1] 74390
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 1991]: Single man
## [1] 40735
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 1991]: Single women
## [1] 34585
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 1991]: Single dad
## [1] 67091
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 1991]: Single mom
## [1] 33590
```


### 2003 

**Agegroup and mean income**

```r
prop.table(table(datenbs$agegroup[datenbs$steuerjahr==2003]))
```

```
## 
##  18-25  26-65    66+ 
## 0.1186 0.6268 0.2546
```

```r
by(datenbs$einkommen_steuerbar[datenbs$steuerjahr==2003],datenbs$agegroup[datenbs$steuerjahr==2003],mean)
```

```
## datenbs$agegroup[datenbs$steuerjahr == 2003]: 18-25
## [1] 18217
## -------------------------------------------------------- 
## datenbs$agegroup[datenbs$steuerjahr == 2003]: 26-65
## [1] 64825
## -------------------------------------------------------- 
## datenbs$agegroup[datenbs$steuerjahr == 2003]: 66+
## [1] 57303
```

**Housholdstructur and mean income**

```r
prop.table(table(datenbs$household[datenbs$steuerjahr==2003]))
```

```
## 
##    Married with Kid(s) Married without Kid(s)             Single man 
##               0.243120               0.134682               0.267215 
##           Single women             Single dad             Single mom 
##               0.345038               0.001302               0.008642
```

```r
by(datenbs$einkommen_steuerbar[datenbs$steuerjahr==2003],datenbs$household[datenbs$steuerjahr==2003],mean)
```

```
## datenbs$household[datenbs$steuerjahr == 2003]: Married with Kid(s)
## [1] 81836
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2003]: Married without Kid(s)
## [1] 83011
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2003]: Single man
## [1] 44111
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2003]: Single women
## [1] 41619
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2003]: Single dad
## [1] 64957
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2003]: Single mom
## [1] 53008
```

<br><br>
<br><br>

## Periode 2: 2006 to 2011

### 2006

**Agegroup and mean Total income**

```r
prop.table(table(datenbs$agegroup[datenbs$steuerjahr==2006]))
```

```
## 
##  18-25  26-65    66+ 
## 0.1223 0.6236 0.2541
```

```r
by(datenbs$TOTEINK[datenbs$steuerjahr==2006],datenbs$agegroup[datenbs$steuerjahr==2006],mean)
```

```
## datenbs$agegroup[datenbs$steuerjahr == 2006]: 18-25
## [1] 20197
## -------------------------------------------------------- 
## datenbs$agegroup[datenbs$steuerjahr == 2006]: 26-65
## [1] 82165
## -------------------------------------------------------- 
## datenbs$agegroup[datenbs$steuerjahr == 2006]: 66+
## [1] 71762
```

**Housholdstructur and mean income**

```r
prop.table(table(datenbs$household[datenbs$steuerjahr==2006]))
```

```
## 
##    Married with Kid(s) Married without Kid(s)             Single man 
##               0.231597               0.129073               0.277327 
##           Single women             Single dad             Single mom 
##               0.350736               0.001816               0.009452
```

```r
by(datenbs$TOTEINK[datenbs$steuerjahr==2006],datenbs$household[datenbs$steuerjahr==2006],mean)
```

```
## datenbs$household[datenbs$steuerjahr == 2006]: Married with Kid(s)
## [1] 102664
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2006]: Married without Kid(s)
## [1] 121860
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2006]: Single man
## [1] 51323
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2006]: Single women
## [1] 49936
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2006]: Single dad
## [1] 92635
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2006]: Single mom
## [1] 87158
```


### 2011

**Agegroup and mean income**

```r
prop.table(table(datenbs$agegroup[datenbs$steuerjahr==2011]))
```

```
## 
##  18-25  26-65    66+ 
## 0.1220 0.6278 0.2502
```

```r
by(datenbs$TOTEINK[datenbs$steuerjahr==2011],datenbs$agegroup[datenbs$steuerjahr==2011],mean)
```

```
## datenbs$agegroup[datenbs$steuerjahr == 2011]: 18-25
## [1] 20531
## -------------------------------------------------------- 
## datenbs$agegroup[datenbs$steuerjahr == 2011]: 26-65
## [1] 88042
## -------------------------------------------------------- 
## datenbs$agegroup[datenbs$steuerjahr == 2011]: 66+
## [1] 77587
```

**Housholdstructur and mean income**

```r
prop.table(table(datenbs$household[datenbs$steuerjahr==2011]))
```

```
## 
##    Married with Kid(s) Married without Kid(s)             Single man 
##               0.205975               0.129297               0.294826 
##           Single women             Single dad             Single mom 
##               0.357226               0.001951               0.010725
```

```r
by(datenbs$TOTEINK[datenbs$steuerjahr==2011],datenbs$household[datenbs$steuerjahr==2011],mean)
```

```
## datenbs$household[datenbs$steuerjahr == 2011]: Married with Kid(s)
## [1] 113979
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2011]: Married without Kid(s)
## [1] 135567
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2011]: Single man
## [1] 54013
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2011]: Single women
## [1] 53785
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2011]: Single dad
## [1] 98841
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2011]: Single mom
## [1] 64015
```

<br><br>
<br><br>


## Periode 3: 1991 to 2011

### 1991

**Agegroup and mean Reineinkommen**

```r
prop.table(table(datenbs$agegroup[datenbs$steuerjahr==1991]))
```

```
## 
##  18-25  26-65    66+ 
## 0.1190 0.6259 0.2551
```

```r
by(datenbs$reineinkommen[datenbs$steuerjahr==1991],datenbs$agegroup[datenbs$steuerjahr==1991],mean)
```

```
## datenbs$agegroup[datenbs$steuerjahr == 1991]: 18-25
## [1] 23892
## -------------------------------------------------------- 
## datenbs$agegroup[datenbs$steuerjahr == 1991]: 26-65
## [1] 64890
## -------------------------------------------------------- 
## datenbs$agegroup[datenbs$steuerjahr == 1991]: 66+
## [1] 51706
```

**Agegroup (more categories) and mean Reineinkommen**

```r
prop.table(table(datenbs$alterskategorie[datenbs$steuerjahr==1991]))
```

```
## 
##    00_26    26_35    36_45    46_55    56_65    66_75    76_85    86_95 
## 0.118994 0.192969 0.153221 0.143052 0.136680 0.118630 0.102896 0.032310 
##     95_+ 
## 0.001248
```

```r
by(datenbs$reineinkommen[datenbs$steuerjahr==1991],datenbs$alterskategorie[datenbs$steuerjahr==1991],mean)
```

```
## datenbs$alterskategorie[datenbs$steuerjahr == 1991]: 00_26
## [1] 23892
## -------------------------------------------------------- 
## datenbs$alterskategorie[datenbs$steuerjahr == 1991]: 26_35
## [1] 46674
## -------------------------------------------------------- 
## datenbs$alterskategorie[datenbs$steuerjahr == 1991]: 36_45
## [1] 66918
## -------------------------------------------------------- 
## datenbs$alterskategorie[datenbs$steuerjahr == 1991]: 46_55
## [1] 78919
## -------------------------------------------------------- 
## datenbs$alterskategorie[datenbs$steuerjahr == 1991]: 56_65
## [1] 73652
## -------------------------------------------------------- 
## datenbs$alterskategorie[datenbs$steuerjahr == 1991]: 66_75
## [1] 57825
## -------------------------------------------------------- 
## datenbs$alterskategorie[datenbs$steuerjahr == 1991]: 76_85
## [1] 47700
## -------------------------------------------------------- 
## datenbs$alterskategorie[datenbs$steuerjahr == 1991]: 86_95
## [1] 42875
## -------------------------------------------------------- 
## datenbs$alterskategorie[datenbs$steuerjahr == 1991]: 95_+
## [1] 28946
```

**Housholdstructur and mean income**

```r
prop.table(table(datenbs$household[datenbs$steuerjahr==1991]))
```

```
## 
##    Married with Kid(s) Married without Kid(s)             Single man 
##               0.295989               0.141344               0.229389 
##           Single women             Single dad             Single mom 
##               0.321332               0.005724               0.006221
```

```r
by(datenbs$reineinkommen[datenbs$steuerjahr==1991],datenbs$household[datenbs$steuerjahr==1991],mean)
```

```
## datenbs$household[datenbs$steuerjahr == 1991]: Married with Kid(s)
## [1] 69759
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 1991]: Married without Kid(s)
## [1] 92966
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 1991]: Single man
## [1] 43174
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 1991]: Single women
## [1] 37683
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 1991]: Single dad
## [1] 88125
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 1991]: Single mom
## [1] 51969
```

### 2011

**Agegroup and mean income**

```r
prop.table(table(datenbs$agegroup[datenbs$steuerjahr==2011]))
```

```
## 
##  18-25  26-65    66+ 
## 0.1220 0.6278 0.2502
```

```r
by(datenbs$reineinkommen[datenbs$steuerjahr==2011],datenbs$agegroup[datenbs$steuerjahr==2011],mean)
```

```
## datenbs$agegroup[datenbs$steuerjahr == 2011]: 18-25
## [1] 16385
## -------------------------------------------------------- 
## datenbs$agegroup[datenbs$steuerjahr == 2011]: 26-65
## [1] 72964
## -------------------------------------------------------- 
## datenbs$agegroup[datenbs$steuerjahr == 2011]: 66+
## [1] 68001
```

**Agegroup (more categories) and mean income**

```r
prop.table(table(datenbs$alterskategorie[datenbs$steuerjahr==2011]))
```

```
## 
##    00_26    26_35    36_45    46_55    56_65    66_75    76_85    86_95 
## 0.122034 0.163346 0.156984 0.170398 0.137058 0.110929 0.094215 0.041206 
##     95_+ 
## 0.003831
```

```r
by(datenbs$reineinkommen[datenbs$steuerjahr==2011],datenbs$alterskategorie[datenbs$steuerjahr==2011],mean)
```

```
## datenbs$alterskategorie[datenbs$steuerjahr == 2011]: 00_26
## [1] 16385
## -------------------------------------------------------- 
## datenbs$alterskategorie[datenbs$steuerjahr == 2011]: 26_35
## [1] 50935
## -------------------------------------------------------- 
## datenbs$alterskategorie[datenbs$steuerjahr == 2011]: 36_45
## [1] 77221
## -------------------------------------------------------- 
## datenbs$alterskategorie[datenbs$steuerjahr == 2011]: 46_55
## [1] 82534
## -------------------------------------------------------- 
## datenbs$alterskategorie[datenbs$steuerjahr == 2011]: 56_65
## [1] 82446
## -------------------------------------------------------- 
## datenbs$alterskategorie[datenbs$steuerjahr == 2011]: 66_75
## [1] 74886
## -------------------------------------------------------- 
## datenbs$alterskategorie[datenbs$steuerjahr == 2011]: 76_85
## [1] 65806
## -------------------------------------------------------- 
## datenbs$alterskategorie[datenbs$steuerjahr == 2011]: 86_95
## [1] 56961
## -------------------------------------------------------- 
## datenbs$alterskategorie[datenbs$steuerjahr == 2011]: 95_+
## [1] 41353
```


**Housholdstructur and mean income**

```r
prop.table(table(datenbs$household[datenbs$steuerjahr==2011]))
```

```
## 
##    Married with Kid(s) Married without Kid(s)             Single man 
##               0.205975               0.129297               0.294826 
##           Single women             Single dad             Single mom 
##               0.357226               0.001951               0.010725
```

```r
by(datenbs$reineinkommen[datenbs$steuerjahr==2011],datenbs$household[datenbs$steuerjahr==2011],mean)
```

```
## datenbs$household[datenbs$steuerjahr == 2011]: Married with Kid(s)
## [1] 96860
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2011]: Married without Kid(s)
## [1] 112782
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2011]: Single man
## [1] 45636
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2011]: Single women
## [1] 45157
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2011]: Single dad
## [1] 81604
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2011]: Single mom
## [1] 54343
```

<br><br>
<br><br>

# Change of overall inequality

## Periode 3: 1991 to 2011

### 1991


```r
Gini(datenbs$reineinkommen[datenbs$steuerjahr==1991])
```

```
## [1] 0.4318
```

```r
Theil(datenbs$reineinkommen[datenbs$steuerjahr==1991])
```

```
## [1] 0.3793
```


### 2011


```r
Gini(datenbs$reineinkommen[datenbs$steuerjahr==2011])
```

```
## [1] 0.4937
```

```r
Theil(datenbs$reineinkommen[datenbs$steuerjahr==2011])
```

```
## [1] 0.4973
```


### Lorenz curve


```r
Lc.1991<-Lc(datenbs$reineinkommen[datenbs$steuerjahr==1991])
Lc.2011<-Lc(datenbs$reineinkommen[datenbs$steuerjahr==2011])

plot(Lc.1991,lty="dotted")
lines(Lc.2011$p, Lc.2011$L,lty="dashed", lwd=1.2, col=4)
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19.png) 

## Change of inequality measured with calcGEI (THEIL)


```r
# calcGEI kann nix mit 0 anfangen, macht aber das selbe
x.1991<-datenbs$reineinkommen[datenbs$steuerjahr==1991]
x.1991[x.1991==0]<-1
calcGEI(x.1991)
```

```
## $ineq
## $ineq$index
##    GEI 
## 0.4115 
## 
## $ineq$parameter
## alpha 
##     1 
## 
## 
## $nas
## $nas$xNA
## [1] 0
## 
## $nas$wNA
## NULL
## 
## $nas$totalNA
## [1] 0
## 
## 
## attr(,"class")
## [1] "ICI"
```

```r
Theil(x.1991)
```

```
## [1] 0.4115
```

```r
x.2011<-datenbs$reineinkommen[datenbs$steuerjahr==2011]
x.2011[x.2011==0]<-1
calcGEI(x.2011)
```

```
## $ineq
## $ineq$index
##    GEI 
## 0.5626 
## 
## $ineq$parameter
## alpha 
##     1 
## 
## 
## $nas
## $nas$xNA
## [1] 0
## 
## $nas$wNA
## NULL
## 
## $nas$totalNA
## [1] 0
## 
## 
## attr(,"class")
## [1] "ICI"
```

```r
Theil(x.2011)
```

```
## [1] 0.5626
```


<br><br>
<br><br>

# Decomposing overall inequality

## Households

### 1991
 

```r
decompGEI(x.1991,datenbs$household[datenbs$steuerjahr==1991])
```

```
## $ineq
## $ineq$index
##    GEI 
## 0.4152 
## 
## $ineq$parameter
## alpha 
##     1 
## 
## 
## $decomp
## $decomp$within
## [1] 0.3552
## 
## $decomp$between
## [1] 0.05998
## 
## $decomp$betweenELMO
## [1] 0.2725
## 
## 
## $intra
## $intra$GEIGroups
##    Married with Kid(s) Married without Kid(s)             Single man 
##                 0.3881                 0.2554                 0.3642 
##           Single women             Single dad             Single mom 
##                 0.4123                 0.1830                 0.1678 
## 
## $intra$contribGEIGroups
##    Married with Kid(s) Married without Kid(s)             Single man 
##              0.1415248              0.0592669              0.0636980 
##           Single women             Single dad             Single mom 
##              0.0881610              0.0016301              0.0009578 
## 
## 
## $ws
## $ws$wIntra
##    Married with Kid(s) Married without Kid(s)             Single man 
##               0.295989               0.141344               0.229389 
##           Single women             Single dad             Single mom 
##               0.321332               0.005724               0.006221 
## 
## $ws$sIntra
##    Married with Kid(s) Married without Kid(s)             Single man 
##               0.364623               0.232041               0.174889 
##           Single women             Single dad             Single mom 
##               0.213831               0.008908               0.005709 
## 
## 
## $nas
## $nas$xNA
## [1] 0
## 
## $nas$zNA
## [1] 2498
## 
## $nas$wNA
## NULL
## 
## $nas$totalNA
## [1] 2498
## 
## 
## attr(,"class")
## [1] "ICI"
```


### 2011


```r
decompGEI(x.2011,datenbs$household[datenbs$steuerjahr==2011])
```

```
## $ineq
## $ineq$index
##    GEI 
## 0.5594 
## 
## $ineq$parameter
## alpha 
##     1 
## 
## 
## $decomp
## $decomp$within
## [1] 0.4757
## 
## $decomp$between
## [1] 0.08372
## 
## $decomp$betweenELMO
## [1] 0.3641
## 
## 
## $intra
## $intra$GEIGroups
##    Married with Kid(s) Married without Kid(s)             Single man 
##                 0.5532                 0.3848                 0.4597 
##           Single women             Single dad             Single mom 
##                 0.4849                 0.2353                 0.2765 
## 
## $intra$contribGEIGroups
##    Married with Kid(s) Married without Kid(s)             Single man 
##              0.1701708              0.0865137              0.0953516 
##           Single women             Single dad             Single mom 
##              0.1205982              0.0005776              0.0024845 
## 
## 
## $ws
## $ws$wIntra
##    Married with Kid(s) Married without Kid(s)             Single man 
##               0.205975               0.129297               0.294826 
##           Single women             Single dad             Single mom 
##               0.357226               0.001951               0.010725 
## 
## $ws$sIntra
##    Married with Kid(s) Married without Kid(s)             Single man 
##               0.307591               0.224824               0.207440 
##           Single women             Single dad             Single mom 
##               0.248705               0.002454               0.008986 
## 
## 
## $nas
## $nas$xNA
## [1] 0
## 
## $nas$zNA
## [1] 3835
## 
## $nas$wNA
## NULL
## 
## $nas$totalNA
## [1] 3835
## 
## 
## attr(,"class")
## [1] "ICI"
```


#### Interpretation

* Ungleichheit ist von 1991 bis 2011 gestiegen (0.43 auf 0.51)
* Dies ist mit einer Zunahme der within-inequality und einer Abnahme der between-inequality verbunden
* Die within Group inequality war 1991 bei den Verheirateten mit Kinder am grössten, diese Gruppe hat auch am meisten zur overall inequality beigetragen (16%?), 2011 ist die Ungleichheit stark durch die Gruppen single Man (13%) und Women (24%) getrieben, während die Beitrag der Ungleichheit bei den Verheirateten mit Kindern (8%) an Bedeutung verloren hat. Generell ist der Anteil der Verheirateten zurückgegangen, während der Anteil an Singels stark zugenommen hat.
* Der Wandel der Lebensentwürfe scheint sich in den Daten niederzuschlagen und auch einen Teil der Zunahme der Overall inequality zu "erklären"
* Alleinerziehende tragen nicht viel zur Overall inequality bei, obwohl der Anteil an Alleinerziehenden Mütter geringfügig zugenommen hat und diese Gruppe im Mittel über deutlich am wenigsten Einkommen verfügt. Es sind aber einfach sehr wenige.


## Agegroups

### 1991
 

```r
decompGEI(x.1991,datenbs$agegroup[datenbs$steuerjahr==1991])
```

```
## $ineq
## $ineq$index
##    GEI 
## 0.4115 
## 
## $ineq$parameter
## alpha 
##     1 
## 
## 
## $decomp
## $decomp$within
## [1] 0.3787
## 
## $decomp$between
## [1] 0.0328
## 
## $decomp$betweenELMO
## [1] 0.1732
## 
## 
## $intra
## $intra$GEIGroups
##  18-25  26-65    66+ 
## 0.3889 0.3072 0.5966 
## 
## $intra$contribGEIGroups
##   18-25   26-65     66+ 
## 0.01952 0.22024 0.13891 
## 
## 
## $ws
## $ws$wIntra
##  18-25  26-65    66+ 
## 0.1190 0.6259 0.2551 
## 
## $ws$sIntra
##   18-25   26-65     66+ 
## 0.05019 0.71698 0.23283 
## 
## 
## $nas
## $nas$xNA
## [1] 0
## 
## $nas$zNA
## [1] 0
## 
## $nas$wNA
## NULL
## 
## $nas$totalNA
## [1] 0
## 
## 
## attr(,"class")
## [1] "ICI"
```


### 2011


```r
decompGEI(x.2011,datenbs$agegroup[datenbs$steuerjahr==2011])
```

```
## $ineq
## $ineq$index
##    GEI 
## 0.5626 
## 
## $ineq$parameter
## alpha 
##     1 
## 
## 
## $decomp
## $decomp$within
## [1] 0.5088
## 
## $decomp$between
## [1] 0.05382
## 
## $decomp$betweenELMO
## [1] 0.2252
## 
## 
## $intra
## $intra$GEIGroups
##  18-25  26-65    66+ 
## 0.6397 0.4846 0.5586 
## 
## $intra$contribGEIGroups
##   18-25   26-65     66+ 
## 0.01974 0.34243 0.14660 
## 
## 
## $ws
## $ws$wIntra
##  18-25  26-65    66+ 
## 0.1220 0.6278 0.2502 
## 
## $ws$sIntra
##   18-25   26-65     66+ 
## 0.03085 0.70669 0.26247 
## 
## 
## $nas
## $nas$xNA
## [1] 0
## 
## $nas$zNA
## [1] 0
## 
## $nas$wNA
## NULL
## 
## $nas$totalNA
## [1] 0
## 
## 
## attr(,"class")
## [1] "ICI"
```


#### Interpretation


* Bedetung der Between Group inequality hat abgenommmen
* Die Anteile der Altersgruppen ist relativ konstant geblieben
* Die Ungleichheit bei den Rentern hat sich markant reduziert
* Dafür ist die Ungleichheit innerhalb der Erwerbsbevölkerung stark gestiegen.
* Ebenso die Bedeutung der Ungleichheit dieser Gruppe für die Overall Ungleichheit



### Medianes Einkommen


### 1991

**Agegroup and medianes Reineinkommen**

```r
by(datenbs$reineinkommen[datenbs$steuerjahr==1991],datenbs$agegroup[datenbs$steuerjahr==1991],median)
```

```
## datenbs$agegroup[datenbs$steuerjahr == 1991]: 18-25
## [1] 22500
## -------------------------------------------------------- 
## datenbs$agegroup[datenbs$steuerjahr == 1991]: 26-65
## [1] 55400
## -------------------------------------------------------- 
## datenbs$agegroup[datenbs$steuerjahr == 1991]: 66+
## [1] 34800
```


**Housholdstructur and mean income**

```r
by(datenbs$reineinkommen[datenbs$steuerjahr==1991],datenbs$household[datenbs$steuerjahr==1991],median)
```

```
## datenbs$household[datenbs$steuerjahr == 1991]: Married with Kid(s)
## [1] 55600
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 1991]: Married without Kid(s)
## [1] 75700
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 1991]: Single man
## [1] 38800
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 1991]: Single women
## [1] 31000
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 1991]: Single dad
## [1] 73950
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 1991]: Single mom
## [1] 46100
```

### 2011

**Agegroup and mean income**

```r
prop.table(table(datenbs$agegroup[datenbs$steuerjahr==2011]))
```

```
## 
##  18-25  26-65    66+ 
## 0.1220 0.6278 0.2502
```

```r
by(datenbs$reineinkommen[datenbs$steuerjahr==2011],datenbs$agegroup[datenbs$steuerjahr==2011],median)
```

```
## datenbs$agegroup[datenbs$steuerjahr == 2011]: 18-25
## [1] 8916
## -------------------------------------------------------- 
## datenbs$agegroup[datenbs$steuerjahr == 2011]: 26-65
## [1] 55504
## -------------------------------------------------------- 
## datenbs$agegroup[datenbs$steuerjahr == 2011]: 66+
## [1] 49348
```


**Housholdstructur and mean income**

```r
by(datenbs$reineinkommen[datenbs$steuerjahr==2011],datenbs$household[datenbs$steuerjahr==2011],median)
```

```
## datenbs$household[datenbs$steuerjahr == 2011]: Married with Kid(s)
## [1] 71384
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2011]: Married without Kid(s)
## [1] 85023
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2011]: Single man
## [1] 37197
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2011]: Single women
## [1] 37949
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2011]: Single dad
## [1] 68704
## -------------------------------------------------------- 
## datenbs$household[datenbs$steuerjahr == 2011]: Single mom
## [1] 48923
```



