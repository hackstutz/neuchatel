## program:     lorenz.R
## task:        Draw pretty lorence curves
## project:     Inequality of income and wealth in switzerland
## subproject:  Inequality by Demographic Factors
## author:      Oliver Hümbelin
## date:        September2014


# libraries 
library(reshape2)
library(gridExtra)


# Load Jura-Data
load("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Valorisierung/Ungleichheitsworkshop Neuchatel 2014/Inequality by demographic factors/Auswertungen/datenju.Rda")

Lc.2006<-Lc(datenju$R590[datenju$steuerjahr==2006])
Lc.2012<-Lc(datenju$R590[datenju$steuerjahr==2012])

lorenz.2006 <- data.frame(Lc.2006$p, Lc.2006$L)
lorenz.2012 <- data.frame(Lc.2012$p, Lc.2012$L)
lorenz.2006$Lorenzkurve="2006"
lorenz.2012$Lorenzkurve="2012"
names(lorenz.2006)<- c("p","L","Lorenzkurve")
names(lorenz.2012)<- c("p","L","Lorenzkurve")
lorenz <- rbind(lorenz.2006, lorenz.2012)


jura<-ggplot(lorenz, aes(x=p,y=L,shape=Lorenzkurve)) + 
  geom_line(data=lorenz,aes(linetype=Lorenzkurve),size=0.5) +
  geom_segment(y=0, x=0,yend=1,xend=1,colour="black",size=0.5)+theme_bw()+
  ggtitle("Jura")+
  theme(plot.title=element_text(size=30,face="bold"))+
  xlab("Share of Tax subjects")+
  ylab("Share of total net income")+
  annotate("text",label="2006",x=0.1,y=0.9,size=8)+
  annotate("text",label="Gini=0.42",x=0.1,y=0.85,size=5)+
  annotate("text",label="Theil=0.32",x=0.1,y=0.80,size=5)+
  annotate("text",label="2012",x=0.1,y=0.7,size=8)+
  annotate("text",label="Gini=0.45",x=0.1,y=0.65,size=5)+
  annotate("text",label="Theil=0.36",x=0.1,y=0.60,size=5)+
  scale_x_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1),labels=c("0%","20%","40%","60%","80%","100%"))+
  scale_y_continuous(labels=c("0%","25%","50%","75%","100%"))
jura

# Load Basel-Data
load("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Valorisierung/Ungleichheitsworkshop Neuchatel 2014/Inequality by demographic factors/Auswertungen/datenbs.Rda")


Lc.1991<-Lc(datenbs$reineinkommen[datenbs$steuerjahr==1991])
Lc.2011<-Lc(datenbs$reineinkommen[datenbs$steuerjahr==2011])

lorenz.1991 <- data.frame(Lc.1991$p, Lc.1991$L)
lorenz.2011 <- data.frame(Lc.2011$p, Lc.2011$L)
lorenz.1991$Lorenzkurve="1991"
lorenz.2011$Lorenzkurve="2011"
names(lorenz.1991)<- c("p","L","Lorenzkurve")
names(lorenz.2011)<- c("p","L","Lorenzkurve")
lorenz <- rbind(lorenz.1991, lorenz.2011)


basel<-ggplot(lorenz, aes(x=p,y=L,shape=Lorenzkurve)) + 
  geom_line(data=lorenz,aes(linetype=Lorenzkurve),size=0.5) +
  geom_segment(y=0, x=0,yend=1,xend=1,colour="black",size=0.5)+theme_bw()+
  ggtitle("Basel-City")+
  theme(plot.title=element_text(size=30,face="bold"))+
  xlab("Share of Tax subjects")+
  ylab("Share of total net income")+
  annotate("text",label="1991",x=0.1,y=0.9,size=8)+
  annotate("text",label="Gini=0.43",x=0.1,y=0.85,size=5)+
  annotate("text",label="Theil=0.39",x=0.1,y=0.80,size=5)+
  annotate("text",label="2011",x=0.1,y=0.7,size=8)+
  annotate("text",label="Gini=0.49",x=0.1,y=0.65,size=5)+
  annotate("text",label="Theil=0.50",x=0.1,y=0.60,size=5)+
  scale_x_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1),labels=c("0%","20%","40%","60%","80%","100%"))+
  scale_y_continuous(labels=c("0%","25%","50%","75%","100%"))
basel


# Ploting together

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(jura,basel, cols=2)


# Exporting it with 1000 / 500




