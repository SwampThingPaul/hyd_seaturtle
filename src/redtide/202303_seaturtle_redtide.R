## Title:      Sea turtle red tide data analysis
## Created by: Paul Julian (pjulian@evergladesfoundation.org; pauljulianphd@gmail.com)
## Created on: 2023-03-20

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

# Libraries
# devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape2)
library(openxlsx)

library(flextable)
library(magrittr)

#Paths
wd="C:/Julian_LaCie/_GitHub/hyd_seaturtle"
paths=paste0(wd,c("/Plots/redtide/","/Export/","/Data/RedTideData/"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]


# -------------------------------------------------------------------------
dat=read.xlsx(paste0(data.path,"Red Tide Project Database_03.01.23.xlsx"),sheet=2)

names.val=names(dat)
names.val=gsub("%","per",names.val)
names.val=gsub("/","_",names.val)
names.val=gsub("#","num",names.val)
names.val=gsub(".1.","1.",names.val)
names.val=gsub(".2.","2.",names.val)
names.val=gsub(".3.","3.",names.val)
names.val=gsub("\\(","_",names.val)
names.val=gsub("\\)","_",names.val)

colnames(dat)=names.val

####
## (1)	Characterize plasma brevetoxin concentrations nesting 
## loggerhead sea turtles on Sanibel Island, FL during the nesting 
## seasons following a major red tide event
####
### PbTx MDL egg = 10 ng/g, liver = 10 ng/g, plasma = 1 ng/g
dat2=dat;# preserve the original dataset

unique(dat$Maternal.PbTx3._ng_ml_)
subset(dat,Maternal.PbTx3._ng_ml_==0)
dat$Maternal.PbTx3._ng_ml_[dat$Maternal.PbTx3._ng_ml_%in%c("NA","Missing")]=NA
dat$Maternal.PbTx3._ng_ml_=as.numeric(dat$Maternal.PbTx3._ng_ml_)
dat=subset(dat,is.na(Maternal.PbTx3._ng_ml_)==F)
## Compare 1/2 MDLs 
dat$PbTx3_1=dat$Maternal.PbTx3._ng_ml_
dat$PbTx3_1[dat$PbTx3_1==0]=1/2
dat$PbTx3_10=dat$Maternal.PbTx3._ng_ml_
dat$PbTx3_10[dat$PbTx3_10==0]=10/2

plot(PbTx3_1~PbTx3_10,dat);abline(0,1)
mean(dat$PbTx3_1,na.rm=T)
mean(dat$PbTx3_10,na.rm=T)

boxplot(value~variable,melt(dat[,c("PbTx3_1","PbTx3_10")]))
kruskal.test(value~variable,melt(dat[,c("PbTx3_1","PbTx3_10")]))

library(NADA)
dat$PbTx3_censored=FALSE
dat$PbTx3_censored[dat$Maternal.PbTx3._ng_ml_==0]=T

PbTx.ros=with(dat,cenros(Maternal.PbTx3._ng_ml_,PbTx3_censored))
dat$pp.vals=with(dat,hc.ppoints(Maternal.PbTx3._ng_ml_,PbTx3_censored))
# pred.ros=predict(PbTx.ros,qnorm(dat$pp.vals[dat$PbTx3_censored==T]))

dat$PbTx3_ros=ifelse(dat$PbTx3_censored==T,
                              predict(PbTx.ros,qnorm(dat$pp.vals)),
                              dat$Maternal.PbTx3._ng_ml_)
# mean(pred.ros)
# mean(subset(dat,PbTx3_censored==T)$PbTx3_ros)
range(dat$Maternal.PbTx3._ng_ml_[dat$PbTx3_censored==F])
## Kaplan-Meier method
# PbTx.KM=with(dat,cenfit(Cen(Maternal.PbTx3._ng_ml_,PbTx3_censored)))
# summary(PbTx.KM)
# plot(PbTx.KM)
# mean(PbTx.KM)


boxplot(value~variable,melt(dat[,c("PbTx3_1","PbTx3_ros")]))
kruskal.test(value~variable,melt(dat[,c("PbTx3_1","PbTx3_ros")]))


### 
# dat$Maternal.PbTx3._ng_ml_[dat$Maternal.PbTx3._ng_ml_==0]=1/2

sum.stats.Q1=ddply(dat,c("Year"),summarise,
      median.val=median(PbTx3_ros,na.rm=T),
      mean.val=mean(PbTx3_ros,na.rm=T),
      sd.val=sd(PbTx3_ros,na.rm=T),
      min.val=min(PbTx3_ros,na.rm=T),
      max.val=max(PbTx3_ros,na.rm=T),
      N.val=N.obs(PbTx3_ros))
sum.stats.Q1$mean.val[sum.stats.Q1$mean.val=="NaN"]=NA
sum.stats.Q1$min.val[is.infinite(sum.stats.Q1$min.val)]=NA
sum.stats.Q1$max.val[is.infinite(sum.stats.Q1$max.val)]=NA

sum.stats.Q1%>%
  flextable()%>%
  colformat_double(j=1,digits=0,big.mark = "")%>%
  colformat_double(j=2:6,digits=2,na_str= "---")%>%
  set_header_labels("median.val"="Median",
                    "mean.val"="Mean",
                    "sd.val"="Std Dev",
                    "N.val"="N",
                    "min.val"="Minimum",
                    "max.val"="Maximum")%>%
  padding(padding=1.5,part="all")%>%
  align(j=2:7,part="all",align="center")%>%
  width(width=c(0.5,0.75,0.75,0.75,0.75,0.75,0.75))%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=10,part="all")%>%
  fontsize(size=12,part="header")# %>%print("docx")

subset(dat,Year==2022)

boxplot(PbTx3_ros~Year,dat,outline=F)
kruskal.test(PbTx3_ros~Year,dat)

library(dunn.test)             
library(rcompanion)
with(dat,dunn.test(PbTx3_ros,Year))

DT.val=with(dat,dunn.test(PbTx3_ros,Year))

vars=c("comparisons","Z","P.adjusted")
data.frame(DT.val)[,vars]%>%
  flextable()%>%
  colformat_double(j=2,digits=2,big.mark = "")%>%
  colformat_double(j=3,digits=2,big.mark = "")%>%
  compose(j="P.adjusted",i=~P.adjusted<0.05,value=as_paragraph('< 0.05'))%>%
  compose(j="P.adjusted",i=~P.adjusted<0.01,value=as_paragraph('< 0.01'))%>%
  italic(j="P.adjusted",i=~P.adjusted<0.05)%>%
  set_header_labels("comparisons"="Pairwise\nComparisons",
                    "Z"="Z-score",
                    "P.adjusted"="\u03C1-value")%>%
  padding(padding=1.5,part="all")%>%
  align(j=2:3,part="all",align="center")%>%
  width(width=c(1,0.7,0.7))%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=10,part="all")%>%
  fontsize(size=12,part="header") #%>%print("docx")
  
DT.lts=toupper(rcompanion::cldList(P.adjusted ~ comparison,data=DT.val,threshold = 0.05)$Letter)
# png(filename=paste0(plot.path,"Maternal_PbTx_Year.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,0.5),oma=c(2,2,0.5,0.5),lwd=0.5);
ylim.val=c(0,10);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

x=boxplot(PbTx3_ros~Year,dat,outline=F,ylim=ylim.val,ann=F,axes=F)
dunn.letters(3,1:3,x$stats[5,],DT.lts,"red",1)
axis_fun(1,1:3,1:3,c(2019:2021),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2,"Plasma Brevetoxin (ng mL\u207B\u00B9)")
mtext(side=1,line=1.5,"Year")
dev.off()


####
## (2)	Determine the effects of red tide exposure on immune function and
## overall health in nesting loggerheads 
#### 

boxplot(PbTx3_ros~Hemolysi,subset(dat,Hemolysi%in%c(0,1)),outline=F)
kruskal.test(PbTx3_ros~Hemolysi,subset(dat,Hemolysi%in%c(0,1)))

boxplot(PbTx3_ros~Lipemia,subset(dat,Lipemia%in%c(0,1)),outline=F)
kruskal.test(PbTx3_ros~Lipemia,subset(dat,Lipemia%in%c(0,1)))

which(names.val=="PCV")
which(names.val=="Gamma.Gl")
which(names.val=="WBC.estimate.K_ul")
which(names.val=="Immature.RBC._round.stages_1.0.mature.RBC.in.patient.with.PCV.2.per")

str(dat[,42:80])

names.val[42:45]
names.val[42:80]
unique(dat$PCV)
unique(dat$TS)
unique(dat$Hemolysi); #factors 0, 1,2
unique(dat$Lipemia); #factors 0, 1,2

unique(dat$Alkaline)
dat$Alkaline[dat$Alkaline=="<20"]=10
dat$Alkaline[dat$Alkaline=="< 10"]=5
range(dat$AST,na.rm=T)
range(dat$CPK,na.rm=T)
unique(dat$Calcium)
unique(dat$Phosphor)
unique(dat$ca_phos)


library(corrplot)

vars=c("Amylase", 
       "Calcium", "Phosphor", "Choleste", 
       "Potassiu", "Mag", "Sodium", "Chloride", "CO2", "Glucose", 
       "BUN", "Total.Pr", "Triglyce", 
       "A_Gratio", "per.Prealb", "Pre-albu", "per.Albumi", "Albumin", 
       "perAlpha1", "Alpha.1", "per.Alpha2", "Alpha.2", "per.Beta.G", 
       "Beta.Glo", "per.Gamma", "Gamma.Gl")
dat[,vars]=sapply(dat[,vars],as.numeric)

summary(dat[,vars])
dat3=dat[,c("PbTx3_ros",vars)]

M=cor(na.omit(dat3))
corrplot(M, type="upper", col=c("black", "white"),
         bg="lightblue")

cor.mtest.pval <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  return(p.mat)
}
cor.mtest.est <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$estimate
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  return(p.mat)
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest.pval(dat3,method="spearman")
head(p.mat[, 1:5])

M=cor.mtest.est(dat3,method="spearman")


col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(4),  
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE,number.cex=0.5 
)



vars2=c("PCV", "TS", "Alkaline", "Amylase", 
        "AST", "CPK", "Calcium", "Phosphor", "Choleste", "GGT", 
        "Potassiu", "Lipase", "Mag", "Sodium", "Chloride", "CO2", "Glucose", 
        "BUN", "Anion.Ga", "Osmolali", "Uric.Aci", "Total.Pr", "Triglyce", 
        "A_Gratio", "per.Prealb", "Pre-albu", "per.Albumi", "Albumin", 
        "perAlpha1", "Alpha.1", "per.Alpha2", "Alpha.2", "per.Beta.G", 
        "Beta.Glo", "per.Gamma", "Gamma.Gl")
dat[,c("PbTx3_ros",vars2)]
dat[,vars2]=sapply(dat[,vars2],as.numeric)

M=cor.mtest.est(dat[,c("PbTx3_ros",vars2)],method="spearman")
p.mat <- cor.mtest.pval(dat[,c("PbTx3_ros",vars2)],method="spearman")

corrplot(M, method="color", col=col(4),  
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE,number.cex=0.5 
)


corrplot(M, type="upper", 
         p.mat = p.mat, sig.level = 0.05, insig = "blank",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         number.cex=0.75)
