## 
## Nest Groundwater evaluation
## 
##
## Code was compiled by Paul Julian
## contact info: pjulian.sccf.org

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

#Libraries
library(AnalystHelper);#devtools::install_github("SwampThingPaul/AnalystHelper")
library(plyr)
library(reshape2)
library(openxlsx)

#Paths
wd="C:/Julian_LaCie/_GitHub/hyd_seaturtle"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/GIS","/src/"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]


# -------------------------------------------------------------------------
dat=read.xlsx(paste0(data.path,"Sand Samples 2021.xlsx"),sheet=2);# all data

vars=c("Nest","Beach","Sample.Depth",
       "4.75.mm.Actual.Weight",
       "2.36.mm.Actual.Weight",
       "1.18.mm.Actual.Weight",
       "600.um.Actual.Weight",
       "300.um.Actual.Weight",
       "150.um.Actual.Weight",
       "75.um.Actual.Weight",
       "38.um.Actual.Weight",
       "<.38.um.Actual.Weight")
dat2=dat[,vars]

dat2.melt=melt(dat2,id.vars=vars[1:3])

x.walk=data.frame(variable=c(
"4.75.mm.Actual.Weight",
"2.36.mm.Actual.Weight",
"1.18.mm.Actual.Weight",
"600.um.Actual.Weight",
"300.um.Actual.Weight",
"150.um.Actual.Weight",
"75.um.Actual.Weight",
"38.um.Actual.Weight"
),
grainsize.mm=c(
  4.75,2.36,1.18,
  c(600,300,150,75,38)*0.001)
)

dat2.melt=merge(dat2.melt,x.walk,"variable")

dat2.melt=merge(dat2.melt,
                ddply(dat2.melt,c("Nest","Beach","Sample.Depth"),summarise,Twgt=sum(value,na.rm=T)),
                c("Nest","Beach","Sample.Depth"))
dat2.melt$percent=with(dat2.melt,(value/Twgt)*100)
dat2.melt$Log10_gainsize=log10(dat2.melt$grainsize.mm)

tmp=subset(dat2.melt,Nest=="N121"&Beach=="East"&Sample.Depth=="Egg Chamber")
tmp=subset(dat2.melt,Nest=="N121"&Beach=="East"&Sample.Depth=="Top ")
tmp=tmp[order(tmp$grainsize.mm),]
tmp$cumPer=cumsum(tmp$percent)
tmp

plot(cumPer~grainsize.mm,tmp,log="x",type="b")
tmp.d10=10^approx(x = tmp$cumPer, y = log10( tmp$grainsize.mm), xout = 10)$y
lines(c(0.001,tmp.d10),c(10,10),col="red",lty=2)
lines(c(tmp.d10,tmp.d10),c(-10,10),col="red",lty=2)
text(tmp.d10,10,"d10",pos=4)
tmp.d50=10^approx(x = tmp$cumPer, y = log10( tmp$grainsize.mm), xout = 50)$y
lines(c(0.001,tmp.d50),c(50,50),col="red",lty=2)
lines(c(tmp.d50,tmp.d50),c(-10,50),col="red",lty=2)
text(tmp.d50,50,"d50",pos=4)

(tmp.d50-tmp.d10)/40

# tmp.dat=data.frame(xval=c(tmp.d10,tmp.d50),yval=c(10,50))
# tmp.dat$log.xval=log10(tmp.dat$xval)
# lm.test=lm(yval~log.xval,tmp.dat)
# test2=predict(lm.test,data.frame(log.xval=log10(c(0.05,0.20))))
# lines(c(0.05,0.20),test2,col="red")

lm.test=lm(cumPer~Log10_gainsize,subset(tmp, grainsize.mm%in%c(0.075,0.150)))
test2=predict(lm.test,data.frame(Log10_gainsize=log10(c(0.05,0.20))))
# lm.test=lm(cumPer~grainsize.mm,subset(tmp, grainsize.mm%in%c(0.075,0.150)))
# test2=predict(lm.test,data.frame(grainsize.mm=c(0.05,0.20)))
lines(c(0.05,0.20),test2,col="red")
I0=as.numeric(10^((0-coef(lm.test)[1])/coef(lm.test)[2]))

1300*(I0+(0.025*(tmp.d50-tmp.d10)))^2


## 
dat2.melt$Sample.Depth=trimws(dat2.melt$Sample.Depth)
dat2.melt$nest_id_loc=with(dat2.melt,paste(Nest,Beach,Sample.Depth,sep="_"))

grainsize.dat=data.frame()
ids=unique(dat2.melt$nest_id_loc)
plot.grain=FALSE
for(i in 1:length(ids)){
  tmp=subset(dat2.melt,nest_id_loc==ids[i])
  tmp=tmp[order(tmp$grainsize.mm),]
  tmp$cumPer=cumsum(tmp$percent)
  
  tmp.d10=10^approx(x = tmp$cumPer, y = log10( tmp$grainsize.mm), xout = 10)$y
  tmp.d50=10^approx(x = tmp$cumPer, y = log10( tmp$grainsize.mm), xout = 50)$y
  lm.test=lm(cumPer~Log10_gainsize,subset(tmp, grainsize.mm%in%c(0.075,0.150)))
  I0=as.numeric(10^((0-coef(lm.test)[1])/coef(lm.test)[2]))
  
  K.val=1300*(I0+(0.025*(tmp.d50-tmp.d10)))^2
  
  if(plot.grain==T){
  plot(cumPer~grainsize.mm,tmp,log="x",type="b")
  lines(c(0.001,tmp.d10),c(10,10),col="red",lty=2)
  lines(c(tmp.d10,tmp.d10),c(-10,10),col="red",lty=2)
  text(tmp.d10,10,"d10",pos=4)
  lines(c(0.001,tmp.d50),c(50,50),col="red",lty=2)
  lines(c(tmp.d50,tmp.d50),c(-10,50),col="red",lty=2)
  text(tmp.d50,50,"d50",pos=4)
  
  test2=predict(lm.test,data.frame(Log10_gainsize=log10(c(0.05,0.20))))
  lines(c(0.05,0.20),test2,col="red")
  }
  
  pg <- 100 - tmp$cumPer[match(4.75, round(tmp$grainsize.mm, 2))]
  pf <- tmp$cumPer[match(0.075, round(tmp$grainsize.mm, 3))]
  ps <- 100 - pg - pf
  
  rslt=data.frame(nest_id_loc=ids[i],
                  d50=tmp.d50,
                  d10=tmp.d10,
                  I0=I0,
                  K=K.val,
                  Percent.Sand=ps
  )
  grainsize.dat=rbind(grainsize.dat,rslt)
}
grainsize.dat

sp.val=strsplit(grainsize.dat$nest_id_loc,"_")
grainsize.dat$nest=sapply(sp.val,"[",1)
grainsize.dat$Beach=sapply(sp.val,"[",2)
grainsize.dat$Sample.Depth=sapply(sp.val,"[",3)

vars=c("nest","Beach", "Sample.Depth", "d50", "d10", "I0", "K")
grainsize.dat=grainsize.dat[,vars]

boxplot(K~Sample.Depth+Beach,grainsize.dat)
kruskal.test(K~Sample.Depth,grainsize.dat)

boxplot(d10~Sample.Depth,grainsize.dat)
kruskal.test(d10~Sample.Depth,grainsize.dat)

boxplot(K~Sample.Depth+Beach,subset(grainsize.dat,Beach!="Sample"))

# write.csv(grainsize.dat,paste0(export.path,"K_Calc.csv"),row.names=F)

grainsize.dat2=subset(grainsize.dat,Beach!="Sample")
grainsize.dat2$Beach=factor(grainsize.dat2$Beach,levels=c("Captiva",'West',"East"))
grainsize.dat2$Depth_Beach=with(grainsize.dat2,paste(Beach,Sample.Depth,sep="_"))

x=boxplot(d50~Sample.Depth+Beach,grainsize.dat2)

library(dunn.test)
DT=with(grainsize.dat2,dunn.test(K,Depth_Beach))
DT.ltr=rcompanion::cldList(P.adjusted ~ comparison,data=DT,threshold = 0.05)
DT.ltr$Letter=toupper(DT.ltr$Letter)

levels.var=c(paste("Captiva",c("EggChamber","Top"),sep="_"),
             paste("West",c("EggChamber","Top"),sep="_"),
             paste("East",c("EggChamber","Top"),sep="_"))
DT.ltr=DT.ltr[order(match(DT.ltr$Group,levels.var)),]

cols=sort(rep(adjustcolor(wesanderson::wes_palette("Zissou1",3,"continuous"),0.5),2))
# png(filename=paste0(plot.path,"K_compare.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3.5,0.5,0.75),oma=c(3,2,0.5,0.5));
ylim.val=c(5,10);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

x=boxplot(K~Sample.Depth+Beach,grainsize.dat2,outline=F,ylim=ylim.val,axes=F,ann=F,col=cols)
abline(v=c(2.5,4.5),col="grey",lty=2)
axis_fun(1,1:6,1:6,rep(c("Chamber","Top"),3),line=-0.5)
axis(1,c(1.5,3.5,5.5),c("Captiva","West","East"),line=0.5,lty=0)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
text(1:6,x$stats[5,],DT.ltr$Letter,pos=3,col="red")
mtext(side=2,line=2.5,"Hydraulic Conductivity (m d\u207B\u00B9)")
mtext(side=1,line=2.5,"Depth & Beach")
dev.off()

#### Exploring data analysis


library(G2Sd)
granstat(tmp$percent)

quantile(tmp$percent,probs=0.5)
quantile(tmp$cumPer,probs=0.5)-quantile(tmp$cumPer,probs=0.1)

data(granulo) 
data(coord_gran) 
result=granstat(granulo) 
granplot(granulo,1)


library(geotech)
# https://rdrr.io/cran/geotech/man/grainSize.html
##  Example 1:  Grain-size distribution

##  (a) Define data
sieve.example <- c(3/8, 4, 10, 20, 40, 140, 200)
percent.example <- c(95.72, 90.23, 81.49, 66.36, 50.00, 8.51, 4.82)

##  (b) Percent gravel, sand, and fines
percentComponents(sieve = sieve.example, percent = percent.example,
                  metric = TRUE)

percentComponents(size = tmp$grainsize.mm, percent = tmp$cumPer,
                  metric = TRUE)

##  (c) Plot grain-size distribution
grainSize.plot(sieve = sieve.example, percent = percent.example,
               metric = TRUE)

grainSize.plot(size = tmp$grainsize.mm*1000, percent = tmp$cumPer,
               metric = TRUE)

##  (d) Calculate D50
Dsize(N = 50, sieve = sieve.example, percent = percent.example,
      metric = TRUE)

Dsize(N = 50, size = tmp$grainsize.mm, percent = 100-tmp$cumPer,
      metric = TRUE)
Dsize(N = 10, size = tmp$grainsize.mm, percent = 100-tmp$cumPer,
      metric = TRUE)


##  (e) Coefficients of uniformity and curvature
grainSize.coefs(sieve = sieve.example, percent = percent.example)

grainSize.coefs(size = tmp$grainsize.mm*1000, percent = 100-tmp$cumPer)

##  Example 2:  coefficients of uniformity and curvature
grainSize.coefs(D60 = 0.10, D30 = 0.03, D10 = 0.002)  
