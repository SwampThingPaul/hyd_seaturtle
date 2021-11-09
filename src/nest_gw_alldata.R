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

library(vegan)
library(REdaS)

#Paths
wd="C:/Julian_LaCie/_GitHub/hyd_seaturtle"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/GIS","/src/"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]


# -------------------------------------------------------------------------
dat=read.xlsx(paste0(data.path,"Cumulative Data.xlsx"),sheet=1);# all data
dat$Date.Time=date.fun(convertToDateTime(dat$Date.Time),form = "%F %R")
dat$Date=date.fun(dat$Date,form="%m/%d/%Y")
head(dat)
dat=rename(dat,c("Station.#"="Station","Nest.#"="Nest"))
colnames(dat)=c("Station", "Nest", "Location", "Date.Time", "Date", "Time", 
                "Date.Laid", "Incubation.(days)", "Incubation", "Date.Hatched", 
                "Rain", "Temp", "Water.Content", "Abs.Pres.psi", 
                "Temp.WLL", "Abs.Pres.Barom.psi", "Sensor.Depth.m", 
                "Well.Bottom.Elevation", "Water.Level.to.MSL.m", "Water.Level.Distance.from.Clutch.m", 
                "Sand.Surface.Elevation", "Chamber.Top.Elevation", "Temperature.Probe.Elevation", 
                "Moisture.Probe.Elevation", "Chamber.Bottom.Elevation", "Salinity", 
                "Measuring.Stick.Value", "Sensor.Depth.v.meas.stick")

boxplot(Water.Level.to.MSL.m~Location,dat)
boxplot(Water.Level.Distance.from.Clutch.m~Location,dat)
# subset(dat,Station==14)
da.dat=ddply(dat,c("Date","Station","Nest","Location"),
             summarise,
             mean.Temp=mean(Temp,na.rm=T),median.Temp=median(Temp,na.rm=T),
             min.Temp=min(Temp,na.rm=T),max.Temp=max(Temp,na.rm=T),
             delta.Temp=diff(range(Temp,na.rm = T)),
             mean.WC=mean(Water.Content,na.rm=T),median.WC=median(Water.Content,na.rm=T),
             delta.WC=diff(range(Water.Content,na.rm = T)),
             mean.WL.MSL=mean(Water.Level.to.MSL.m,na.rm=T),median.WL.MSL=median(Water.Level.to.MSL.m,na.rm=T),
             delta.WL.MSL=diff(range(Water.Level.to.MSL.m,na.rm = T)),
             mean.WL.CL=mean(Water.Level.Distance.from.Clutch.m,na.rm=T),median.WL.CL=median(Water.Level.Distance.from.Clutch.m,na.rm=T),
             delta.WL.CL=diff(range(Water.Level.Distance.from.Clutch.m,na.rm = T)),
             mean.tot.rain=mean(sum(Rain,na.rm=T)))
da.dat

subset(da.dat,delta.Temp>30)

boxplot(mean.Temp~Location,da.dat,outline=F)
boxplot(delta.Temp~Location,da.dat,outline=F)
boxplot(mean.WL.MSL~Location,da.dat,outline=F)
boxplot(delta.WL.MSL~Location,da.dat,outline=F)
boxplot(mean.WL.CL~Location,da.dat,outline=F)
boxplot(delta.WL.CL~Location,da.dat,outline=F);# might indicate Captiva has higher infiltration/exfiltration 

library(dunn.test)
library(rcompanion)
with(da.dat,dunn.test(mean.Temp,Location))

range(da.dat$mean.Temp,na.rm=T)
range(da.dat$delta.Temp,na.rm=T)

cols=viridis::cividis(3,alpha=0.5)
# png(filename=paste0(plot.path,"DailyCompare.png"),width=8,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,4,0.5,0.75),oma=c(4,1,1,0.5));
layout(matrix(c(1:6),2,3,byrow=F))
axis.lab.cex=0.8

ylim.val=c(25,38);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(mean.Temp~Location,da.dat,outline=F,col=cols,ylim=ylim.val,ann=F,axes=F)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:3,1:3,NA);box(lwd=1)
mtext(side=2,line=2.5,"Daily Mean Temp (\u00B0 C)",cex=axis.lab.cex)
temp.DT=with(da.dat,dunn.test(mean.Temp,Location))
temp.DT.ltr=cldList(P.adjusted ~ comparison,data=temp.DT,threshold = 0.05)
temp.DT.ltr$Letter=toupper(temp.DT.ltr$Letter)
text(1:5,x$stats[5,],temp.DT.ltr$Letter,pos=3)

ylim.val=c(0,3);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(delta.Temp~Location,da.dat,outline=F,col=cols,ylim=ylim.val,ann=F,axes=F)
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(1,1:3,1:3,c("Captiva","Sanibel\nEast","Sanibel\nWest"),line=-0.75,padj=1);box(lwd=1)
mtext(side=2,line=2.5,"\u0394 Mean Temp (\u00B0 C)",cex=axis.lab.cex)
temp.DT=with(da.dat,dunn.test(delta.Temp,Location))
temp.DT.ltr=cldList(P.adjusted ~ comparison,data=temp.DT,threshold = 0.05)
temp.DT.ltr$Letter=toupper(temp.DT.ltr$Letter)
text(1:5,x$stats[5,],temp.DT.ltr$Letter,pos=3)

ylim.val=c(-0.5,1.5);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(mean.WL.MSL~Location,da.dat,outline=F,col=cols,ylim=ylim.val,ann=F,axes=F)
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(1,1:3,1:3,NA);box(lwd=1)
mtext(side=2,line=2.5,"Daily Mean WL (m; MSL)",cex=axis.lab.cex)
WL.DT=with(da.dat,dunn.test(mean.WL.MSL,Location))
WL.DT.ltr=cldList(P.adjusted ~ comparison,data=WL.DT,threshold = 0.05)
WL.DT.ltr$Letter=toupper(WL.DT.ltr$Letter)
text(1:5,x$stats[5,],WL.DT.ltr$Letter,pos=3)

ylim.val=c(0,0.8);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(delta.WL.MSL~Location,da.dat,outline=F,col=cols,ylim=ylim.val,ann=F,axes=F)
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(1,1:3,1:3,c("Captiva","Sanibel\nEast","Sanibel\nWest"),line=-0.75,padj=1);box(lwd=1)
mtext(side=2,line=2.5,"\u0394 Mean Water Level (m)",cex=axis.lab.cex)
WL.DT=with(da.dat,dunn.test(delta.WL.MSL,Location))
WL.DT.ltr=cldList(P.adjusted ~ comparison,data=WL.DT,threshold = 0.05)
WL.DT.ltr$Letter=toupper(WL.DT.ltr$Letter)
text(1:5,x$stats[5,],WL.DT.ltr$Letter,pos=3)


ylim.val=c(-0.75,1.75);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(mean.WL.CL~Location,da.dat,outline=F,col=cols,ylim=ylim.val,ann=F,axes=F)
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(1,1:3,1:3,NA);box(lwd=1)
mtext(side=2,line=2.75,"Daily Mean WL to clutch (m; MSL)",cex=axis.lab.cex)
WL.DT=with(da.dat,dunn.test(mean.WL.CL,Location))
WL.DT.ltr=cldList(P.adjusted ~ comparison,data=WL.DT,threshold = 0.05)
WL.DT.ltr$Letter=toupper(WL.DT.ltr$Letter)
text(1:5,x$stats[5,],WL.DT.ltr$Letter,pos=3)

ylim.val=c(0,1);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(delta.WL.CL~Location,da.dat,outline=F,col=cols,ylim=ylim.val,ann=F,axes=F)
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(1,1:3,1:3,c("Captiva","Sanibel\nEast","Sanibel\nWest"),line=-0.75,padj=1);box(lwd=1)
mtext(side=2,line=2.75,"\u0394 Mean WL to clutch (m)",cex=axis.lab.cex)
WL.DT=with(da.dat,dunn.test(delta.WL.CL,Location))
WL.DT.ltr=cldList(P.adjusted ~ comparison,data=WL.DT,threshold = 0.05)
WL.DT.ltr$Letter=toupper(WL.DT.ltr$Letter)
text(1:5,x$stats[5,],WL.DT.ltr$Letter,pos=3)
mtext(side=1,outer=T,"Location",line=2.5)
dev.off()


# -------------------------------------------------------------------------
sand.dat=read.xlsx(paste0(data.path,"Sand Samples 2021.xlsx"),sheet=5);
colnames(sand.dat)=c("Sample", "Nest", "loc", "beach", "mean.grain", "sorting", 
  "skewness", "kurtosis", "compaction", "compaction.stdev", "bulk.density", 
  "VWC", "hatch.success", "emergence.success", "uni_bi", 
  "Sorting.description", "mean.description", "skewness.description", 
  "kurtosis.description", "Beach.Slope.i", "Beach.Slope.f", 
  "Beach.Slope.mean", "Chamber.Bottom.Elevation", "Coyote.depredation", 
  "Emergency.dig", "Avg.delta.Temp", "Avg.delta.WC", 
  "Avg.delta.WL.MSL", "Avg.delta.WL.cl", 
  "max.delta.temp", "max.delta.WC", 
  "max.delta.WL.MSL", "max.delta.WL.cl", 
  "dist.MHW","avg.nest.elev","Elevation")
sand.dat=subset(sand.dat,Coyote.depredation=="N")
vars=c("mean.grain", "sorting","skewness", "kurtosis", "compaction",
       "emergence.success","Beach.Slope.mean",
       "Avg.delta.Temp", "Avg.delta.WC", 
       "Avg.delta.WL.MSL", "Avg.delta.WL.cl",
       "dist.MHW","avg.nest.elev")

pca.dat=sand.dat[,c("Nest", "loc", "beach",vars)]

# Chamber top
pca.dat1=pca.dat#subset(pca.dat,loc=="Top")

# Data check
nrow(pca.dat1)
nrow(na.omit(pca.dat1))
pca.dat1=na.omit(pca.dat1)

# assumptions check
KMOS(pca.dat1[,vars])


vars2=c("mean.grain","sorting","compaction","bulk.density","hatch.success","Beach.Slope.mean",
       "max.delta.temp","max.delta.WL.cl","dist.MHW","avg.nest.elev")
# pca.dat=subset(sand.dat[,c("Nest", "loc", "beach",vars2)],loc=="Egg Chamber")
# pca.dat=subset(sand.dat[,c("Nest", "loc", "beach",vars2)],loc=="Top")
pca.dat=sand.dat[,c("Nest", "loc", "beach",vars2)]
pca.dat1=na.omit(pca.dat)
KMOS(pca.dat1[,vars2])
bart_spher(pca.dat1[,vars2])

plot(mean.grain~max.delta.WL.cl,pca.dat1)

# PCA
pca.dat1.pca=rda(pca.dat1[,vars2],scale=T)
#Extract eigenvalues (see definition above)
eig <- pca.dat1.pca$CA$eig
# Percent of variance explained by each compoinent
variance <- eig*100/sum(eig)
# The cumulative variance of each component (should sum to 1)
cumvar <- cumsum(variance)
# Combine all the data into one data.frame
eig.pca <- data.frame(eig = eig, variance = variance,cumvariance = cumvar)
eig.pca

layout(matrix(1:2,1,2))
par(family="serif",mar=c(1,2,0.1,1),oma=c(3,1,0.75,0.5));

ylim.val=c(0,5);by.y=1;ymaj=seq(ylim.val[1],100,by.y);ymin=seq(ylim.val[1],100,by.y/2)
x=barplot(eig.pca$eig,ylim=ylim.val,col="grey",yaxt="n")
abline(h=ymaj,lty=3,col="grey")
x=barplot(eig.pca$eig,ylim=ylim.val,col="grey",yaxt="n",add=T)
abline(h=1,lty=2,col="red",lwd=2)
axis_fun(1,line=-0.7,x,x,seq(1,length(x),1),0.7)
axis_fun(2,ymaj,ymin,ymaj,0.75);box(lwd=1)
mtext(side=1,line=1.5,"Principal Components")
mtext(side=2,line=1.5,"Eigenvalue")

ylim.val=c(0,110);by.y=25;ymaj=seq(ylim.val[1],100,by.y);ymin=seq(ylim.val[1],100,by.y/2);#set y limit and delineates the major and minor ticks
x=barplot(eig.pca$variance,ylim=ylim.val,col="white",border=0,yaxt="n")# inital plot to get the measurements
abline(h=ymaj,lty=3,col="grey")#makes vertical lines from y axis
x=barplot(eig.pca$variance,ylim=ylim.val,col="grey",yaxt="n",add=T)# the real plot that matters
lines(x,eig.pca$cumvariance,col="indianred1",lwd=2)# adds the cumulative variance for each factor
points(x,eig.pca$cumvariance,pch=21,bg="indianred1",cex=1.25)
abline(h=80,lty=2,col="red",lwd=2)
axis_fun(1,line=-0.7,x,x,seq(1,length(x),1),0.7)
axis_fun(2,ymaj,ymin,ymaj,0.75);box(lwd=1)
mtext(side=1,line=1.5,"Principal Components")
mtext(side=2,line=1.75,"Percentage of Variances")
legend.text=c("Absolute","Cumulative");#helper vaiable for legend
pt.col=c("grey","indianred1")#helper vaiable for legend
legend("topleft",legend=legend.text,pch=c(22,21),pt.bg=pt.col,col=c("black",pt.col[2]),lty=c(0,1),lwd=1.5,pt.cex=1.5,ncol=2,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,text.col="white")
legend("topleft",legend=legend.text,pch=c(22,21),pt.bg=pt.col,col="black",lty=0,lwd=0.5,pt.cex=1.55,ncol=2,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5)

plot(pca.dat1.pca)
# PCA
scrs <- scores(pca.dat1.pca,display=c("sites","species"),choices=c(1,2,3));

pca.dat1 <- cbind(pca.dat1,scrs$sites)
pca.dat1$loc=factor(pca.dat1$loc,levels=c("Egg Chamber","Top"))
pca.dat1$beach=factor(pca.dat1$beach,levels=c("Captiva","East","West"))


# png(filename=paste0(plot.path,"NestPCA.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,0.1,0.5),oma=c(2.5,1.5,0.75,0.5));
layout(matrix(1:2,1,2),widths=c(1,0.5))
# cols=wesanderson::wes_palette("Zissou1",3,"continuous")
cols=colorRampPalette(c("red","grey",'blue'))(3)
cols.vals=cols[pca.dat1$beach]
pchs=c(21,24)
pch.vals=pchs[pca.dat1$loc]

labs=rownames(scrs$species)
labs=c("Grain Size","Sorting","Compaction","BD","Hatch Succ.","Beach Slope","Max \u0394 Temp","Max \u0394 WL to clutch","Distance to MHW","Nest Elev")
# labs=c("Grain Size","Sorting","Compaction","BD","Emerg. Succ.","Beach Slope","Max \u0394 Temp","Max \u0394 WL to clutch","Distance to MHW","Nest Elev")
xlim.val=c(-2,2);by.x=1;xmaj=c(0,seq(xlim.val[1],xlim.val[2],by.x));xmin=seq(xlim.val[1],xlim.val[2],by.x/2);
ylim.val=c(-2,2);by.y=1;ymaj=c(0,seq(ylim.val[1],ylim.val[2],by.y));ymin=seq(ylim.val[1],ylim.val[2],by.y/2);
plot(xlim.val,ylim.val,type="n",yaxt="n",xaxt="n",ylab=NA,xlab=NA);
abline(h=0,v=0,lty=3,col="grey");
with(pca.dat1,points(PC1,PC2,pch=pch.vals,bg=adjustcolor(cols.vals,0.25),col=adjustcolor(cols.vals,0.5),lwd=0.1,cex=1.25))
arrows(0,0,scrs$species[,1],scrs$species[,2],length = 0.05, angle = 15, code = 2,col="indianred1",lwd=1.5);# makes the arrows
# with(scrs,text((species[,1]+0.15),species[,2],labels=labs,cex=0.75,font=3));#adds labels to the arrows; 
text(scrs$species[scrs$species[,1]<0,1],scrs$species[scrs$species[,1]<0,2],labels=labs[scrs$species[,1]<0],cex=0.75,font=3,pos=2,offset=0.01)
text(scrs$species[scrs$species[,1]>0,1],scrs$species[scrs$species[,1]>0,2],labels=labs[scrs$species[,1]>0],cex=0.75,font=3,pos=4,offset=0.01)
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj),1); #adds x axis ticks
axis_fun(2,ymaj,ymin,format(ymaj),1); #adds y axis ticks
mtext(side=1,line=1.8,paste0("PCA 1 (",round(eig.pca$variance[1],1),"%)"));#adds x axis label with percent variance
mtext(side=2,line=2.25,paste0("PCA 2 (",round(eig.pca$variance[2],1),"%)"));#adds y axis label with percent variance

plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)
legend(0.5,0.75,legend=c("Egg Chamber","Top"),
       pch=pchs,
       col=adjustcolor("grey",0.75),
       pt.bg=adjustcolor("grey",0.25),
       lwd=c(0.01,0.01),lty=c(NA),pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj = 0,title="Location")
legend(0.5,0.25,legend=c("Captiva","Sanibel East","Sanibel West"),
       pch=21,
       col=adjustcolor(cols,0.5),
       pt.bg=adjustcolor(cols,0.25),
       lwd=c(0.01,0.01,0.01),lty=c(NA),pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj = 0,title="Beach")
dev.off()


pca.dat.ch=subset(sand.dat[,c("Nest", "loc", "beach",vars2)],loc=="Egg Chamber")
pca.dat.ch=na.omit(pca.dat.ch)
pca.dat.ch.pca=rda(pca.dat.ch[,vars2],scale=T)
eig <- pca.dat.ch.pca$CA$eig
variance <- eig*100/sum(eig)
cumvar <- cumsum(variance)
eig.pca.ch <- data.frame(eig = eig, variance = variance,cumvariance = cumvar)
eig.pca.ch

pca.dat.top=subset(sand.dat[,c("Nest", "loc", "beach",vars2)],loc=="Top")
pca.dat.top=na.omit(pca.dat.top)
pca.dat.top.pca=rda(pca.dat.top[,vars2],scale=T)
eig <- pca.dat.top.pca$CA$eig
variance <- eig*100/sum(eig)
cumvar <- cumsum(variance)
eig.pca.top <- data.frame(eig = eig, variance = variance,cumvariance = cumvar)
eig.pca.top

scrs.ch <- scores(pca.dat.ch.pca,display=c("sites","species"),choices=c(1,2,3));
pca.dat.ch <- cbind(pca.dat.ch,scrs.ch$sites)
pca.dat.ch$beach=factor(pca.dat.ch$beach,levels=c("Captiva","East","West"))

scrs.top <- scores(pca.dat.top.pca,display=c("sites","species"),choices=c(1,2,3));
pca.dat.top <- cbind(pca.dat.top,scrs.top$sites)
pca.dat.top$beach=factor(pca.dat.top$beach,levels=c("Captiva","East","West"))

# png(filename=paste0(plot.path,"NestPCA_2.png"),width=8,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,0.1,0.5),oma=c(2.5,1.5,0.75,0.5));
layout(matrix(1:2,1,2),widths=c(1,1))
# cols=wesanderson::wes_palette("Zissou1",3,"continuous")
pchs=c(21,24)
labs=c("Grain Size","Sorting","Compaction","BD","Hatch Succ.","Beach Slope","Max \u0394 Temp","Max \u0394 WL to clutch","Distance to MHW","Nest Elev")
# labs=c("Grain Size","Sorting","Compaction","BD","Emerg. Succ.","Beach Slope","Max \u0394 Temp","Max \u0394 WL to clutch","Distance to MHW","Nest Elev")

cols.vals=cols[pca.dat.top$beach]
pch.vals=pchs[2]
xlim.val=c(-2,2);by.x=1;xmaj=c(0,seq(xlim.val[1],xlim.val[2],by.x));xmin=seq(xlim.val[1],xlim.val[2],by.x/2);
ylim.val=c(-2,2);by.y=1;ymaj=c(0,seq(ylim.val[1],ylim.val[2],by.y));ymin=seq(ylim.val[1],ylim.val[2],by.y/2);
plot(xlim.val,ylim.val,type="n",yaxt="n",xaxt="n",ylab=NA,xlab=NA);
abline(h=0,v=0,lty=3,col="grey");
with(pca.dat.top,points(PC1,PC2,pch=pch.vals,bg=adjustcolor(cols.vals,0.25),col=adjustcolor(cols.vals,0.5),lwd=0.1,cex=1.25))
arrows(0,0,scrs.top$species[,1],scrs.top$species[,2],length = 0.05, angle = 15, code = 2,col="indianred1",lwd=1.5);# makes the arrows
# with(scrs,text((species[,1]+0.15),species[,2],labels=labs,cex=0.75,font=3));#adds labels to the arrows; 
text(scrs.top$species[scrs.top$species[,1]<0,1],scrs.top$species[scrs.top$species[,1]<0,2],labels=labs[scrs.top$species[,1]<0],cex=0.75,font=3,pos=2,offset=0.01)
text(scrs.top$species[scrs.top$species[,1]>0,1],scrs.top$species[scrs.top$species[,1]>0,2],labels=labs[scrs.top$species[,1]>0],cex=0.75,font=3,pos=4,offset=0.01)
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj),1); #adds x axis ticks
axis_fun(2,ymaj,ymin,format(ymaj),1); #adds y axis ticks
mtext(side=1,line=1.8,paste0("PCA 1 (",round(eig.pca.top$variance[1],1),"%)"));#adds x axis label with percent variance
mtext(side=2,line=2.25,paste0("PCA 2 (",round(eig.pca.top$variance[2],1),"%)"));#adds y axis label with percent variance
mtext(side=3,adj=0,"Top Only")

cols.vals=cols[pca.dat.ch$beach]
pch.vals=pchs[1]
xlim.val=c(-2,2);by.x=1;xmaj=c(0,seq(xlim.val[1],xlim.val[2],by.x));xmin=seq(xlim.val[1],xlim.val[2],by.x/2);
ylim.val=c(-2,2);by.y=1;ymaj=c(0,seq(ylim.val[1],ylim.val[2],by.y));ymin=seq(ylim.val[1],ylim.val[2],by.y/2);
plot(xlim.val,ylim.val,type="n",yaxt="n",xaxt="n",ylab=NA,xlab=NA);
abline(h=0,v=0,lty=3,col="grey");
with(pca.dat.ch,points(PC1,PC2,pch=pch.vals,bg=adjustcolor(cols.vals,0.25),col=adjustcolor(cols.vals,0.5),lwd=0.1,cex=1.25))
arrows(0,0,scrs.ch$species[,1],scrs.ch$species[,2],length = 0.05, angle = 15, code = 2,col="indianred1",lwd=1.5);# makes the arrows
# with(scrs,text((species[,1]+0.15),species[,2],labels=labs,cex=0.75,font=3));#adds labels to the arrows; 
text(scrs.ch$species[scrs.ch$species[,1]<0,1],scrs.ch$species[scrs.ch$species[,1]<0,2],labels=labs[scrs.ch$species[,1]<0],cex=0.75,font=3,pos=2,offset=0.01)
text(scrs.ch$species[scrs.ch$species[,1]>0,1],scrs.ch$species[scrs.ch$species[,1]>0,2],labels=labs[scrs.ch$species[,1]>0],cex=0.75,font=3,pos=4,offset=0.01)
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj),1); #adds x axis ticks
axis_fun(2,ymaj,ymin,format(ymaj),1); #adds y axis ticks
mtext(side=1,line=1.8,paste0("PCA 1 (",round(eig.pca.ch$variance[1],1),"%)"));#adds x axis label with percent variance
mtext(side=2,line=2.25,paste0("PCA 2 (",round(eig.pca.ch$variance[2],1),"%)"));#adds y axis label with percent variance
mtext(side=3,adj=0,"Egg Chamber Only")
legend("bottomright",legend=c("Captiva","Sanibel East","Sanibel West"),
       pch=21,
       col=adjustcolor(cols,0.5),
       pt.bg=adjustcolor(cols,0.25),
       lwd=c(0.01,0.01,0.01),lty=c(NA),pt.cex=1,ncol=1,cex=0.5,bty="n",
       y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()

