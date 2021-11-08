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