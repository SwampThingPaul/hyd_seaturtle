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

### 
logo=png::readPNG(paste0(wd,"/horiz_SCCF_Logo.png"))
# logo=png::readPNG(paste0(wd,"/Logo no Background.png"))

# -------------------------------------------------------------------------

dat=read.xlsx(paste0(data.path,"Shalmiar_WLL_Final.xlsx"))
colnames(dat)=c("rownum","datetime","abs_press","temp","depth.m","datetime2","depth.ft")
dat$datetime=date.fun(convertToDateTime(dat$datetime),form="%F %R",tz="America/New_York")

with(subset(dat,depth.ft>4),diff(range(datetime)))


# png(filename=paste0(plot.path,"Shalmiar_WL.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,0.5),oma=c(2,2.5,0.5,0.25));

bk.val=c(-1,1.75,4,6,8,10,12)
bks=findInterval(dat$depth.ft,bk.val)
cols=viridisLite::rocket(length(bk.val),alpha=0.5)# viridisLite::viridis(length(bk.val),option="E")

land.fall=date.fun("2022-09-28 15:05",form="%F %R",tz="America/New_York")
ylim.val=c(0,12);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2022-09-22","2022-10-02"),tz="America/New_York");xmaj=seq(xlim.val[1],xlim.val[2],"2 days");xmin=seq(xlim.val[1],xlim.val[2],"1 days")

plot(depth.ft~datetime,dat,xlim=xlim.val,ylim=ylim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
lines(depth.ft~datetime,dat,col="grey",lwd=2)
points(depth.ft~datetime,dat,pch=21,bg=cols[bks],col=cols[bks])
abline(v=land.fall)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d"),line=-0.5)
box(lwd=1)

arrows(xlim.val[1],2,date.fun("2022-09-27"),2,angle=90,length=0.05,code=3)
text(date.fun(xlim.val[1]+lubridate::ddays(2.5),form="%F %R"),2,"Normal Tidal Fluctuations",pos=3,cex=0.8,font=3)
text(land.fall,max(dat$depth.ft,na.rm=T),"Landfall",pos=4,cex=0.8,font=3)

mtext(side=2,line=2,"Water Level (Feet, Mean Sea Level)")
mtext(side=1,line=1.5,"Date (M-D-2022)")
dev.off()



# png(filename=paste0(plot.path,"Shalmiar_WL_square.png"),width=5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,0.5),oma=c(2,2.5,0.5,0.25));

bk.val=c(-1,1.75,4,6,8,10,12)
bks=findInterval(dat$depth.ft,bk.val)
cols=viridisLite::rocket(length(bk.val),alpha=0.5)# viridisLite::viridis(length(bk.val),option="E")

land.fall=date.fun("2022-09-28 15:05",form="%F %R",tz="America/New_York")
ylim.val=c(0,12);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2022-09-22","2022-10-02"),tz="America/New_York");xmaj=seq(xlim.val[1],xlim.val[2],"2 days");xmin=seq(xlim.val[1],xlim.val[2],"1 days")

plot(depth.ft~datetime,dat,xlim=xlim.val,ylim=ylim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
lines(depth.ft~datetime,dat,col="grey",lwd=2)
points(depth.ft~datetime,dat,pch=21,bg=cols[bks],col=cols[bks])
abline(v=land.fall)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d"),line=-0.5)
box(lwd=1)

arrows(xlim.val[1],2,date.fun("2022-09-27"),2,angle=90,length=0.05,code=3)
text(date.fun(xlim.val[1]+lubridate::ddays(2.5),form="%F %R"),2,"Normal Tidal Fluctuations",pos=3,cex=0.8,font=3)
text(land.fall,max(dat$depth.ft,na.rm=T),"Landfall",pos=4,cex=0.8,font=3)

mtext(side=2,line=2,"Water Level (Feet, Mean Sea Level)")
mtext(side=1,line=1.5,"Date (M-D-2022)")
grid::grid.raster(logo,x=0.17,y=0.85,just=c("left","bottom"),width=grid::unit(1.75,"inches"))
dev.off()


w.val=6.5
h.val=(w.val)*(9/16)
# png(filename=paste0(plot.path,"Shalmiar_WL_16x9.png"),width=w.val,height=h.val,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,0.5),oma=c(2,2.5,0.5,0.25));

bk.val=c(-1,1.75,4,6,8,10,12)
bks=findInterval(dat$depth.ft,bk.val)
cols=viridisLite::rocket(length(bk.val),alpha=0.5)# viridisLite::viridis(length(bk.val),option="E")

land.fall=date.fun("2022-09-28 15:05",form="%F %R",tz="America/New_York")
ylim.val=c(0,12);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2022-09-22","2022-10-02"),tz="America/New_York");xmaj=seq(xlim.val[1],xlim.val[2],"2 days");xmin=seq(xlim.val[1],xlim.val[2],"1 days")

plot(depth.ft~datetime,dat,xlim=xlim.val,ylim=ylim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
lines(depth.ft~datetime,dat,col="grey",lwd=2)
points(depth.ft~datetime,dat,pch=21,bg=cols[bks],col=cols[bks])
abline(v=land.fall)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d"),line=-0.5)
box(lwd=1)

arrows(xlim.val[1],2,date.fun("2022-09-27"),2,angle=90,length=0.05,code=3)
text(date.fun(xlim.val[1]+lubridate::ddays(2.5),form="%F %R"),2,"Normal Tidal Fluctuations",pos=3,cex=0.8,font=3)
text(land.fall,max(dat$depth.ft,na.rm=T),"Landfall",pos=4,cex=0.8,font=3)

mtext(side=2,line=2,"Water Level (Feet, Mean Sea Level)")
mtext(side=1,line=1.5,"Date (M-D-2022)")

grid::grid.raster(logo,x=0.14,y=0.8,just=c("left","bottom"),width=grid::unit(1.75,"inches"))
dev.off()