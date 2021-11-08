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

#Paths
wd="C:/Julian_LaCie/_GitHub/hyd_seaturtle"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/GIS","/src/"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]

# usethis::use_readme_rmd(open = rlang::is_interactive()); #one and done

stickplot.arrows=function(DateTime,WSPD,WD,data,...){
  require(lubridate)
  #DateTime = Date and Time in POSIXct format (also works with just dates)
  #WSPD = Wind Speed in velocity units (i.e. m/s)
  #WD = Wind direction in degrees
  
  if(!missing(data)){
    DateTime=data[,deparse(substitute(DateTime))]
    WSPD=data[,deparse(substitute(WSPD))]
    WD=data[,deparse(substitute(WD))]
  }
  
  y=rep(0,length(DateTime))
  xend=DateTime + lubridate::dhours(WSPD * 1 * -cos((90-WD) / 360 * 2 * pi))
  yend = WSPD * 1 * -sin((90-WD) / 360 * 2 * pi)
  tmp=data.frame(DateTime=DateTime,y=y,xend=xend,yend=yend)
  arrows(DateTime,y,xend,yend,length=0,...)
}
# RECON Data ---------------------------------------------------------
recon.wave=read.csv(paste0(data.path,"wavebouy_RECON.csv"))
recon.wave$DateTime=date.fun(recon.wave$DateTime,form="%F %R")

colnames(recon.wave)<-c("DateTime","maxheight_ft","dir_deg","height_ft")
recon.wave$dum.speed=1# not real data

y=with(recon.wave,rep(0,length(DateTime)))
xend=with(recon.wave,DateTime + lubridate::dhours(dum.speed * 1 * -cos((90-dir_deg) / 360 * 2 * pi)))
yend =with(recon.wave, dum.speed * 1 * -sin((90-dir_deg) / 360 * 2 * pi))
tmp=data.frame(DateTime=recon.wave$DateTime,y=y,xend=xend,yend=yend)
with(tmp,arrows(DateTime,y,xend,yend,length=0))

par(family="serif",mar=c(1,3,0.5,0.5),oma=c(2,1,0.5,1),lwd=0.1);
layout(matrix(1:2,2,1))
plot(dum.speed~DateTime,recon.wave,ylim=c(-2,2),xlim=date.fun(c("2021-07-01","2021-07-10")),type="n")
with(tmp,arrows(DateTime,y,xend,yend,length=0))

plot(dir_deg~DateTime,recon.wave,xlim=date.fun(c("2021-07-01","2021-07-10")))
plot(ft.to.m(height_ft)~DateTime,recon.wave,xlim=date.fun(c("2021-07-01","2021-07-10")))


GOM.wx=read.csv(paste0(data.path,"GOM_RECON.csv"))
GOM.wx$DateTime=date.fun(GOM.wx$DateTime,form="%F %R")
colnames(GOM.wx)<-c("DateTime","curspd","WSPD","WD","surf_curspd","bot_curspd","wave_dir","maxheight_ft","height_ft")
GOM.wx$dum.speed=1# not real data

plot(height_ft~DateTime,GOM.wx)
plot(WSPD~DateTime,GOM.wx,ylim=c(-max(GOM.wx$WSPD),max(GOM.wx$WSPD)),xlim=date.fun(c("2021-07-01","2021-07-10")),type="n")
stickplot.arrows(DateTime,WSPD,WD,data=GOM.wx)

plot(dum.speed~DateTime,GOM.wx,ylim=c(-2,2),xlim=date.fun(c("2021-07-01","2021-07-10")),type="n")
stickplot.arrows(DateTime,WSPD=dum.speed,WD=wave_dir,data=GOM.wx)


# Tide data (NOAA) --------------------------------------------------------
# Tide predictions no observed tide
# library(rtide)
# tide.data <- rtide::tide_height(
#   "Fort Myers",
#   from = as.Date("2021-07-01"), to = as.Date("2021-07-10"),
#   minutes = 15L, tz = "EST"
# )
# plot(TideHeight~DateTime,tide.data)

##
# tide.dat=read.csv("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?product=water_level&application=NOS.COOPS.TAC.WL&begin_date=20210701&end_date=20210710&datum=MLLW&station=8725520&time_zone=LST&units=english&format=csv")
tide.dat=read.csv("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?product=water_level&application=NOS.COOPS.TAC.WL&begin_date=20210701&end_date=20210710&datum=MLLW&station=8725110&time_zone=LST&units=english&format=csv")
tide.dat$Date.Time=date.fun(tide.dat$Date.Time,form="%F %R")
tide.dat$Water.Level.m=ft.to.m(tide.dat$Water.Level)
head(tide.dat)
plot(Water.Level.m~Date.Time,tide.dat)

# Nest Data ---------------------------------------------------------------
library(openxlsx)
n247=read.xlsx(paste0(data.path,"N247 Station 8.xlsx"),sheet=1)
# n247$Date.Time=date.fun(convertToDateTime(n247$Date.Time,form="%F %R"))
n247$Date.Laid=date.fun(convertToDateTime(n247$Date.Laid))
n247$Date.Hatched=date.fun(convertToDateTime(n247$Date.Hatched))

vars=c("Date.Time", "Date", "Time", "Date.Laid", "Incubation.(days)", 
       "Incubation", "Date.Hatched", "WC", "Temp", 
       "Abs.Pres", "WLL.Temp", "Abs.Pres.Barom", "Sensor.Depth", 
       "Well.Bottom.Elevation", "Water.Level.to.MSL", "Sand.Surface.Elevation", 
       "Chamber.Top.Elevation", "Temperature.Probe.Elevation", "Moisture.Probe.Elevation", 
       "Chamber.Bottom.Elevation", "Salinity")
colnames(n247)<-vars
n247$Date.Time=with(n247,date.fun(paste(Date, Time),form="%m/%d/%Y %R"))
n247$z.WL=with(n247,Sensor.Depth-Sand.Surface.Elevation)


plot(Sensor.Depth~Date.Time,n247,xlim=date.fun(c("2021-07-05","2021-07-08")),type="l")

plot(z.WL~Date.Time,n247,xlim=date.fun(c("2021-07-05","2021-07-08")),type="l",yaxs="i",ylim=c(-1.6,2))
abline(h=(1.016-1.471)); #chamber bottom (chamber bottom - sand surf)
abline(h=(1.081-1.471),lty=2); #chamber top (chamber top - sand surf)
with(tide.dat,lines(Water.Level.m-1.471~Date.Time))
# with(recon.wave,lines((ft.to.m(height_ft)-1.471)~DateTime,col="blue"))


# GW infiltration
## Based on medium grain sand 9e-7 to 5e-4 m/sec
## coarse sand 9e-7 to 6e-3 m/sec
## m/sec to m/day 1.1574e-5
ms.to.md=function(x) x*86400
k.val=ms.to.md(9e-7)
nest.area=pi*(0.6096^2) # assumes nest 4 ft across
n247$Qgw=with(n247,k.val*nest.area*((Sensor.Depth-Chamber.Bottom.Elevation)/(Chamber.Top.Elevation-Chamber.Bottom.Elevation)))

plot(Qgw~Date.Time,n247,xlim=date.fun(c("2021-07-05","2021-07-08")),type="l")

## RF Data
RF.dat=read.xlsx(paste0(data.path,"N375 rain.xlsx"),sheet=1)
RF.dat$Date.Time=date.fun(convertToDateTime(RF.dat$Date.Time),form="%F %R")
colnames(RF.dat)<-c("Date.Time","RF.inch")
RF.dat$RF.cm=in.to.cm(RF.dat$RF.inch)


with(RF.dat,lines(RF.cm~Date.Time))
