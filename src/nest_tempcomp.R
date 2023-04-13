## 
## Nest temp loggers
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
dat=read.xlsx(paste0(data.path,"Cumulative Data 2022 (Good Ones).xlsx"),sheet=1);# all data
dat$Date.Time=convertToDateTime(dat$Date.Time)
dat$Date=date.fun(convertToDate(dat$Date))
dat$Temp.C.WLL=with(dat,ifelse(Temp.C.WLL<0,NA,Temp.C.WLL))

# Temp WLL - temperature probe at water well
# Temp - Temp in nest

vars=c("Station", "Nest", "Location", "Date.Time", "Date", "Time", 
  "Date.Laid", "Incubation.days", "Incubation", "Date.Hatched", 
  "Rain.mm", "Temp.C", "Water.Content", "Abs.Pres.psi", 
  "Temp.C.WLL", "Abs.Pres.Barom.psi", "Sensor.Depth.m", 
  "Well.Bottom.Elevation", "Water.Level.to.MSL.m", "Water.Level.Distance.from.Clutch.m", 
  "Sand.Surface.Elevation", "Chamber.Top.Elevation", "Temperature.Probe.Elevation", 
  "Moisture.Probe.Elevation", "Chamber.Bottom.Elevation", "Salinity", 
  "Measuring.Stick.Value", "Sensor.Depth.v.meas.stick")
colnames(dat)=vars

tmp=ddply(subset(dat,Temp.C.WLL>0),
          c("Station","Nest","Date","Incubation.days"),summarise,
          Temp.C.WLL=mean(Temp.C.WLL,na.rm=T),
          Temp.C=mean(Temp.C,na.rm=T))


plot(Temp.C.WLL~Temp.C,subset(dat,Temp.C.WLL>0));abline(0,1)

plot(Temp.C.WLL~Temp.C,tmp);abline(0,1)


unique(tmp$Nest)

plot(Temp.C~Incubation.days,tmp)

plot(Temp.C.WLL~Temp.C,subset(tmp,Nest==112));abline(0,1)
plot(Temp.C.WLL~Temp.C,subset(tmp,Nest==86));abline(0,1)
plot(Temp.C.WLL~Temp.C,subset(tmp,Nest==1));abline(0,1)
# abline(lm(Temp.C.WLL~Temp.C,subset(tmp,Nest==1)))
plot(Temp.C.WLL~Temp.C,subset(tmp,Nest==104));abline(0,1)


plot(Temp.C.WLL~Temp.C,subset(dat,Nest==112));abline(0,1)
plot(Temp.C.WLL~Temp.C,subset(dat,Nest==86));abline(0,1)
plot(Temp.C.WLL~Temp.C,subset(dat,Nest==1));abline(0,1)
plot(Temp.C.WLL~Temp.C,subset(dat,Nest==104));abline(0,1)
