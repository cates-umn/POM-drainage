#anna's updated data look script
#calculate recovery and check data
#graph OC and IC by g/g fraction or % and g/kg soil
#figure out which is the better response variable?
#run models

library(ggplot2)
library(Rmisc)
library(magrittr)
library(dplyr)
library(multcomp)
library(ggpubr)
library(emmeans)
library(multcompView)
library(lme4)
library(car)
library(nlme)


#read in data####

SOMData = read.delim("SOMData.csv", header=TRUE, sep = ",")
str(SOMData)

# Set variables to factors
SOMData$Depth.cm <- as.factor(SOMData$Depth.cm)
SOMData$Block <- as.factor(SOMData$Block)
SOMData$Treatment <- as.factor(SOMData$Treatment)
SOMData$Year<- as.factor(SOMData$Year)
SOMData$Section <- as.factor(SOMData$Section)
#calculating mass of TOC in POM and MAOM####

#steps
#multiply POM % by POM mass percent to get g POM-OC/g soil
#repeat for MAOM
#add POM-OC + MAOM OC for recovered OC %
#and check recovery with bulk OC: reocvered OC/bulk OC


#multiply POM % by POM mass percent to get g POM-OC/g soil
ggdensity(SOMData$Pom.TOC.percent)
SOMData$POM.OC.gsoil= (SOMData$Pom.TOC.percent/100)*(SOMData$Pom_percent/100)
ggdensity(SOMData$POM.OC.gsoil)
SOMData$MAOM.OC.gsoil= (SOMData$Maom.TOC.percent/100)*(SOMData$Maom_percent/100)
ggdensity(SOMData$MAOM.OC.gsoil)

#add POM-OC + MAOM OC for recovered OC %
SOMData$OC.rec = SOMData$MAOM.OC.gsoil+SOMData$POM.OC.gsoil
ggdensity(SOMData$OC.rec)

#and check recovery with bulk OC: reocvered OC/bulk OC
SOMData$OC.rec.percent = SOMData$OC.rec/(SOMData$Bulk.TOC.percent/100)

ggplot(SOMData,
       aes(y= OC.rec,
           x = Bulk.TOC.percent/100,
           color = Treatment,
           shape = Depth.cm,
           size = Pom.TOC.percent))+
  geom_point()+
  geom_abline(slope=1, intercept =0)


cor.test(SOMData$OC.rec, SOMData$Bulk.TOC.percent/100)
#correlation is pretty good. 
#I'd say the data at the surface pulls a little higher towards the recovered OC,
#maybe a little bias towards samples with higher %POM-OC having higher OC.rec
#that makes sense


#calculating mass of TIC in POM and MAOM####

#steps
#multiply POM % by POM mass percent to get g POM-IC/g soil
#repeat for MAOM
#add POM-OC + MAOM IC for recovered OC %
#and check recovery with bulk IC: reocvered IC/bulk IC

#multiply POM % by POM mass percent to get g POM-IC/g soil
ggdensity(SOMData$Pom.TIC.percent)
SOMData$POM.IC.gsoil= (SOMData$Pom.TIC.percent/100)*(SOMData$Pom_percent/100)
ggdensity(SOMData$POM.IC.gsoil)
SOMData$MAOM.IC.gsoil= (SOMData$Maom.TIC.percent/100)*(SOMData$Maom_percent/100)
ggdensity(SOMData$MAOM.IC.gsoil)

#add POM-IC + MAOM IC for recovered IC %
SOMData$IC.rec = SOMData$MAOM.IC.gsoil+SOMData$POM.IC.gsoil
ggdensity(SOMData$IC.rec)

#and check recovery with bulk OC: reocvered OC/bulk OC
SOMData$IC.rec.percent = SOMData$IC.rec/(SOMData$Bulk.TIC.percent/100)
ggdensity(SOMData$IC.rec.percent)

ggplot(SOMData,
       aes(y= IC.rec,
           x = Bulk.TIC.percent/100,
           color = Treatment,
           shape = Depth.cm,
           size = Pom.TIC.percent,
           linetype = interaction(Treatment, Depth.cm)))+
  geom_point()+
  geom_smooth(method = "lm")
# geom_abline(slope=1, intercept =0)


cor.test(SOMData$IC.rec, SOMData$Bulk.TIC.percent/100)
#here the values trend more distinctly lower, as in, the two fractions
#add up to less than the bulk soil IC.

write.csv(SOMData, "SOM Data 1.12.24.csv", row.names = FALSE)

#start here with "full data"####

somdata = read.csv("SOM Data 1.12.24.csv", header = TRUE)

somdata$Depth.cm <- as.factor(somdata$Depth.cm)
somdata$Block <- as.factor(somdata$Block)
somdata$Treatment <- as.factor(somdata$Treatment)
somdata$Year<- as.factor(somdata$Year)
somdata$Section <- as.factor(somdata$Section)

#mass separation figs####

Pompersum <- summarySE(somdata, measurevar = "Pom_percent", groupvars = c("Depth.cm", "Year", "Treatment"))

PomperDatasum <- within(Pompersum, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))  #flip axis

#your plot w/ Depth.cm on y-axis will essentially be flipped-

ggplot(PomperDatasum,
       aes(x=Pom_percent,
           y=as.factor(Depth.cm), 
           shape = Year,
           color = Treatment))+
  theme_bw()+
  geom_errorbar(aes(xmin=Pom_percent-se, xmax=Pom_percent+se), 
                colour="black", width=.1, position=position_jitter(seed=100))+
  #I think this "width" argument removes the horizontal lines at
  #the end of the error bar- not sure
  geom_point(size=7, position = position_jitter(seed=100)) +
  
  #you can futz with these themes and see if you like them. 
  theme(#panel.grid.major = element_blank(), #removes gridlines
    #panel.grid.minor = element_blank(), #removes gridlines
    panel.background = element_blank(), #removes gridlines
    axis.line = element_line(colour = "black"), 
    axis.text.x = element_text(size=16), #size of x axis text
    axis.text.y = element_text(size=16), #size of y axis text
    axis.title = element_text(size= 21), # size of title text
    legend.title= element_text(size = 15), #size of legend title
    legend.text = element_text(size = 15), #size of legend text
    plot.title = element_text(hjust=0.5, size=24)) #size of plot title and centered




#IC figs####
PomICsum <- summarySE(somdata, measurevar = "Pom.TIC.percent", groupvars = c("Depth.cm", "Year", "Treatment"))

PomICDatasum <- within(PomICsum, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))  #flip axis

#your plot w/ Depth.cm on y-axis will essentially be flipped-

ggplot(PomICDatasum,
       aes(x=Pom.TIC.percent,
           y=as.factor(Depth.cm), 
           shape = Year,
           color = Treatment))+
  theme_bw()+
  geom_errorbar(aes(xmin=Pom.TIC.percent-se, xmax=Pom.TIC.percent+se), 
                colour="black", width=.1, position=position_jitter(seed=100))+
  #I think this "width" argument removes the horizontal lines at
  #the end of the error bar- not sure
  geom_point(size=7, position=position_jitter(seed=100)) +
  
  #you can futz with these themes and see if you like them. 
  theme(#panel.grid.major = element_blank(), #removes gridlines
    #panel.grid.minor = element_blank(), #removes gridlines
    panel.background = element_blank(), #removes gridlines
    axis.line = element_line(colour = "black"), 
    axis.text.x = element_text(size=16), #size of x axis text
    axis.text.y = element_text(size=16), #size of y axis text
    axis.title = element_text(size= 21), # size of title text
    legend.title= element_text(size = 15), #size of legend title
    legend.text = element_text(size = 15), #size of legend text
    plot.title = element_text(hjust=0.5, size=24)) #size of plot title and centered

#pom in g/kg

PomICsum.g <- summarySE(somdata, measurevar = "POM.IC.gsoil", groupvars = c("Depth.cm", "Year", "Treatment"))

PomICDatasum.g <- within(PomICsum.g, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))  #flip axis

ggplot(PomICDatasum.g,
       aes(x=POM.IC.gsoil,
           y=as.factor(Depth.cm), 
           shape = Year,
           color = Treatment))+
  theme_bw()+
  geom_errorbar(aes(xmin=POM.IC.gsoil-se, xmax=POM.IC.gsoil+se), 
                colour="black", width=.1, position=position_jitter(seed=100))+
  #I think this "width" argument removes the horizontal lines at
  #the end of the error bar- not sure
  geom_point(size=7, position = position_jitter(seed=100)) +
  
  #you can futz with these themes and see if you like them. 
  theme(#panel.grid.major = element_blank(), #removes gridlines
    #panel.grid.minor = element_blank(), #removes gridlines
    panel.background = element_blank(), #removes gridlines
    axis.line = element_line(colour = "black"), 
    axis.text.x = element_text(size=16), #size of x axis text
    axis.text.y = element_text(size=16), #size of y axis text
    axis.title = element_text(size= 21), # size of title text
    legend.title= element_text(size = 15), #size of legend title
    legend.text = element_text(size = 15), #size of legend text
    plot.title = element_text(hjust=0.5, size=24)) #size of plot title and centered

#maom

MaomICsum <- summarySE(somdata, measurevar = "Maom.TIC.percent", groupvars = c("Depth.cm", "Year", "Treatment"))

MaomICDatasum <- within(MaomICsum, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))  #flip axis

#your plot w/ Depth.cm on y-axis will essentially be flipped-

ggplot(MaomICDatasum,
       aes(x=Maom.TIC.percent,
           y=as.factor(Depth.cm), 
           shape = Year,
           color = Treatment))+
  theme_bw()+
  geom_errorbar(aes(xmin=Maom.TIC.percent-se, xmax=Maom.TIC.percent+se), 
                colour="black", width=.1,  position=position_jitter(seed=100))+
  #I think this "width" argument removes the horizontal lines at
  #the end of the error bar- not sure
  geom_point(size=7,  position=position_jitter(seed=100)) +
  
  #you can futz with these themes and see if you like them. 
  theme(#panel.grid.major = element_blank(), #removes gridlines
    #panel.grid.minor = element_blank(), #removes gridlines
    panel.background = element_blank(), #removes gridlines
    axis.line = element_line(colour = "black"), 
    axis.text.x = element_text(size=16), #size of x axis text
    axis.text.y = element_text(size=16), #size of y axis text
    axis.title = element_text(size= 21), # size of title text
    legend.title= element_text(size = 15), #size of legend title
    legend.text = element_text(size = 15), #size of legend text
    plot.title = element_text(hjust=0.5, size=24)) #size of plot title and centered


#MAOM IC in g/kg soil #

MaomICsum.g <- summarySE(somdata, measurevar = "MAOM.IC.gsoil", groupvars = c("Depth.cm", "Year", "Treatment"))

MaomICDatasum.g <- within(MaomICsum.g, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))  #flip axis

ggplot(MaomICDatasum.g,
       aes(x=MAOM.IC.gsoil,
           y=as.factor(Depth.cm), 
           shape = Year,
           color = Treatment))+
  theme_bw()+
  geom_errorbar(aes(xmin=MAOM.IC.gsoil-se, xmax=MAOM.IC.gsoil+se), 
                colour="black", width=.1, position=position_jitter(seed=100))+
  #I think this "width" argument removes the horizontal lines at
  #the end of the error bar- not sure
  geom_point(size=7, position = position_jitter(seed=100)) +
  
  #you can futz with these themes and see if you like them. 
  theme(#panel.grid.major = element_blank(), #removes gridlines
    #panel.grid.minor = element_blank(), #removes gridlines
    panel.background = element_blank(), #removes gridlines
    axis.line = element_line(colour = "black"), 
    axis.text.x = element_text(size=16), #size of x axis text
    axis.text.y = element_text(size=16), #size of y axis text
    axis.title = element_text(size= 21), # size of title text
    legend.title= element_text(size = 15), #size of legend title
    legend.text = element_text(size = 15), #size of legend text
    plot.title = element_text(hjust=0.5, size=24)) #size of plot title and centered


#bulk

bulkICsum <- summarySE(SOMData, measurevar = "Bulk.TIC.percent", groupvars = c("Depth.cm", "Year", "Treatment"))

bulkICDatasum <- within(bulkICsum, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))  #flip axis

#your plot w/ Depth.cm on y-axis will essentially be flipped-

ggplot(bulkICDatasum,
       aes(x=Bulk.TIC.percent,
           y=as.factor(Depth.cm), 
           shape = Year,
           color = Treatment))+
  theme_bw()+
  geom_errorbar(aes(xmin=Bulk.TIC.percent-se, xmax=Bulk.TIC.percent+se), 
                colour="black", width=.1, position="identity")+
  #I think this "width" argument removes the horizontal lines at
  #the end of the error bar- not sure
  geom_point(size=7, position = "jitter") +
  
  #you can futz with these themes and see if you like them. 
  theme(#panel.grid.major = element_blank(), #removes gridlines
    #panel.grid.minor = element_blank(), #removes gridlines
    panel.background = element_blank(), #removes gridlines
    axis.line = element_line(colour = "black"), 
    axis.text.x = element_text(size=16), #size of x axis text
    axis.text.y = element_text(size=16), #size of y axis text
    axis.title = element_text(size= 21), # size of title text
    legend.title= element_text(size = 15), #size of legend title
    legend.text = element_text(size = 15), #size of legend text
    plot.title = element_text(hjust=0.5, size=24)) #size of plot title and centered


#OC figs####
PomOCsum <- summarySE(SOMData, measurevar = "Pom.TOC.percent", groupvars = c("Depth.cm", "Year", "Treatment"))

PomOCDatasum <- within(PomOCsum, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))  #flip axis

ggplot(PomOCDatasum,
       aes(x=Pom.TOC.percent,
           y=as.factor(Depth.cm), 
           shape = Year,
           color = Treatment))+
  theme_bw()+
  geom_errorbar(aes(xmin=Pom.TOC.percent-se, xmax=Pom.TOC.percent+se), 
                colour="black", width=.1, position=position_jitter(seed=100))+
  #I think this "width" argument removes the horizontal lines at
  #the end of the error bar- not sure
  geom_point(size=7, position = position_jitter(seed=100)) +
  
  #you can futz with these themes and see if you like them. 
  theme(#panel.grid.major = element_blank(), #removes gridlines
    #panel.grid.minor = element_blank(), #removes gridlines
    panel.background = element_blank(), #removes gridlines
    axis.line = element_line(colour = "black"), 
    axis.text.x = element_text(size=16), #size of x axis text
    axis.text.y = element_text(size=16), #size of y axis text
    axis.title = element_text(size= 21), # size of title text
    legend.title= element_text(size = 15), #size of legend title
    legend.text = element_text(size = 15), #size of legend text
    plot.title = element_text(hjust=0.5, size=24)) #size of plot title and centered

#pom in g/kg

PomOCsum.g <- summarySE(SOMData, measurevar = "POM.OC.gsoil", groupvars = c("Depth.cm", "Year", "Treatment"))

PomOCDatasum.g <- within(PomOCsum.g, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))  #flip axis

ggplot(PomOCDatasum.g,
       aes(x=POM.OC.gsoil,
           y=as.factor(Depth.cm), 
           shape = Year,
           color = Treatment))+
  theme_bw()+
  geom_errorbar(aes(xmin=POM.OC.gsoil-se, xmax=POM.OC.gsoil+se), 
                colour="black", width=.1, position=position_jitter(seed=100))+
  #I think this "width" argument removes the horizontal lines at
  #the end of the error bar- not sure
  geom_point(size=7, position = position_jitter(seed=100)) +
  
  #you can futz with these themes and see if you like them. 
  theme(#panel.grid.major = element_blank(), #removes gridlines
    #panel.grid.minor = element_blank(), #removes gridlines
    panel.background = element_blank(), #removes gridlines
    axis.line = element_line(colour = "black"), 
    axis.text.x = element_text(size=16), #size of x axis text
    axis.text.y = element_text(size=16), #size of y axis text
    axis.title = element_text(size= 21), # size of title text
    legend.title= element_text(size = 15), #size of legend title
    legend.text = element_text(size = 15), #size of legend text
    plot.title = element_text(hjust=0.5, size=24)) #size of plot title and centered


#maom

MaomOCsum <- summarySE(SOMData, measurevar = "Maom.TOC.percent", groupvars = c("Depth.cm", "Year", "Treatment"))

MaomOCDatasum <- within(MaomOCsum, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))  #flip axis

#your plot w/ Depth.cm on y-axis will essentially be flipped-

ggplot(MaomOCDatasum,
       aes(x=Maom.TOC.percent,
           y=as.factor(Depth.cm), 
           shape = Year,
           color = Treatment))+
  theme_bw()+
  geom_errorbar(aes(xmin=Maom.TOC.percent-se, xmax=Maom.TOC.percent+se), 
                colour="black", width=.1, position=position_jitter(seed=100))+
  #I think this "width" argument removes the horizontal lines at
  #the end of the error bar- not sure
  geom_point(size=7, position = position_jitter(seed=100)) +
  
  #you can futz with these themes and see if you like them. 
  theme(#panel.grid.major = element_blank(), #removes gridlines
    #panel.grid.minor = element_blank(), #removes gridlines
    panel.background = element_blank(), #removes gridlines
    axis.line = element_line(colour = "black"), 
    axis.text.x = element_text(size=16), #size of x axis text
    axis.text.y = element_text(size=16), #size of y axis text
    axis.title = element_text(size= 21), # size of title text
    legend.title= element_text(size = 15), #size of legend title
    legend.text = element_text(size = 15), #size of legend text
    plot.title = element_text(hjust=0.5, size=24)) #size of plot title and centered


#maom- g/kg soil

MaomOCsum.g <- summarySE(SOMData, measurevar = "MAOM.OC.gsoil", groupvars = c("Depth.cm", "Year", "Treatment"))

MaomOCDatasum.g <- within(MaomOCsum.g, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))  #flip axis

#your plot w/ Depth.cm on y-axis will essentially be flipped-

ggplot(MaomOCDatasum.g,
       aes(x=MAOM.OC.gsoil,
           y=as.factor(Depth.cm), 
           shape = Year,
           color = Treatment))+
  theme_bw()+
  geom_errorbar(aes(xmin=MAOM.OC.gsoil-se, xmax=MAOM.OC.gsoil+se), 
                colour="black", width=.1, position=position_jitter(seed=100))+
  #I think this "width" argument removes the horizontal lines at
  #the end of the error bar- not sure
  geom_point(size=7, position = position_jitter(seed=100)) +
  
  #you can futz with these themes and see if you like them. 
  theme(#panel.grid.major = element_blank(), #removes gridlines
    #panel.grid.minor = element_blank(), #removes gridlines
    panel.background = element_blank(), #removes gridlines
    axis.line = element_line(colour = "black"), 
    axis.text.x = element_text(size=16), #size of x axis text
    axis.text.y = element_text(size=16), #size of y axis text
    axis.title = element_text(size= 21), # size of title text
    legend.title= element_text(size = 15), #size of legend title
    legend.text = element_text(size = 15), #size of legend text
    plot.title = element_text(hjust=0.5, size=24)) #size of plot title and centered


#bulk

bulkOCsum <- summarySE(SOMData, measurevar = "Bulk.TOC.percent", groupvars = c("Depth.cm", "Year", "Treatment"))

bulkOCDatasum <- within(bulkOCsum, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))  #flip axis

#your plot w/ Depth.cm on y-axis will essentially be flipped-

ggplot(bulkOCDatasum,
       aes(x=Bulk.TOC.percent,
           y=as.factor(Depth.cm), 
           shape = Year,
           color = Treatment))+
  theme_bw()+
  geom_errorbar(aes(xmin=Bulk.TOC.percent-se, xmax=Bulk.TOC.percent+se), 
                colour="black", width=.1, position=position_jitter(seed=100))+
  #I think this "width" argument removes the horizontal lines at
  #the end of the error bar- not sure
  geom_point(size=7, position = position_jitter(seed=100)) +
  
  #you can futz with these themes and see if you like them. 
  theme(#panel.grid.major = element_blank(), #removes gridlines
    #panel.grid.minor = element_blank(), #removes gridlines
    panel.background = element_blank(), #removes gridlines
    axis.line = element_line(colour = "black"), 
    axis.text.x = element_text(size=16), #size of x axis text
    axis.text.y = element_text(size=16), #size of y axis text
    axis.title = element_text(size= 21), # size of title text
    legend.title= element_text(size = 15), #size of legend title
    legend.text = element_text(size = 15), #size of legend text
    plot.title = element_text(hjust=0.5, size=24)) #size of plot title and centered


#models####

#create data file for anovas
aov.pom = data.frame()

emmeans.pom.depth =data.frame()
emmeans.pom.year =data.frame()
emmeans.pom.trt = data.frame()
emmeans.pom.trt.depth = data.frame()
emmeans.pom.year.depth= data.frame()
emmeans.pom.trt.year.depth = data.frame()


#not putting anything in these to begin with means I'll need to make sure I keep these in order and paste in that extra column at the end

#measurement= rep(c("pom.percent", "pom.OC", "pom.IC", "maom.OC", "maom.IC"), 8)) 
                    

#mass POM- %

#check normality and transformations
ggqqplot(somdata$Pom_percent)
ggqqplot(log(somdata$Pom_percent))

POMmodel <- lmer(log(Pom_percent) ~ Treatment * Year * Depth.cm + 
                   (1|Block/Point) + (Year|Block/Point), 
                 data = somdata)
#filter(SOMData, Year!="2019"))
car::Anova(POMmodel, test = "F", type=3)

t.aov.pom= car::Anova(POMmodel, test = "F", type=3)
t.aov.pom$measurement = "log.pom.percent"

aov.pom = t.aov.pom #for future iterations this should be rbind

#sig = depth, trtxdepth, yrxdepth
t.cld.depth = cld(emmeans(POMmodel, pairwise ~ Depth.cm), Letters = LETTERS)
t.cld.depth$measurement = "log.pom.percent"
emmeans.pom.depth= t.cld.depth #for future iterations this should be rbind

t.cld.yr.depth = cld(emmeans(POMmodel, pairwise ~ Year|Depth.cm), Letters = LETTERS)
t.cld.yr.depth$measurement = "log.pom.percent"
emmeans.pom.year.depth= t.cld.yr.depth #for future iterations this should be rbind

t.cld.trt.depth = cld(emmeans(POMmodel, pairwise ~ Treatment|Depth.cm), Letters = LETTERS)
t.cld.trt.depth$measurement = "log.pom.percent"
emmeans.pom.trt.depth= t.cld.trt.depth #for future iterations this should be rbind


#POM-OC-g

#check normality and transformations
ggqqplot(somdata$POM.OC.gsoil)
ggqqplot(log(somdata$POM.OC.gsoil))

ocPOMmodel <- lmer(log(POM.OC.gsoil) ~ Treatment * Year * Depth.cm + 
                   (1|Block/Point) + (Year|Block/Point), 
                 data = somdata)
#filter(SOMData, Year!="2019"))
car::Anova(ocPOMmodel, test = "F", type=3)

po.aov.pom= car::Anova(ocPOMmodel, test = "F", type=3)
po.aov.pom$measurement = "log.pom.OC.gsoil"

aov.pom = rbind(aov.pom,po.aov.pom)

#sig = depth, trtxdepth, trtxyrxdepth
po.cld.depth = cld(emmeans(ocPOMmodel, pairwise ~ Depth.cm), Letters = LETTERS)
po.cld.depth$measurement = "log.pom.OC.gsoil"
emmeans.pom.depth= rbind(emmeans.pom.depth, po.cld.depth)

po.cld.trt.yr.depth = cld(emmeans(ocPOMmodel, pairwise ~ Treatment|Year|Depth.cm), Letters = LETTERS)
po.cld.trt.yr.depth$measurement = "log.pom.OC.gsoil"
emmeans.pom.trt.year.depth= po.cld.trt.yr.depth #for future iterations this should be rbind

po.cld.trt.depth = cld(emmeans(ocPOMmodel, pairwise ~ Treatment|Depth.cm), Letters = LETTERS)
po.cld.trt.depth$measurement = "log.pom.OC.gsoil"
emmeans.pom.trt.depth= rbind(emmeans.pom.trt.depth, po.cld.trt.depth)

#POM-IC-g

#check normality and transformations
ggqqplot(somdata$POM.IC.gsoil)
ggqqplot(log(somdata$POM.IC.gsoil))

icPOMmodel <- lmer(log(POM.IC.gsoil) ~ Treatment * Year * Depth.cm + 
                     (1|Block/Point) + (Year|Block/Point), 
                   data = somdata)
#filter(SOMData, Year!="2019"))
car::Anova(icPOMmodel, test = "F", type=3)

pi.aov.pom= car::Anova(icPOMmodel, test = "F", type=3)
pi.aov.pom$measurement = "log.pom.IC.gsoil"

aov.pom = rbind(aov.pom,pi.aov.pom)

#sig = nothing

#MAOM-OC-g

#check normality and transformations
ggqqplot(somdata$MAOM.OC.gsoil)
ggqqplot(log(somdata$MAOM.OC.gsoil))

ocMAOMmodel <- lmer(log(MAOM.OC.gsoil) ~ Treatment * Year * Depth.cm + 
                     (1|Block/Point) + (Year|Block/Point), 
                   data = somdata)
#filter(SOMData, Year!="2019"))
car::Anova(ocMAOMmodel, test = "F", type=3)

mo.aov.pom= car::Anova(ocMAOMmodel, test = "F", type=3)
mo.aov.pom$measurement = "log.maom.OC.gsoil"

aov.pom = rbind(aov.pom, mo.aov.pom)

#sig = depth, yrxdepth
mo.cld.depth = cld(emmeans(ocMAOMmodel, pairwise ~ Depth.cm), Letters = LETTERS)
mo.cld.depth$measurement = "log.maom.OC.gsoil"
emmeans.pom.depth= rbind(emmeans.pom.depth, mo.cld.depth)

mo.cld.yr.depth = cld(emmeans(ocMAOMmodel, pairwise ~ Year|Depth.cm), Letters = LETTERS)
mo.cld.yr.depth$measurement = "log.maom.OC.gsoil"
emmeans.pom.year.depth= rbind(emmeans.pom.year.depth, mo.cld.yr.depth)


#MAOM-IC-g

##check normality and transformations
ggqqplot(somdata$MAOM.IC.gsoil)
ggqqplot(log(somdata$MAOM.IC.gsoil))

icMAOMmodel <- lmer(log(MAOM.IC.gsoil) ~ Treatment * Year * Depth.cm + 
                      (1|Block/Point) + (Year|Block/Point), 
                    data = somdata)
#filter(SOMData, Year!="2019"))
car::Anova(icMAOMmodel, test = "F", type=3)

mi.aov.pom= car::Anova(icMAOMmodel, test = "F", type=3)
mi.aov.pom$measurement = "log.maom.IC.gsoil"

aov.pom = rbind(aov.pom, mi.aov.pom)

#sig = depth, yrxdepth
mi.cld.depth = cld(emmeans(icMAOMmodel, pairwise ~ Depth.cm), Letters = LETTERS)
mi.cld.depth$measurement = "log.maom.IC.gsoil"
emmeans.pom.depth= rbind(emmeans.pom.depth, mi.cld.depth)

mi.cld.yr.depth = cld(emmeans(icMAOMmodel, pairwise ~ Year|Depth.cm), Letters = LETTERS)
mi.cld.yr.depth$measurement = "log.maom.IC.gsoil"
emmeans.pom.year.depth= rbind(emmeans.pom.year.depth, mo.cld.yr.depth)

#bulk-OC-g

#bulk-IC-g

write.csv(aov.pom,
          "POM lmer models ANOVA 1.26.24.csv",
          row.names = FALSE)

write.csv(emmeans.pom.depth,
          "POM depth CLD 1.26.24.csv",
          row.names = FALSE)

write.csv(emmeans.pom.trt.depth,
          "POM trt.depth CLD 1.26.24.csv",
          row.names = FALSE)

write.csv(emmeans.pom.trt.year.depth,
          "POM trt.year.depth CLD 1.26.24.csv",
          row.names = FALSE)

write.csv(emmeans.pom.year.depth,
          "POM year.depth CLD 1.26.24.csv",
          row.names = FALSE)
