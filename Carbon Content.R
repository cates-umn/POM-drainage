
# dataset is SOMData
# The code below is for POM Total Carbon percent.  This code can be manipulated for any combination of POM, MAOM, or Bulk soil and its respective inroganic, organic, or total carbon content (percent)


library(ggplot2)
library(Rmisc)
library(magrittr)
library(dplyr)

POM.percentsum <- summarySE(SOMData, measurevar = "POM.percent", 
                            groupvars = c("Depth.cm", "Section", "Year", "Treatment"))

str(SOMData)
# There are no replicated in soiltoc data

#Graph SOMData

SOMData$Year <- as.factor(SOMData$Year)
SOMData$Depth.cm <- as.factor(SOMData$Depth.cm)
SOMData$Section <- as.factor(SOMData$Section)


######   POM Total C percent   #######

SOMData <- within(SOMData, Year<- ordered(Year, levels = rev(sort(unique(Year)))))  #flip axis

SOMData_plot_combined <- ggplot(SOMData, aes(x= Year, y= Pom.TC.percent, fill=Treatment)) +
  geom_boxplot() +
  ylab("Carbon Content") +
  xlab("Year") + 
  ggtitle("Change in POM Total Carbon After \n Two Years of Drainage")+
  facet_wrap(~Depth.cm)+
  theme_pubclean()+
  theme(
    panel.grid.major.y =  element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 22),
    axis.text.x = element_text(size=10, angle=0, hjust = 1.0), #size of x axis text
    axis.text.y = element_text(size=12), #size of y axis text
    axis.title = element_text(size= 18), # size of title text
    legend.title= element_text(size = 15), #size of legend title
    legend.text = element_text(size = 13))+  #size of legend text 
  scale_fill_manual(values = c( "deepskyblue2",  "darkgoldenrod"))
SOMData_plot_combined <- SOMData_plot_combined + stat_summary(fun = "mean", geom = "point", shape = 10, size = 1, col = "red", position = position_dodge(width = .75))

SOMData_plot_combined






########################################    POM Total C stats   #############################################


# data as a whole for POM Total Carbon

ggdensity(SOMData$Pom.TC.percent)
ggqqplot(SOMData$Pom.TC.percent)
#SOMData$Pom.TC.percent_transformed <- sqrt(SOMData$Pom.TC.percent   #If you need to transform data, the heading used below may need to be changed.
ggdensity(SOMData$Pom.TC.percent)

pomTCmodel <- lmer(Pom.TC.percent ~ Treatment * Depth.cm  + (1|Block), data = SOMData)  # If I remember correctly, year was dropped due to limited statisical power with only three years of data
car::Anova(pomTCmodel, test = "F", type=3)

cld(emmeans(pomTCmodel, pairwise ~ Depth.cm), Letters = LETTERS)
cld(emmeans(pomTCmodel, pairwise ~ Treatment), Letters = LETTERS)
emmeans(pomTCmodel, pairwise ~ Depth.cm*Treatment)


# filter SOMData based on depth
SOMData030 <- filter(SOMData, Depth.cm == "0-30")

SOMData3060 <- filter(SOMData, Depth.cm == "30-60")

SOMData6090 <- filter(SOMData, Depth.cm == "60-90")



#POM TC percent 0-30 cm 

ggdensity(SOMData030$Pom.TC.percent)
ggqqplot(SOMData030$Pom.TC.percent)
#SOMData$Pom.TC.percent_transformed <- sqrt(SOMData$Pom.TC.percent)
ggdensity(SOMData$Pom.TC.percent)

pomTCpercent030model <- lmer(Pom.TC.percent ~ Treatment * Year + (1|Block), data = SOMData030)
car::Anova(pomTCpercent030model, test = "F", type=3)

cld(emmeans(pomTCpercent030model, pairwise ~ Year), Letters = LETTERS)
cld(emmeans(pomTCpercent030model, pairwise ~ Treatment), Letters = LETTERS)

emmeans(pomTCpercent030model, pairwise ~ Year*Treatment)

SOMData030treatmet <- summarySE(SOMData030, measurevar="Pom.TC.percent", groupvars = "Treatment")
SOMData030year <- summarySE(SOMData030, measurevar="Pom.TC.percent", groupvars = "Year")



#POM TC percent 30-60 cm 

ggdensity(SOMData3060$Pom.TC.percent)
ggqqplot(SOMData3060$Pom.TC.percent)
#SOMData3060$Pom.TC.percent_transformed <- sqrt(SOMData3060$Pom.TC.percent)
ggdensity(SOMData3060$Pom.TC.percent)

pomTCpercent3060model <- lmer(Pom.TC.percent ~ Treatment *Year + (1|Block), data = SOMData3060)
car::Anova(pomTCpercent3060model, test = "F", type=3)

cld(emmeans(pomTCpercent3060model, pairwise ~ Treatment), Letters = LETTERS)
cld(emmeans(pomTCpercent3060model, pairwise ~ Year), Letters = LETTERS)

emmeans(pomTCpercent3060model, pairwise ~ Year*Treatment)

SOMData3060treatmet <- summarySE(SOMData360, measurevar="Pom.TC.percent", groupvars = "Treatment")
SOMData3060year <- summarySE(SOMData360, measurevar="Pom.TC.percent", groupvars = "Year")



#POM TC percent 60-90 cm 

ggdensity(SOMData6090$Pom.TC.percent)
ggqqplot(SOMData6090$Pom.TC.percent)
#SOMData6090$Pom.TC.percent_transformed <- sqrt(SOMData6090$Pom.TC.percent)
ggdensity(SOMData6090$Pom.TC.percent)

pomTCpercent6090model <- lmer(Pom.TC.percent ~ Treatment *Year + (1|Block), data = SOMData6090)
car::Anova(pomTCpercent6090model, test = "F", type=3)

cld(emmeans(pomTCpercent6090model, pairwise ~ Treatment), Letters = LETTERS)
cld(emmeans(pomTCpercent6090model, pairwise ~ Year), Letters = LETTERS)

emmeans(pomTCpercent6090model, pairwise ~ Year*Treatment)

SOMData6090treatmet <- summarySE(SOMData360, measurevar="Pom.TC.percent", groupvars = "Treatment")
SOMData6090year <- summarySE(SOMData360, measurevar="Pom.TC.percent", groupvars = "Year")




