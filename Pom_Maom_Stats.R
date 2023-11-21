############################## SOM Stats (POM and MAOM)
# data is called "SOMData"

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



SOMData = read.delim("SOMData.csv", header=TRUE, sep = ",")
str(SOMData)

#Convert percentages to mass of POM and MAOM
#does KS mean proportion, not mass?
# I don't think this number is useful at all
SOMData$Pom_mass<-SOMData$Pom_percent*10
SOMData$MAOM_mass<-SOMData$Maom_percent*10

# Set variables to factors
SOMData$Depth.cm <- as.factor(SOMData$Depth.cm)
SOMData$Block <- as.factor(SOMData$Block)
SOMData$Treatment <- as.factor(SOMData$Treatment)
SOMData$Year<- as.factor(SOMData$Year)
SOMData$Section <- as.factor(SOMData$Section)

str(SOMData)


#Normality tests
#transform POM and MAOM mass data
# save each transformation as a new column in data frame
POMstatsum$Pom_percent_transformed <- log(POMstatsum$Pom_percent)
ggdensity(POMstatsum$Pom_percent)
ggdensity(POMstatsum$Pom_percent_transformed)
ggqqplot(POMstatsum$Pom_percent_transformed)
ggqqplot(POMstatsum$Pom_percent)

ggdensity(POMmassstatsum$Pom_mass)
POMmassstatsum$Pom_mass_transformed <- log(POMmassstatsum$Pom_mass)
ggdensity(POMmassstatsum$Pom_mass_transformed)

#none of these transformations are helpful
#basically those high numbers
ggdensity(MAOMstatsum$Maom_percent)
MAOMstatsum$MAOM_percent_transformed <- log10(1/(MAOMstatsum$Maom_percent))
#sqrt(max(MAOMstatsum$MAOM_mass + 1) - MAOMstatsum$MAOM_mass)
ggdensity(MAOMstatsum$MAOM_percent_transformed)
ggqqplot(MAOMstatsum$MAOM_percent)

#Filter out two years

POMstatsum1920<- filter(POMstatsum, Year == c("2019","2020"))
str(POMstatsum1920)

POMcdnmodel <- lmer(Pom_percent_transformed ~ Treatment * Year * Depth.cm + (1|Block/Point) + (Year|Block/Point), data = POMstatsum)
car::Anova(POMcdnmodel, test = "F", type = 3)

emmeans(POMcdnmodel, pairwise ~ Treatment*Depth.cm*Year)

options(max.print=100000)
str(POMstatsum)

# Mixed Effects Model - POM####
# fixed: treatment, depth, year
#random: point nested in block
# repeated measure of point nested in block over time (year)
POMmodel <- lmer(Pom_percent_transformed ~ Treatment * Year * Depth.cm + 
                   (1|Block/Point) + (Year|Block/Point), 
                 data = POMstatsum)
                   #filter(POMstatsum, Year!="2019"))
car::Anova(POMmodel, test = "F", type=3)

cld(emmeans(POMmodel, pairwise ~ Year|Depth.cm), Letters = LETTERS)
cld(emmeans(POMmodel, pairwise ~ Treatment|Depth.cm), Letters = LETTERS)
cld(emmeans(POMmodel, pairwise ~ Treatment|Year), Letters = LETTERS)

#emtrends() <- might be useful in the future (?)


# Mixed Effects Model - MAOM
MAOMmodel <- lmer(MAOM_mass_transformed ~ Treatment * Depth.cm * Year + 
                    (1|Block/Point) + (Year|Block/Point), data = MAOMstatsum)
Anova(MAOMmodel, test = "F")

cld(emmeans(MAOMmodel, pairwise ~ Depth.cm), Letters = LETTERS)
cld(emmeans(MAOMmodel, pairwise ~ Treatment), Letters = LETTERS)



#calculating mass of TOC/TIC in POM and MAOM####

#steps
#multiply POM % by POM mass percent to get g POM-OC/g soil
#repeat for MAOM
#add POM-OC + MAOM OC for recovered OC %
#and check recovery with bulk OC: reocvered OC/bulk OC

#repeat above for IC


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
SOMData$OC.rec.percent = SOMData$OC.rec/SOMData$Bulk.TOC.percent

ggplot(SOMData,
       aes(y= OC.rec,
           x = Bulk.TOC.percent/100,
           color = Treatment,
           shape = Depth.cm,
           size = Pom.TOC.percent))+
  geom_point()+
  geom_abline(slope=1, intercept =0)
         

cor.test(SOMData$OC.rec.percent, SOMData$Bulk.TOC.percent/100)
#correlation is not great, though the spread isn't terrible either. 
#I'd say the data at the surface pulls a little higher towards the recovered OC,
#maybe a little bias towards samples with higher %POM-OC having higher OC.rec
#that makes sense

# Summarizing POM and MAOM based on drainage treatment and depth####

POMtreatmentsum <- summarySE(POMstatsum, measurevar = "Pom_percent", groupvars = "Treatment")

POMdepthsum <- summarySE(POMstatsum, measurevar = "Pom_percent", groupvars = "Depth.cm")

MAOMtreatmentsum <- summarySE(MAOMstatsum, measurevar = "MAOM_mass", groupvars = "Treatment")

MAOMdepthsum <- summarySE(MAOMstatsum, measurevar = "MAOM_mass", groupvars = "Depth.cm")


##########################################    END    ##################################################
POMstatsum$Year <- as.factor(POMstatsum$Year)
POMstatsum$Treatment <- as.factor(POMstatsum$Treatment)
POMstatsum$Block <- as.factor(POMstatsum$Block)
POMstatsum$Point <- as.factor(POMstatsum$Point)

MAOMstatsum$Year <- as.factor(MAOMstatsum$Year)
MAOMstatsum$Treatment <- as.factor(MAOMstatsum$Treatment)
MAOMstatsum$Block <- as.factor(MAOMstatsum$Block)
MAOMstatsum$Point <- as.factor(MAOMstatsum$Point)


# filter POMstatsum based on depth
POMstatsum030 <- filter(POMstatsum, Depth.cm == "0-30")

POMstatsum3060 <- filter(POMstatsum, Depth.cm == "30-60")

POMstatsum6090 <- filter(POMstatsum, Depth.cm == "60-90")

MAOMstatsum030 <- filter(MAOMstatsum, Depth.cm == "0-30")

MAOMstatsum3060 <- filter(MAOMstatsum, Depth.cm == "30-60")

MAOMstatsum6090 <- filter(MAOMstatsum, Depth.cm == "60-90")

str(POMstatsum030)
str(POMstatsum3060)
str(POMstatsum6090)


POMmassstatsum030 <- filter(POMmassstatsum, Depth.cm == "0-30")
POM030mass <- lmer(Pom_mass_transformed ~ Treatment * Year  + (1|Block/Point), data = POMmassstatsum030)
car::Anova(POM030mass, test = "F", type=3)

ggdensity(POMstatsum030$Pom_percent)
POMstatsum030$Pom_percent.transformed <- log(POMstatsum030$Pom_percent)

ggdensity(POMstatsum030$Pom_percent.transformed)

POM030 <- lmer(Pom_percent.transformed ~ Treatment * Year  + (1|Block/Point), data = POMstatsum030)
car::Anova(POM030, test = "F", type=3)

cld(emmeans(POM030, pairwise ~ Year), Letters = LETTERS)
cld(emmeans(POM030, pairwise ~ Treatment), Letters = LETTERS)
emmeans(POM030, pairwise ~ Treatment*Year)

POM030treatment <- summarySE(POMstatsum030, measurevar="Pom_percent", groupvars = "Treatment")
POM030treatment

POM030Year <- summarySE(POMstatsum030, measurevar="Pom_percent", groupvars = "Year")
POM030Year


POM3060 <- lmer(Pom_percent_transformed ~ Treatment * Year + (1|Block/Point), data = POMstatsum3060)
car::Anova(POM3060, test = "F", type=3)

cld(emmeans(POM3060, pairwise ~ Year), Letters = LETTERS)
cld(emmeans(POM3060, pairwise ~ Treatment), Letters = LETTERS)
emmeans(POM3060, pairwise ~ Treatment*Year)

POM3060treatment <- summarySE(POMstatsum3060, measurevar="Pom_percent", groupvars = "Treatment")
POM3060treatment

POM3060Year <- summarySE(POMstatsum3060, measurevar="Pom_percent", groupvars = "Year")
POM3060Year


POM6090 <- lmer(Pom_percent_transformed ~ Treatment * Year + (1|Block/Point), data = POMstatsum6090)
car::Anova(POM6090, test = "F", type=3)

cld(emmeans(POM6090, pairwise ~ Year), alpha=0.1, Letters = LETTERS)
cld(emmeans(POM6090, pairwise ~ Treatment), Letters = LETTERS)
emmeans(POM6090, pairwise ~ Treatment*Year)

POM6090treatment <- summarySE(POMstatsum6090, measurevar="Pom_percent", groupvars = "Treatment")
POM6090treatment

POM6090Year <- summarySE(POMstatsum6090, measurevar="Pom_percent", groupvars = "Year")
POM6090Year


# ANOVA POM 0-30 cm
POM030model <- lmer(Pom_percent_transformed ~ Treatment * Year + (1|Block/Point), data = POMstatsum030)
Anova(POM030model, test= "F")

summary(POMstatsum030)

# ANOVA POM 30-60 cm
aovPOM3 <- aov(Pom_percent ~ Treatment*Year + Block, data = POMstatsum3060)
summary(aovPOM3)
summary(POMstatsum3060)

# ANOVA POM 60-90 cm
aovPOM4 <- aov(Pom_percent ~ Treatment*Year + Block, data = POMstatsum6090)
summary(aovPOM4)

summary(POMstatsum6090)




##############################  Graph POM by section     #############################################
Pompercentsum <- summarySE(SOMData, measurevar = "Pom_percent", groupvars = c("Depth.cm", "Year", "Point", 
                                                                          "Treatment", "Block", "Section"))


Pompercentsum <- within(Pompercentsum, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))  #flip axis

Pompercentsum <- within(Pompercentsum, Year<- ordered(Year, levels = rev(sort(unique(Year)))))

Pomplot_combined <- ggplot(Pompercentsum, aes(x= Pom_percent, y= Depth.cm, fill=Treatment)) +
  geom_boxplot()+
 # geom_errorbar(aes(xmin=Pom_percent-se, xmax=Pom_percent+se), 
                #for a plot w/ Depth on y-axis,
                #use xmin and xmax instead of ymin and ymax
  #              colour="black", width=.1, position="identity")+
  #I think this "width" argument removes the horizontal lines at
  #the end of the error bar- not sure
 # geom_path() + #connects points based on order  +
 
  
   xlab("POM (percent of soil mass)")+ 
  ylab("Depth (cm)")+
  ggtitle("Change in POM After Two Years of Drainage")+
  facet_wrap(~Year*Section)+
  theme_pubclean()+
  theme(plot.title = element_text(hjust = 0.5, size = 22),
        axis.text.x = element_text(size=10, angle=0, hjust = 1.0), #size of x axis text
        axis.text.y = element_text(size=12), #size of y axis text
        axis.title = element_text(size= 18), # size of title text
        legend.title= element_text(size = 15), #size of legend title
        legend.text = element_text(size = 13))+  #size of legend text 
  scale_fill_manual(values = c( "deepskyblue2",  "darkgoldenrod"))
Pomplot_combined <- Pomplot_combined + stat_summary(fun = "mean", geom = "point", shape = 8, size = 1, col = "red", position = position_dodge(width = .75))

Pomplot_combined



Pomplot_combined <- ggplot(Pompercentsum, aes(x= Pom_percent, y= as.factor(Year), fill=Treatment)) +
  geom_boxplot()+
  # geom_errorbar(aes(xmin=Pom_percent-se, xmax=Pom_percent+se), 
  #for a plot w/ Depth on y-axis,
  #use xmin and xmax instead of ymin and ymax
  #              colour="black", width=.1, position="identity")+
  #I think this "width" argument removes the horizontal lines at
  #the end of the error bar- not sure
  # geom_path() + #connects points based on order  +
  
  
  xlab("POM (percent of soil mass)")+ 
  ylab("Year")+
  ggtitle("Change in POM After Two Years of Drainage")+
  facet_wrap(~Depth.cm*Section)+
  theme_pubclean()+
  theme(plot.title = element_text(hjust = 0.5, size = 22),
        axis.text.x = element_text(size=10, angle=0, hjust = 1.0), #size of x axis text
        axis.text.y = element_text(size=12), #size of y axis text
        axis.title = element_text(size= 18), # size of title text
        legend.title= element_text(size = 15), #size of legend title
        legend.text = element_text(size = 13))+  #size of legend text 
  scale_fill_manual(values = c( "deepskyblue2",  "darkgoldenrod"))
Pomplot_combined <- Pomplot_combined + stat_summary(fun = "mean", geom = "point", shape = 8, size = 1, col = "red", position = position_dodge(width = .75))

Pomplot_combined




ggplot(SOMDatasum, aes(x=Pom_percent, y=as.factor(Depth.cm), group=interaction(Treatment, Year)))+
  
  guides(shape=guide_legend(title="Treatment and Year")) + #format legend
  guides(fill=guide_legend(title="Treatment and Year")) + #format legend
  
  guides(shape=guide_legend(title="Treatment and Year")) + #format legend
  guides(fill=guide_legend(title="Treatment and Year")) + #format legend
  
  theme_bw()+
  geom_errorbar(aes(xmin=Pom_percent-se, xmax=Pom_percent+se), 
                #for a plot w/ Depth on y-axis,
                #use xmin and xmax instead of ymin and ymax
                colour="black", width=.1, position="identity")+
  #I think this "width" argument removes the horizontal lines at
  #the end of the error bar- not sure
  geom_path() + #connects points based on order
  
  
  geom_point(aes(shape=interaction(Treatment, Year), #make the different year categories different shapes
                 fill = interaction(Treatment, Year)),position="identity", #make the different year categories different colors
             size=7) + 
  
  scale_shape_manual(values=c(21,21,22,22,24,24))+ #manually choses which shapes I wanted
  scale_fill_manual(values = c( "deepskyblue2",  "darkgoldenrod","deepskyblue2", "darkgoldenrod", "deepskyblue2", "darkgoldenrod"))+ #manually chooses which colors
  #  scale_x_continuous(breaks = c(0,5,10,15,20,25,30), position = 'top')+
  # tells the axis not to record all the numbers between the week #top moves axis labels to top of graph
  scale_linetype_manual(values=c("solid"))+ #manually chooses which line types I wanted
  
  labs(y="Depth (cm)", x="POM (% of soil mass)", cex=17) + #change text of axis labels
  ggtitle("Change in POM After Two Years of Drainage") + #title of plot
  
  
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


#Summary stats for MAOM ####
# - averaging analytical replicates together
#none of the below works on SOMData in 2023 because it doesn't contain analytical reps
#looks like these are available in the 2022 version of the data.

# MAOMstatsum <- summarySE(SOMData, 
#                          measurevar = "Maom_percent", 
#                          groupvars = c("Depth.cm", "Year", "Point", 
#                                  "Treatment", "Block", "Section"))
# str(MAOMstatsum) 



# Summary stats for POM - averaging analytical replicates together
# POMstatsum <- summarySE(SOMData, measurevar="Pom_percent", 
#                         groupvars = c("Depth.cm", "Year", "Point", 
#                                       "Treatment", "Section", "Block")) 
# str(POMstatsum)

# POMmassstatsum <- summarySE(SOMData, measurevar="Pom_mass", groupvars = c("Depth.cm", "Year", "Point", 
# "Treatment", "Section", "Block")) 
# str(POMmassstatsum)
