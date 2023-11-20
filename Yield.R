##yield
## I think Aaron wrote some of the code in the second half of this script
## For the yield data, we used a webpage through John Deere.  I tihnk Lindsay told Aaron what it was, but I do not remember the name of it.



##packages
library(tidyverse)
library(RColorBrewer)
library(patchwork)

dataset = read.delim("~/Desktop/yield.csv", header=TRUE, sep = ",")
str(dataset)
datasetsum<- summarySE(dataset, measurevar = "Maom_percent", groupvars = c("Year", "Depth.cm", 
                                                                        "Treatment", "Point"))

yieldsumplot <-summarySE(yield, measurevar = "Yield2", groupvars = c("Year", "Section"))
yieldsumplot

yieldsumtreatment <-summarySE(yield, measurevar = "Yield2", groupvars = c("Year", "Treatment"))
yieldsumtreatment


##read in data

yield <- read.delim("~/Desktop/project/yield/Yield_Combined.csv", sep = ",")

yield$Yield2 <-ifelse(yield$Year == 2019 & 2021, yield$Yield*58.75, yield$Yield*59.02 )
yield

hist(yield$Yield2)

str(yield)

yield_2019 <- yield %>%
  filter(Year == "2019") %>%
  ggplot() +
  aes(x = Row, y = Column, fill = Yield2)+
  geom_tile()+
  labs(x = "", y = "")+
  ggtitle("2019 Yield")+
  scale_fill_distiller(palette = "RdYlGn", direction = 1, limits = c(1500,5000))+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),#legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

yield_2019 

   

yield_2021 <- yield %>%
  filter(Year == "2021") %>%
  ggplot() +
  aes(x = Row, y = Column, fill = Yield2)+
  geom_tile()+
  labs(x = "", y = "")+
  ggtitle("2021 Yield")+
  scale_fill_distiller(palette = "RdYlGn", direction = 1,  limits = c(1500,5000))+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),#legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

yield_2021   

##to make a difference graph
yield19 <- filter(yield, Year == "2019")
yield21 <- filter(yield, Year == "2021")

yield_diff<- merge(yield19, yield21, by = "Acre_ID")
yield_diff$difference <- (yield_diff$Yield2.y-yield_diff$Yield2.x)

yield_diff2 <- yield_diff  %>%
  ggplot() +
  aes(x = Row.x, y = Column.x, fill = difference)+
  geom_tile()+
  labs(x = "", y = "")+
  ggtitle("Difference between 2021 and 2019 Yield")+
  scale_fill_distiller(palette = "RdYlGn", direction = 1)+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),#legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

yield_diff2 

#figure?

(yield_2019+yield_2020)/(yield_2021+yield_diff2)




##boxplot figure

yield$Yield2 <- as.factor(yield$Yield2)

yield %>%
  ggplot()+
  aes(x = Year, y = Yield2, fill = Drainage)+
  geom_boxplot()


library(tidyverse)
library(tidyverse)
library(lavaan)
library(cowplot)
library(bitops)
library(caTools)
library(Rmisc)
library(PupillometryR)
library(RColorBrewer)


yield$Drainage <- as.character(yield$Drainage)
yield$Year <- as.character(yield$Year)

str(yield)

drain <- yield %>%
  filter(Drainage=="Drained")

un <- yield %>%
  filter(Drainage=="Undrained")

yield$Year <- factor(yield$Year, levels = c("2021", "2020", "2019"))


#boxplot?
ggplot(yield, aes(x=Year, y=Yield2, fill=Drainage) ) +
  geom_flat_violin(position=position_nudge(x=0.37, y=0), adjust=1, trim=FALSE) + #, alpha=0.5, color=NA) +
  geom_point(aes(color=Drainage), position = position_jitter(width = .05), size = 1, shape=20) + 
  geom_boxplot(data=un, aes(x=as.factor(Year), y=Yield2, fill=Drainage), width=.1, position=position_nudge(x=.29, y=0), outlier.shape=NA, color="black")+ #, alpha=0.5,  ) +  
  geom_boxplot(data=drain, aes(x=as.factor(Year), y=Yield2, fill=Drainage), width=.1, position=position_nudge(x=.16, y=0), outlier.shape=NA, color="black")+ #, alpha=0.5,  ) + ) + 
  coord_flip() +
  theme_cowplot() +
  ylab('Yield (kg ha-1)') +
  xlab('' ) +
  #geom_hline(yintercept=0, size=0.3, linetype="dashed") +
  scale_fill_manual(values = c("#EECC84", "#61ACA5")) +
  scale_color_manual(values = c("#EECC84", "#61ACA5"))


##stats

data2019U <- yield %>% 
  filter(Year=="2019", Drainage == "Undrained")

data2020U <- yield %>% 
  filter(Year=="2020", Drainage == "Undrained")

data2021U <- yield %>% 
  filter(Year=="2021", Drainage == "Undrained")

summary(data2019U)
sd(data2019U$Yield2)

summary(data2020U)
sd(data2020U$Yield2)

data2020 <- yield %>% 
  filter(Year=="2020")

summary(data2021U)
sd(data2021U$Yield2)


data2019 <- yield %>% 
  filter(Year=="2019")

data2020 <- yield %>% 
  filter(Year=="2020")

data2021 <- yield %>% 
  filter(Year=="2021")

model.19 <- lm(Yield2 ~ Treatment, data = data2019)
car::Anova(model.19, test="F", type=3)

model.20 <- lm(Yield2 ~ Treatment, data = data2020)
car::Anova(model.20, test="F", type=3)

model.21 <- lm(Yield2 ~ Treatment, data = data2021)
car::Anova(model.21, test="F", type=3)


library(arsenal)

data2019U <- yield %>% 
  filter(Year=="2019")

data2020U <- yield %>% 
  filter(Year=="2020")

data2021U <- yield %>% 
  filter(Year=="2021")


table <- tableby(Drainage~ Yield2, data = data2019U)
view((table))


str(yield)
yield$Yield2 <-as.numeric(yield$Yield2)
yield$Year <-as.factor(yield$Year)
yield$Block <-as.integer(yield$Block)
yield$Point <-as.integer(yield$Point)

Soil_Yield_Model <-lmer(Yield2 ~ Treatment*Year + (1|Block/Point)+(Year/Block), data = yield) 
car::Anova(Soil_Yield_Model, test = "F", type = 3)

emmeans(Soil_Yield_Model, pairwise ~Treatment*Year, adjust = "none")
emmeans(Soil_Yield_Model, pairwise ~Year, adjust = "none")
cld(emmeans(Soil_Yield_Model, pairwise ~Year, adjust = "none"))


hist(resid(SoilN_model))

?lmerCon

