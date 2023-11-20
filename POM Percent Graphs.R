###### data is SOMData


library(ggplot2)
library(Rmisc)
library(magrittr)
library(dplyr)

# import dataset, which is named SOM Data
SOMData = read.delim("~/Desktop/SOMData.csv", header=TRUE, sep = ",")
str(SOMData)


SOMDatasum<- summarySE(SOMData, measurevar = "Maom_percent", groupvars = c("Year", "Depth.cm", "Treatment", "Point"))

SOMDatasum <- summarySE(SOMData, measurevar="Pom_percent", groupvars = c("Depth.cm", "Year", "Point", "Treatment", "Section")) #summary stats of POM Percent based on the variables Depth.cm, year, point, treatment, and section

str(SOMDatasum)  # at this point the three reps have been averaged together

treatmentsum <- summarySE(SOMData, measurevar = "Pom_percent", groupvars = c("Year","Depth.cm", "Treatment", "Point"))
str(treatmentsum)  #point has been dropped so we are looking at POM percent based on Depth.cm, year, treatment, and section

treatmentsum$Depth.cm <- as.factor(treatmentsum$Depth.cm)

dat<-filter(SOMData, Year=="2019")

str(dat)
str(datsum)
datsum<-summarySE(dat, measurevar = "Pom_percent", groupvars = c("Treatment", "Depth.cm", "Point"))
write.csv(datsum, "SoMm_carbon.csv", ##### CHANGE FILE NAME
          row.names = FALSE)

#######################################################################################################
################ POM
######## Graph with all 3 years for Pom_percent by treatment


SOMData$Pom_mass<-SOMData$Pom_percent*10

treatmentsum <- summarySE(SOMData, measurevar = "Pom_percent", groupvars = c("Depth.cm", "Year", "Treatment"))
str(treatmentsum) 

SOMDatasum <- within(SOMDatasum, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))  #flip axis


#your plot w/ Depth.cm on y-axis will essentially be flipped-
#note that w/ 
#ggplot(wtsum, aes(x=weeks, y=org.rem, 
#                  group=interaction(ant,mesh)))+
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

###############################################################################################
######## Graph with all 3 years for Maom_percent by treatment


SOMData$Maom_mass<-SOMData$Maom_percent*10

treatmentmaomsum <- summarySE(SOMData, measurevar = "Maom_mass", groupvars = c("Depth.cm", "Year", "Treatment"))
str(treatmentmaomsum) 

treatmentmaomsum <- within(treatmentmaomsum, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))  #flip axis


#your plot w/ Depth.cm on y-axis will essentially be flipped-
#note that w/ 
#ggplot(wtsum, aes(x=weeks, y=org.rem, 
#                  group=interaction(ant,mesh)))+
ggplot(treatmentmaomsum, aes(x=Maom_mass, y=as.factor(Depth.cm), group=interaction(Treatment, Year)))+
  
  guides(shape=guide_legend(title="Treatment and Year")) + #format legend
  guides(fill=guide_legend(title="Treatment and Year")) + #format legend
  
  guides(shape=guide_legend(title="Treatment and Year")) + #format legend
  guides(fill=guide_legend(title="Treatment and Year")) + #format legend
  
  theme_bw()+
  geom_errorbar(aes(xmin=Maom_mass-se, xmax=Maom_mass+se), 
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
  
  labs(y="Depth (cm)", x="MAOM (g/kg)", cex=17) + #change text of axis labels
  ggtitle("Change in MAOM After Two Years of Drainage") + #title of plot
  
  
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



##############################################################################################

# Graph POM all 3 years by section

sectionsum <- summarySE(SOMData, measurevar = "Pom_mass", groupvars = c("Depth.cm", "Section", "Year", "Treatment"))
str(sectionsum)

sectionsum <- within(sectionsum, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))



ggplot(sectionsum, aes(x=Pom_mass, y=as.factor(Depth.cm), group=interaction(Year, Section)))+
  
  guides(shape=guide_legend(title="Year and Section")) + #format legend
  guides(fill=guide_legend(title="Year and Section")) + #format legend
  
  guides(shape=guide_legend(title="Year and Section")) + #format legend
  guides(fill=guide_legend(title="Year and Section")) + #format legend
  
  theme_bw()+
  geom_errorbar(aes(xmin=Pom_mass-se, xmax=Pom_mass+se), 
                #for a plot w/ Depth on y-axis,
                #use xmin and xmax instead of ymin and ymax
                colour="black", width=.1, position="identity")+
  #I think this "width" argument removes the horizontal lines at
  #the end of the error bar- not sure
  geom_path() + #connects points based on order
  
  
  geom_point(aes(shape=interaction(Section, Year), #make the different year categories different shapes
                 fill = interaction(Section, Year)),position="identity", #make the different year categories different colors
             size=7) + 
  
  #  geom_point(aes(shape=as.factor(Year), #make the different year categories different shapes
  #                fill = as.factor(Year)),position="identity", #make the different year categories different colors
  #           size=7)  +
  scale_shape_manual(values=c(21,21,21,21,22,22,22,22,24,24,24,24))+ #manually choses which shapes I wanted
  scale_fill_manual(values = c("darkorange", "lawngreen", "darkolivegreen", "red2", "darkorange", "lawngreen", "darkolivegreen", "red2", "darkorange", "lawngreen", "darkolivegreen", "red2"))+ #manually chooses which colors  
 # scale_x_continuous(breaks = c(0,5,10,15,20,25,30), position = 'top')+
  # tells the axis not to record all the numbers between the week #top moves axis labels to top of graph
  scale_linetype_manual(values=c("solid"))+ #manually chooses which line types I wanted
  
  labs(y="Depth (cm)", x="POM (g/kg)", cex=17) + #change text of axis labels
  ggtitle("POM by Section") + #title of plot
  
  
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

###############################################################################################

### Plot POM by year of only drained sections



drained <- summarySE(SOMData, measurevar="Pom_mass", groupvars = c("Depth.cm", "Year", "Treatment", "Section"))

drainedsum <- drained %>% dplyr::filter(Treatment == "Drained")

drainedsum <- within(drainedsum, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))
str(drainedsum)

ggplot(drainedsum, aes(x=Pom_mass, y=as.factor(Depth.cm), group=interaction(Year, Section)))+
  
  guides(shape=guide_legend(title="Year and Section")) + #format legend
  guides(fill=guide_legend(title="Year and Section")) + #format legend
  
  guides(shape=guide_legend(title="Year and Section")) + #format legend
  guides(fill=guide_legend(title="Year and Section")) + #format legend
  
  theme_bw()+
  geom_errorbar(aes(xmin=Pom_mass-se, xmax=Pom_mass+se), 
                #for a plot w/ Depth on y-axis,
                #use xmin and xmax instead of ymin and ymax
                colour="black", width=.1, position="identity")+
  #I think this "width" argument removes the horizontal lines at
  #the end of the error bar- not sure
  geom_path() + #connects points based on order
  
  
  geom_point(aes(shape=interaction(Section, Year), #make the different year categories different shapes
                 fill = interaction(Section, Year)),position="identity", #make the different year categories different colors
             size=7) + 
  
  #  geom_point(aes(shape=as.factor(Year), #make the different year categories different shapes
  #                fill = as.factor(Year)),position="identity", #make the different year categories different colors
  #           size=7)  +
  scale_shape_manual(values=c(21, 21, 22, 22, 24, 24))+ #manually choses which shapes I wanted
  scale_fill_manual(values = c("darkorange","darkolivegreen", "darkorange","darkolivegreen", "darkorange","darkolivegreen", "darkorange","darkolivegreen", "darkorange","darkolivegreen", "darkorange","darkolivegreen"))+ #manually chooses which colors  
 # scale_x_continuous(breaks = c(0,5,10,15,20,25,30), position = 'top')+
  # tells the axis not to record all the numbers between the week #top moves axis labels to top of graph
  scale_linetype_manual(values=c("solid"))+ #manually chooses which line types I wanted
  
  labs(y="Depth (cm)", x="POM (g/kg)", cex=17) + #change text of axis labels
  ggtitle("POM After Two Years in Drained Fields") + #title of plot
  
  
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


###############################################################################################

### Plot POM by year of only undrained sections

undrained <- summarySE(SOMData, measurevar="Pom_mass", groupvars = c("Depth.cm", "Year", "Treatment", "Section"))

undrainedsum <- undrained %>% dplyr::filter(Treatment == "Undrained")

undrainedsum <- within(undrainedsum, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))
str(undrainedsum)


ggplot(undrainedsum, aes(x=Pom_mass, y=as.factor(Depth.cm), group=interaction(Year, Section)))+
  
  guides(shape=guide_legend(title="Year and Section")) + #format legend
  guides(fill=guide_legend(title="Year and Section")) + #format legend
  
  guides(shape=guide_legend(title="Year and Section")) + #format legend
  guides(fill=guide_legend(title="Year and Section")) + #format legend
  
  theme_bw()+
  geom_errorbar(aes(xmin=Pom_mass-se, xmax=Pom_mass+se), 
                #for a plot w/ Depth on y-axis,
                #use xmin and xmax instead of ymin and ymax
                colour="black", width=.1, position="identity")+
  #I think this "width" argument removes the horizontal lines at
  #the end of the error bar- not sure
  geom_path() + #connects points based on order
  
  
  geom_point(aes(shape=interaction(Section, Year), #make the different year categories different shapes
                 fill = interaction(Section, Year)),position="identity", #make the different year categories different colors
             size=7) + 
  
  #  geom_point(aes(shape=as.factor(Year), #make the different year categories different shapes
  #                fill = as.factor(Year)),position="identity", #make the different year categories different colors
  #           size=7)  +
  scale_shape_manual(values=c(21,21,22,22,24,24))+ #manually choses which shapes I wanted
  scale_fill_manual(values = c("lawngreen", "red2", "lawngreen", "red2","lawngreen", "red2"))+ #manually chooses which colors  
#  scale_x_continuous(breaks = c(0,5,10,15,20,25,30), position = 'top')+
  # tells the axis not to record all the numbers between the week #top moves axis labels to top of graph
  scale_linetype_manual(values=c("solid"))+ #manually chooses which line types I wanted
  
  labs(y="Depth (cm)", x="POM (g/kg)", cex=17) + #change text of axis labels
  ggtitle("POM After Two Years in Undrained Fields") + #title of plot
  
  
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

###############################################################################################
###############################################################################################

### Plot POM for 2019 by treatment

pom2019sum <- treatmentsum %>% dplyr::filter(Year == "2019")

pom2019sum <- within(pom2019sum, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))

ggplot(pom2019sum, aes(x=Pom_percent, y=as.factor(Depth.cm), group=Year))+
  
  guides(shape=guide_legend(title="Year")) + #format legend
  guides(fill=guide_legend(title="Year")) + #format legend
  
  
  theme_bw()+
  geom_errorbar(aes(xmin=Pom_percent-se, xmax=Pom_percent+se), 
                #for a plot w/ Depth on y-axis,
                #use xmin and xmax instead of ymin and ymax
                colour="black", width=.1, position="identity")+
  #I think this "width" argument removes the horizontal lines at
  #the end of the error bar- not sure
  geom_path() + #connects points based on order
  
  
  geom_point(aes(shape=Year, #make the different year categories different shapes
                 fill = Year,position="identity", #make the different year categories different colors
                 size=7)) + 
  
  #  geom_point(aes(shape=as.factor(Year), #make the different year categories different shapes
  #                fill = as.factor(Year)),position="identity", #make the different year categories different colors
  #           size=7)  +
  scale_shape_manual(values=c(1,0,5,2,21,22,23,24,16,15,18,17))+ #manually choses which shapes I wanted
  scale_fill_manual(values = c("darkorange","darkolivegreen", "aquamarine", "deepskyblue", "blue",  "darkgoldenrod", "lawngreen", "red2", "darkgrey", "blueviolet", "burlywood", "deeppink"))+ #manually chooses which colors  
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30), position = 'top')+
  # tells the axis not to record all the numbers between the week #top moves axis labels to top of graph
  scale_linetype_manual(values=c("solid"))+ #manually chooses which line types I wanted
  
  labs(y="Depth (cm)", x="Percent POM of Total Soil Mass", cex=17) + #change text of axis labels
  ggtitle("Percent POM in 2019") + #title of plot
  
  
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


###############################################################################################
### Plot POM for 2020 by treatment

pom2020sum <- treatmentsum %>% dplyr::filter(Year == "2020")

pom2020sum <- within(pom2020sum, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))

ggplot(pom2020sum, aes(x=Pom_percent, y=as.factor(Depth.cm), group=Year))+
  
  guides(shape=guide_legend(title="Year")) + #format legend
  guides(fill=guide_legend(title="Year")) + #format legend
  
  
  theme_bw()+
  geom_errorbar(aes(xmin=Pom_percent-se, xmax=Pom_percent+se), 
                #for a plot w/ Depth on y-axis,
                #use xmin and xmax instead of ymin and ymax
                colour="black", width=.1, position="identity")+
  #I think this "width" argument removes the horizontal lines at
  #the end of the error bar- not sure
  geom_path() + #connects points based on order
  
  
  geom_point(aes(shape=Year, #make the different year categories different shapes
                 fill = Year,position="identity", #make the different year categories different colors
                 size=7)) + 
  
  #  geom_point(aes(shape=as.factor(Year), #make the different year categories different shapes
  #                fill = as.factor(Year)),position="identity", #make the different year categories different colors
  #           size=7)  +
  scale_shape_manual(values=c(1,0,5,2,21,22,23,24,16,15,18,17))+ #manually choses which shapes I wanted
  scale_fill_manual(values = c("darkorange","darkolivegreen", "aquamarine", "deepskyblue", "blue",  "darkgoldenrod", "lawngreen", "red2", "darkgrey", "blueviolet", "burlywood", "deeppink"))+ #manually chooses which colors  
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30), position = 'top')+
  # tells the axis not to record all the numbers between the week #top moves axis labels to top of graph
  scale_linetype_manual(values=c("solid"))+ #manually chooses which line types I wanted
  
  labs(y="Depth (cm)", x="Percent MAOM of Total Soil Mass", cex=17) + #change text of axis labels
  ggtitle("Percent POM in 2020") + #title of plot
  
  
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

###############################################################################################
### Plot POM for 2021 by treatment

pom2021 <- SOMData %>% dplyr::filter(Year == "2021")
pom2021sum<- summarySE(pom2021, measurevar = "Pom_percent", groupvars = c("Depth.cm", "Treatment"))

write.csv(pom2021sum, "POMafterrepspoint.csv", ##### CHANGE FILE NAME
          row.names = FALSE)


pom2021sum <- within(pom2021sum, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))

ggplot(pom2021sum, aes(x=Pom_percent, y=as.factor(Depth.cm), group=Treatment))+
  
  guides(shape=guide_legend(title="Treatment")) + #format legend
  guides(fill=guide_legend(title="Treatment")) + #format legend
  
  
  theme_bw()+
  geom_errorbar(aes(xmin=Pom_percent-se, xmax=Pom_percent+se), 
                #for a plot w/ Depth on y-axis,
                #use xmin and xmax instead of ymin and ymax
                colour="black", width=.1, position="identity")+
  #I think this "width" argument removes the horizontal lines at
  #the end of the error bar- not sure
  geom_path() + #connects points based on order
  
  
  geom_point(aes(shape=Treatment, #make the different year categories different shapes
                 fill = Treatment,position="identity", #make the different year categories different colors
                 size=7)) + 
  
  #  geom_point(aes(shape=as.factor(Year), #make the different year categories different shapes
  #                fill = as.factor(Year)),position="identity", #make the different year categories different colors
  #           size=7)  +
  scale_shape_manual(values=c(1,0,5,2,21,22,23,24,16,15,18,17))+ #manually choses which shapes I wanted
  scale_fill_manual(values = c("darkorange","darkolivegreen", "aquamarine", "deepskyblue", "blue",  "darkgoldenrod", "lawngreen", "red2", "darkgrey", "blueviolet", "burlywood", "deeppink"))+ #manually chooses which colors  
#  scale_x_continuous(breaks = c(0,5,10,15,20,25,30), position = 'top')+
  # tells the axis not to record all the numbers between the week #top moves axis labels to top of graph
  scale_linetype_manual(values=c("solid"))+ #manually chooses which line types I wanted
  
  labs(y="Depth (cm)", x="Percent POM of Total Soil Mass", cex=17) + #change text of axis labels
  ggtitle("Percent POM in 2021") + #title of plot
  
  
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

