# If you need help with this code, contact Kyle Sherbine at sherb065@umn.edu or ask Anna Cates or Lindsay
# Pease for the most up to date way to reach me.

# This code provides and example of how to (1) average analytical lab replicates of POM (and MAOM) samples, 
# (2) how to graph the data, (3) setting up a linear mixed model and analyzing data with Anova

# If you want to change POM percent into a mass per mass basis, multiply the percent by 10 (line of code below)
# pom$Pom_mass<-pom$Pom_percent*10

# A result of 12% POM of the total soil mass examined (Pom_percent = 12), is equivalent to 120 g/kg soil
# This means that for every kg of soil, there is 120 grams (or 0.12 kg) of POM


library(ggplot2)
library(Rmisc)
library(magrittr)
library(dplyr)
library(car)


# Load/read in data and call it pom

# add file name/ location to the line below
# YOU WILL NEED TO EDIT THE LINE OF CODE BELOW BEFORE RUNNING IT!!!!!!!!!!!

pom <- read.delim(".csv", sep = ",")

# average analytical replicates; Pom_percent is the name of the variable that you measured
# the groupings are the other variables that will dictate how Pom_percent is summarized
# for example, if you only listed "Year", all data collected in that given year would be reported as one number
#  regardless of other variables
# you will need to know the names of the columns for this!

pomsum <- summarySE(pom, measurevar = "Pom_percent", groupvars = c("Year", "Depth.cm", "Treatment", 
                                                                    "Point", "Section", "Season"))

str(pomsum)  # at this point the three analytical reps have been averaged together


# if you want to created a csv of the averaged replicates, run the code directly below

write.csv(pomsum, "Pom_Percent_RepsAveraged.csv", ##### CHANGE FILE NAME
          row.names = FALSE)



###########################       Graph Pom_percent by variables of interest    ############################

# summarize your data by the variables of interest
# in this case the variables we care about are Year, Depth, and Treatment
# you can save this as a new object or override the previous pomsum object

pomsum <- summarySE(pom, measurevar = "Pom_percent", groupvars = c("Year", "Depth.cm", "Treatment"))


# this code reverses the order of the y-axis
# this is relevant if you are interested in having surface depths located at the top of the graph
#      and have decreasing depth along the y-axis

pomsum <- within(pomsum, Depth.cm<- ordered(Depth.cm, levels = rev(sort(unique(Depth.cm)))))  


# create a plot

ggplot(pomsum, aes(x=Pom_percent, y=as.factor(Depth.cm), group=interaction(Treatment, Year)))+
  
  guides(shape=guide_legend(title="Treatment and Year")) + #format legend
  guides(fill=guide_legend(title="Treatment and Year")) + #format legend
  
  guides(shape=guide_legend(title="Treatment and Year")) + #format legend
  guides(fill=guide_legend(title="Treatment and Year")) + #format legend
  
  theme_bw()+
 
   geom_errorbar(aes(xmin=Pom_percent-se, xmax=Pom_percent+se), 
                 # for a plot with depth on the y-axis, use xmin and xmax instead of ymin and ymax
        # create errorbars with the standard error (se) that is calculated with the summarySE() function
                
        colour="black", width=.1, position="identity")+
        # I think the width argument is the length of the ends of the errorbars
 
  geom_path() + #connects points based on order
  
  
  geom_point(aes(shape=interaction(Treatment, Year), #make the different year categories different shapes
                 fill = interaction(Treatment, Year)),position="identity", size=7) +
             #make the different year categories different colors
              
  
  scale_shape_manual(values=c(21,21,22,22,24,24))+ #manually choses which shapes I wanted
          # Because the shape corresponds to the interaction of Treatment and Year you need to have a value
          # for each unquie combination
          # For example, if you have three years and two treatments, you would have six unique combinations
          # and therefore would need to provide six values
  
  
 
   scale_fill_manual(values = c( "deepskyblue2",  "darkgoldenrod","deepskyblue2", "darkgoldenrod", 
                                "deepskyblue2", "darkgoldenrod"))+ 
    #manually chooses which colors those shapes will be filled with.  Similar to shape, you need to provide
    # the same number of values(colors) to the argument.  In this case, the blue corresponds to one treatment
    # and the gold corresponds to the other.  Year is differentiated by shape
  
  
#  scale_x_continuous(breaks = c(0,5,10,15,20,25,30), position = 'top')+
# the line of code above allows you to rescale one axis or the other
  # the position='top' arugment moved the labels on the axis to the top of the graph as opposed to the bottom
  
  

  scale_linetype_manual(values=c("solid"))+ 
  #manually chooses which line types
 
   
  labs(y="Depth (cm)", x="POM (% of soil mass)", cex=17) + #change text of axis labels

    ggtitle("Title") + #title of plot
  
  
# the rest of the code for the plot relates to the theme  
   
  theme(
    panel.grid.major = element_blank(), #removes major gridlines
    panel.grid.minor = element_blank(), #removes minor gridlines
    panel.background = element_blank(), #removes gridlines
    axis.line = element_line(colour = "black"), 
    axis.text.x = element_text(size=16), #size of x axis text
    axis.text.y = element_text(size=16, angle=45), #size of y axis text and angle text at 45 degree angle
    axis.title = element_text(size= 21), # size of title text
    legend.title= element_text(size = 15), #size of legend title
    legend.text = element_text(size = 15), #size of legend text
    plot.title = element_text(hjust=0.5, size=24)) #size of plot title and centered


###########################################  Example Statistics   ####################################


#Normality tests
ggdensity(pomsum$Pom_percent)
ggqqplot(pomsum$Pom_percent)

# If the data need to be transformed run the code below
# Transformation type may need to be changed depending how data are distrubuted

pomsum$Pom_percent_transformed <- log(pomsum$Pom_percent)
ggdensity(pomsum$Pom_percent_transformed)
ggqqplot(pomsum$Pom_percent_transformed)


# Mixed Effects Model - POM
# fixed: treatment, depth, year
#random: point nested in block
# repeated measure of point nested in block over time (year)
POMmodel <- lmer(Pom_percent_transformed ~ Treatment * Year * Depth.cm + (1|Block/Point) +
                   (Year|Block/Point), data = pomsum)

car::Anova(POMmodel, test = "F", type=3)

cld(emmeans(POMmodel, pairwise ~ Depth.cm), Letters = LETTERS)
cld(emmeans(POMmodel, pairwise ~ Treatment), Letters = LETTERS)
cld(emmeans(POMmodel, pairwise ~ Year), Letters = LETTERS)
emmeans(POMmodel, pairwise ~ Year*Treatment*Depth.cm)


