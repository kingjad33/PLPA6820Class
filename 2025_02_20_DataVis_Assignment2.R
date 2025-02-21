library(tidyverse)
library(ggrepel)
library(ggpubr)

#read in data
mycodata = read.csv("MycotoxinData.csv", na.strings = "na")
View(mycodata)
str(mycodata)
mycodata$Treatment=as.factor(mycodata$Treatment)
mycodata$Cultivar=as.factor(mycodata$Cultivar)
str(mycodata)

#Question 1- create boxplot with needed characteristics
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
colorchoice= c("#56B4E9","#009E73")
graph1= ggplot(mycodata, aes(x=Treatment, y=DON, fill=Cultivar)) + #fill ensures whol box is same color
  geom_boxplot(position=position_dodge(0.5),outlier.color = "NA") + #outlier NA ensures outlier points are not in the graph
  xlab("") + ylab("DON (ppm)") + #axis labels
  geom_point(pch=21, alpha=0.6, position=position_jitterdodge(dodge.width=0.9)) + #pch is shape type and alpha is point transparency
  scale_fill_manual(values= colorchoice)+ #tell R which colors to use
  theme_classic()+
  facet_wrap(~Cultivar) #creates different graphs for each cultivar
graph1

#Question 2
mycodata$Treatment2= factor(mycodata$Treatment, levels= c("NTC","Fg","Fg + 37","Fg + 40","Fg + 70")) #new column with correct order
graph2= ggplot(mycodata, aes(x=Treatment2, y=DON, fill=Cultivar)) + #uses corrected order
  geom_boxplot(position=position_dodge(0.5),outlier.color = "NA") +
  xlab("") + ylab("DON (ppm)") +
  geom_point(pch=21, alpha=0.6, position=position_jitterdodge(dodge.width=0.9)) +
  scale_fill_manual(values= colorchoice)+
  theme_classic()+
  facet_wrap(~Cultivar)
graph2

#Question 3- change the y axis from DON 
graph3= ggplot(mycodata, aes(x=Treatment2, y=X15ADON, fill=Cultivar)) +
  geom_boxplot(position=position_dodge(0.5),outlier.color = "NA") +
  xlab("") + ylab("15ADON") +
  geom_point(pch=21, alpha=0.6, position=position_jitterdodge(dodge.width=0.9)) +
  scale_fill_manual(values= colorchoice)+
  theme_classic()+
  facet_wrap(~Cultivar)
graph3

graph4= ggplot(mycodata, aes(x=Treatment2, y=MassperSeed_mg, fill=Cultivar)) +
  geom_boxplot(position=position_dodge(0.5),outlier.color = "NA") +
  xlab("") + ylab("Seed Mass (mg)") +
  geom_point(pch=21, alpha=0.6, position=position_jitterdodge(dodge.width=0.9)) +
  scale_fill_manual(values= colorchoice)+
  theme_classic()+
  facet_wrap(~Cultivar)
graph4

#Question 4- put all graphs together, 3 column and 1 row- label = ABC, common legend=T
ggarrange(graph2,graph3,graph4, ncol=3, nrow=1, common.legend = TRUE)

#Question 5- create plots with geom_pwc
graph5= graph2 + geom_pwc(aes(group=Treatment2, method= "t_test", label= "p.adj.format"))
graph6= graph3 + geom_pwc(aes(group=Treatment2, method= "t_test", label= "p.adj.format"))
graph7= graph4 + geom_pwc(aes(group=Treatment2, method= "t_test", label= "p.adj.format"))
ggarrange(graph5,graph6,graph7, ncol=3, nrow=1, common.legend = TRUE)
