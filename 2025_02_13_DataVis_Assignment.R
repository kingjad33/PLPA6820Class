#Read in data
myco = read.csv("MycotoxinData.csv", na.strings = "na")
str(myco)
View(myco)
#####Packages#####
library(ggplot2)

#####Question 2#####
#DON = y
#Treatment = x
#Cultivar = color
#Build boxplot with data
ggplot(myco, aes(x=Treatment, y=DON, color = Cultivar)) + 
  geom_boxplot() +
  ylab("DON (ppm)") +
  xlab("")

#####Question 3#####
ggplot(myco, aes(x=Treatment, y=DON, color = Cultivar)) + 
  stat_summary(fun=mean,geom="bar") +
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  geom_point() +
  ylab("DON (ppm)") +
  xlab("")

#####Question 4#####
#boxplot, add jitter, shape to 21, and outline black
ggplot(myco, aes(x=Treatment, y=DON, fill = Cultivar)) + 
  geom_boxplot(outlier.color = "NA") +
  ylab("DON (ppm)") +
  xlab("") +
  geom_point(pch=21,color="black", position=position_jitterdodge(dodge.width=0.9)) 

#barplot, add jitter, shape to 21, and outline black
ggplot(myco, aes(x=Treatment, y=DON, fill = Cultivar)) + 
  stat_summary(fun=mean,geom="bar", position="dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  geom_point(pch=21,color="black", position=position_jitterdodge(dodge.width=0.9))  +
  ylab("DON (ppm)") +
  xlab("")

#####Question 5#####
#boxplot, color
cbbPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
colorchoice = c("#F0E442","#E69F00")

ggplot(myco, aes(x=Treatment, y=DON, fill = Cultivar)) + 
  geom_boxplot(outlier.color = "NA") +
  scale_fill_manual(values=colorchoice) +
  ylab("DON (ppm)") +
  xlab("") +
  geom_point(pch=21,color="black", position=position_jitterdodge(dodge.width=0.9)) 

#####Question 6#####
#boxplot, add facet
ggplot(myco, aes(x=Treatment, y=DON, fill = Cultivar)) + 
  geom_boxplot(outlier.color = "NA") +
  scale_fill_manual(values=colorchoice) +
  ylab("DON (ppm)") +
  xlab("") +
  geom_point(pch=21,color="black", position=position_jitterdodge(dodge.width=0.9)) +
  facet_wrap(~Cultivar)

#Barplot
ggplot(myco, aes(x=Treatment, y=DON, fill = Cultivar)) + 
  stat_summary(fun=mean,geom="bar", position="dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  geom_point(pch=21,color="black", position=position_jitterdodge(dodge.width=0.9))  +
  ylab("DON (ppm)") +
  xlab("") +
  facet_wrap(~Cultivar)


#####Question 7#####
#boxplot, transparency with alpha
ggplot(myco, aes(x=Treatment, y=DON, fill = Cultivar)) + 
  geom_boxplot(outlier.color = "NA") +
  scale_fill_manual(values=colorchoice) +
  ylab("DON (ppm)") +
  xlab("") +
  geom_point(pch=21,color="black",alpha=0.4, position=position_jitterdodge(dodge.width=0.9)) +
  facet_wrap(~Cultivar)

#barplot
ggplot(myco, aes(x=Treatment, y=DON, fill = Cultivar)) + 
  stat_summary(fun=mean,geom="bar", position="dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  geom_point(pch=21,color="black",alpha=0.4, position=position_jitterdodge(dodge.width=0.9))  +
  ylab("DON (ppm)") +
  xlab("") +
  facet_wrap(~Cultivar)

#####Question 8#####
#line plot
ggplot(myco, aes(x=Treatment, y=DON, fill = Cultivar)) + 
  geom_dotplot(binaxis="y", stackgroups = TRUE, binwidth=8, method="histodot",
               position=position_jitterdodge()) +
  scale_fill_manual(values=colorchoice) +
  ylab("DON (ppm)") +
  xlab("") +
  facet_wrap(~Cultivar)

#I would choose to use the bar plot because it gives a better overview of the data and is 
#easy to view and understand. The bar graph had too much going on, and it was harder to view the data as easy.

#Completed






