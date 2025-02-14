# IntroR-Assignment
#IntroR assignment for Rep Science Class

z = c(1:200) #create vector 1 to 200
mean(z) #mean = 100.5
sd(z) #sd = 57.87918

#logical vector of z>30 is true, all else = false
zlog= ifelse(z>30,TRUE,FALSE)

#create dataframe with z and zlog
zdf= data.frame(z, zlog)
View(zdf) #ensure dataframe is correct

#change column names
colnames(zdf)= c("zvec","zlogic")
View(zdf) #ensure dataframe is correct

#create new colum in zdf that is = zvec squared
zdf$zsquared = (zdf$zvec)^2
View(zdf) #ensure dataframe is correct

#subset dataframe to greater than 10 and less than 100
#with subset()
subset(zdf, zsquared>10 & zsquared<100)
#without subset- , at end is necessary to complete
zdf[zdf$zsquared >10 & zdf$zsquared <100,]

#subset dataframe to get only row 26 ([row,column])
zdf[26,]

#subset dataframe to get only column zsquared (column 3) in row 180 ([row,column])
zdf[180,3]

