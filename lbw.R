lbw <- read.csv("/Users/eliecertrillos/Desktop/lbw_tutorial/lbw.csv",header = T)
install.packages("dplyr")
library(dyplr)
lbw<- lbw %>% arrange(percent_poverty)
barplot(lbw$percent_poverty,names.arg=lbw$county,
        cex.names=0.5,las=3,main="Poverty Percentage by County",
        ylim=c(0,30))
hist(lbw$percent_poverty,
     main ="Histogram of poverty",
     col="beige",
     breaks=20,
     xlab="Poverty Rate")
lbw<-lbw %>% arrange(total_pop)
lbw <- lbw %>% mutate(white_per = white / total_pop *100,
                       black_per = black / total_pop *100,
                       other_per = other / total_pop *100,
                       lbw_per = lbw_births / live_births*100)
plot(lbw$percent_poverty,lbw$lbw_per,
     main = "Relationship between\n Porverty percent vs lbw rate",
     xlab = "Poverty percent rate",
     ylab = "lbw rate", pch =21,bg = "red")
plot(lbw$white_per,lbw$lbw_per,
     main = "Relationship between\n White percent vs lbw rate",
     xlab = "White percent rate",
     ylab = "lbw rate", pch =21,bg = "purple")
plot(lbw$black_per,lbw$lbw_per,main = "Relationship between\n Black percent vs lbw rate",
     xlab = "Black percent rate",
     ylab = "lbw rate", pch =21,bg = "blue")
fit <- lm(lbw_per ~ black_per + percent_poverty, data = lbw)
summary(fit)
par(mfrow=c(2,2))
plot(fit)
confint(fit)
