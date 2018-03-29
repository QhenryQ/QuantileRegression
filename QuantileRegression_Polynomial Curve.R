setwd("C:/Users/Henry Qu/Dropbox/Projects/ROS papers/Quantile Regression Manuscript")   
##Import Data
dat = read.csv("Rainout shelter field plan.csv",header = T)
dat = subset(dat, population!="b")
str(dat)

plot(ndvi~day,data = dat)  #plot data

#fit a polynomial equation to the data
my.equation <- ndvi ~ a * day^2 + b * day  + c

# fit the equation to the data via "non-linear least squares"
nls.fit <- nls(my.equation,
               data = dat,
               start = list(a = 2, b = 3, c = 5))

# look at the result
summary(nls.fit)


# create a dummy range of that we use to predict leaf temperature from our fitted model
predict_range <- data.frame(day = seq(10, 70, length = 500))

# calculate for each x-range value the corresponding y-range
my.line <- within(predict_range, ndvi <- predict(nls.fit, newdata = predict_range))

# add the line to the existing graph
# This line represents the "mean" fit, no quantile regression involved
lines(ndvi ~ day, data = my.line, col = "red")

require(quantreg)
# Non-linear quantile regression
# aiming for the upper 95% quantile
my.rq <- nlrq(my.equation,
              data = dat,
              start = list(a = 2, b = 2, c = 5),
              tau = 0.95)
summary(my.rq)
# calculating the values from the model
my.line95 <- within(predict_range, 
                    ndvi <- predict(my.rq, 
                                    newdata = predict_range))

lines(ndvi ~ day, data = my.line95, col = "blue")

# re-doing the 90% quantile
# nlrq can be supplied with a list of "tau" values, to get multiple quantiles in one go.
# For now I'm doing one by one only...

my.rq.90 <- nlrq(my.equation,
                 data = dat,
                 start = list(a = 2, b = 2, c = 5),
                 tau = .90)
summary(my.rq.90)

# the usual calculation of the corresponding line
my.line90<- within(predict_range, 
                   ndvi <- predict(my.rq.90, 
                                   newdata = predict_range))

lines(ndvi ~ day, data = my.line90, col = "green")

# Add a legend to the graph
legend(20, 0.5, 
       c("Mean", "95%", "90%"),
       lty = c(rep("solid", 3)),
       col = c("red", "blue", "green"),
       cex = 0.8) # make the characters slightly smaller than standard text 

# get the data to put lines on the grpah
# combine the two data sets
my.line90$quantile = c("90%")
my.line95$quantile = c("95%")
quantile.reg <- rbind(my.line90, my.line95)
# ggplot Graph:
library(ggplot2)
ggplot(dat, aes(x = day, y = ndvi))+
  geom_point()+
  geom_line(data = quantile.reg,aes(linetype = quantile))+
  theme_bw()

#fit model to differnt populations
#first define a function that fit the model and output model summary
multifit = function(data){
  my.rq <- nlrq(my.equation,
                data = dat,
                start = list(a = 2, b = 2, c = 5),
                tau = 0.95)
  return(summary(my.rq))
}

lapply(split(dat$ndvi,dat$population),multifit)
