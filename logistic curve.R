##Import Data
dat = read.csv("F:/Dropbox/Projects/ROS papers/Quantile Regression Manuscript/Rainout shelter field plan.csv",header = T)
dat = subset(dat, population!="b")
str(dat)

plot(ndvi~day,data = dat,xlim=c(0,100),ylim=c(0,1))  #plot data

nls.fit <- nls(ndvi ~ SSlogis(day, Asym, xmid, scal), data=dat)
summary(nls.fit)

# The model function
# E(y|day) = Asym/(1+exp((Day_mid-Day)/scal))
# create a dummy range of that we use to predict leaf temperature from our fitted model
predict_range <- data.frame(day = seq(0, 100, length = 500))

# calculate for each x-range value the corresponding y-range
my.line <- within(predict_range, ndvi <- predict(nls.fit, newdata = predict_range))

# add the line to the existing graph
# This line represents the "mean" fit, no quantile regression involved
lines(ndvi ~ day, data = my.line, col = "red")

require(quantreg)
# Non-linear quantile regression
# aiming for the upper 95% quantile
start <- list(Asym=coef(nls.fit)[1], xmid=coef(nls.fit)[2], scal=coef(nls.fit)[3])
my.rq <- nlrq(ndvi ~ SSlogis(day, Asym, xmid, scal),
              data = dat,
              start = start,
              tau = 0.975)
summary(my.rq)
# calculating the values from the model
my.line975 <- within(predict_range, 
                    ndvi <- predict(my.rq, 
                                    newdata = predict_range))

lines(ndvi ~ day, data = my.line975, col = "blue")
# re-doing the 2.5% quantile
# nlrq can be supplied with a list of "tau" values, to get multiple quantiles in one go.
# For now I'm doing one by one only...

my.rq2.5 <- nlrq(ndvi ~ SSlogis(day, Asym, xmid, scal),
              data = dat,
              start = start,
              tau = 0.025)
summary(my.rq2.5)
# calculating the values from the model
my.line2.5 <- within(predict_range, 
                     ndvi <- predict(my.rq2.5, 
                                     newdata = predict_range))

lines(ndvi ~ day, data = my.line2.5, col = "green")
# Add a legend to the graph
legend(85, 0.9, 
       c("Mean", "97.5%", "2.5%"),
       lty = c(rep("solid", 3)),
       col = c("red", "blue", "green"),
       cex = 0.8) # make the characters slightly smaller than standard text 
