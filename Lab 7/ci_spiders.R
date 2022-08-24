
# Values for the linear model
beta0 = -25
beta1 = 0.6
sigma = sqrt(733)

### Change these parameters ###
MAX.SAMP = 1000
jump = 10

# Different sample sizes of spider webs
sampsize = seq(3, MAX.SAMP, jump)

# Spider nest height of interest
spider.nest = 155

# Set up arrays
cidiffs = array(NA, dim=length(sampsize))
preddiffs = array(NA, dim=length(sampsize))

# Loop through all my different sample sizes
for(i in 1:length(sampsize)){

    # Simulate Data
    heights = runif(sampsize[i], 90, 400)
    obs = rnorm(sampsize[i], mean=beta0 + beta1 * heights,sigma)
    data = data.frame(x=heights, y=obs)

    # Set my spider nest values
    #vals = rep(spider.nest, sampsize[i])

    # Fit a linear model
    fit = lm(y~x, data=data)

    # Get mean and prediction intervals
    ci = predict(fit, newdata=data.frame(x=spider.nest), se=T, interval="confidence")
    cidiffs[i] =  ci$fit[1,3] - ci$fit[1,2]

    pred = predict(fit, newdata=data.frame(x=spider.nest), se=T, interval="prediction")
    preddiffs[i] = pred$fit[1,3] - pred$fit[1,2]

    # Keep a small samples size
    if(i == 4){
        vals = seq(min(heights), max(heights), 100)
        full_ci = predict(fit, newdata=data.frame(x=vals), se=T, interval="confidence")
        full_pred = predict(fit, newdata=data.frame(x=vals), se=T, interval="prediction")
        temp_heights = heights
        temp_obs = obs
    }

}

par(mfrow=c(2,2))

# plot the Max Number
matplot(vals, full_ci$fit, lty=c(1, 2, 2), col=c(1, 2, 2), type="l",
            xlab="Nest Height", ylab="Num. Spiders", main="Example interval about the mean",
            ylim = c(0, 300))
points(temp_heights, temp_obs, cex=0.1)

# plot the Max Number
matplot(vals, full_pred$fit, lty=c(1, 2, 2), col=c(1, 2, 2), type="l",
            xlab="Nest Height", ylab="Num. Spiders", main="Example prediction interval\nabout an individual",
            ylim=c(0, 300))
points(temp_heights, temp_obs, cex=0.1)

# Plot the difference between the prediction and mean intervals
plot(sampsize, log(cidiffs), pch=19, col='red', ylim=c(0,15), cex=0.5,
    xlab=" Number of spider webs sampled",
    ylab="Log Width of Interval (Upper - Lower)", main="Log width of prediction interval\nand mean interval as sample size increases")
points(sampsize, log(preddiffs), pch=19, col='black', cex=0.5)

legend(MAX.SAMP * .3, 15, c("Width of mean 95% CI",
    "Width of 95%\nPrediction Interval"), col =c("red", "black"),
    pch=c(19,19), border="", cex=.8)
