# Script testing the robustnes of the t-test

### THESE ARE THE PARAMETERS YOU WILL BE CHANGING ###

## POPULATION 1
MEAN1 = 1  # Mean of population 1, should be positive!
SD1 = 1  # Standard deviation of population 1
SAMPLE_SIZE1 = 5  # Number of samples you are drawing from the population
SKEWED1 = FALSE  # Either TRUE or FALSE

## POPULATION 2
MEAN2 = 1  # Mean of population 2, should be positive!
SD2 = 1  # Standard deviation of population 2
SAMPLE_SIZE2 = 5  # Number of samples you are drawing from the population
SKEWED2 = FALSE  # Either TRUE or FALSE

### DO NOT ENTER ANYTHING BELOW THIS LINE
#############################################################
#  All the code below this line is the simulation

## Make plots of the populations

vals = seq(-20, 20, len=100)

par(mfrow=c(1,2))

if(!SKEWED1){
    plot(vals, dnorm(vals, mean=MEAN1, sd=SD1), type="l", ylim=c(0, 0.5),
                    main="Shape of population 1", ylab="Density", xlab="x", col="red")
} else{
    plot(vals, dexp(vals, rate=1 / MEAN1), type="l", ylim=c(0, 0.5),
                    main="Shape of population 1", ylab="Density", xlab="x", col="red")
}

if(!SKEWED2){
    plot(vals, dnorm(vals, mean=MEAN2, sd=SD2), type="l", ylim=c(0, 0.5),
                    main="Shape of population 2", ylab="Density", xlab="x", col="blue")
} else{
    plot(vals, dexp(vals, rate=1 / MEAN2), type="l", ylim=c(0, 0.5),
                    main="Shape of population 2", ylab="Density", xlab="x", col="blue")
}


## RUN THE SIMULATION
SIM=5000  # Number of simulations
alpha = 0.05  # TYPE I ERROR RATE

if(!SKEWED1){
    pop1_results = matrix(rnorm(SIM * SAMPLE_SIZE1, mean=MEAN1, sd=SD1),
                            nrow=SAMPLE_SIZE1, ncol=SIM)
} else{
    pop1_results = matrix(rexp(SIM * SAMPLE_SIZE1, rate=1 / MEAN1),
                            nrow=SAMPLE_SIZE1, ncol=SIM)

}

if(!SKEWED2){
    pop2_results = matrix(rnorm(SIM * SAMPLE_SIZE2, mean=MEAN2, sd=SD2),
                            nrow=SAMPLE_SIZE2, ncol=SIM)
} else{
    pop2_results = matrix(rexp(SIM * SAMPLE_SIZE2, rate=1 / MEAN2),
                            nrow=SAMPLE_SIZE2, ncol=SIM)

}

significant = array(NA, dim=SIM)
for(i in 1:SIM){

    p_val = t.test(pop1_results[,i], pop2_results[,i], var.equal=TRUE)$p.value
    significant[i] = p_val < alpha

}

# PRINT YOUR RESULTS
if(MEAN1 == MEAN2){
    print(paste("Your true TYPE I ERROR RATE is:", sum(significant) / SIM))
} else {
    print(paste("Your TYPE II ERROR RATE is: ",
                    (length(significant) - sum(significant)) / SIM))
}




