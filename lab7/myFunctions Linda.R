

mean<- function(num1, num2){
    return((num1+num2)/2) 
}


fizzbuzz<- function(num1){
    if (num1%% 5==0){
        return ('fizz')
    }
}


# albedo not defined 
# no plotting 
#TODO: add values for snowmelt & sublimation for the whole yr 
snowModel <- function(Precip, Temp, S, Albedo, Start.SWE) {
    dates = length(Temp)
    SWE <- numeric(length(Temp) + 1)
    SWE[1] <- Start.SWE
    snowfall <- numeric(length(Temp))
    rainfall <- numeric(length(Temp))
    snowMelt <- numeric(length(Temp))
    sublimation <- numeric(length(Temp))
    
    for (t in 1:length(Temp)) {
        if (Temp[t] > 0) {
            snowfall[t] <- 0
            rainfall[t] <- Precip[t]
            snowMelt[t] <- (1.2) * Temp[t] + (0.226) * (1 - Albedo) * S[t]
        } else {
            snowfall[t] <- Precip[t]
            rainfall[t] <- 0
            snowMelt[t] <- 0
        }
        
        sublimation[t] <- (0.043) * Temp[t] + (0.03) * (1 - Albedo) * S[t]
        if (sublimation[t] < 0) {
            sublimation[t] <- 0
        }
        
        if (snowMelt[t] < 0) {
            snowMelt[t] <- 0
        }
        
        if ((snowMelt[t] + sublimation[t]) > SWE[t]) {
            sublimation[t] <- 0
            snowMelt[t] <- SWE[t]
        }
        
        SWE[t + 1] <- SWE[t] + snowfall[t] - snowMelt[t] - sublimation[t]
    }
    
    SWE <- SWE[1:length(Temp)]
    return(list(SWE = SWE, snowMelt = snowMelt, rainfall = rainfall))
}
