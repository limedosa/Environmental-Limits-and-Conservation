tryingSnow<- function(Precip, Temp, S, Albedo, SWE){
  # SWE<-0
  snowfall<- numeric()
  rainfall<- numeric()
  snowMelt <- numeric()
  # sumPrecipitation<- 0
  
  totalSnowFall <- numeric()
  totalRainFall<- numeric()
  totalSublimation<- numeric()
  Time <- 365
  
  for(t in 1:Time){
    tempToday<- Temp[t] 
    solarRaditionToday<- S[t]
    sublimation<- (.043*tempToday)+.03*(1-Albedo) * solarRaditionToday
    totalSublimation<- totalSublimation+ sublimation
    # snowMelt[t]<-(1.2*tempToday)+.226*(1-Albedo)*solarRaditionToday#DOTESNT WORK
    # print(snowMelt[t])
    if(tempToday<=0){
      totalSnowFall<- totalSnowFall + Precip[t]
      snowfall[t]<- Precip[t]
      rainfall[t]<- 0
      snowMelt[t] <- 1.2 * Temp[t] + 0.226 * (1 - Albedo) * S[t]
      
      # snowMelt[t]<-(1.2*Temp[t])+.226*(1-Albedo)*S[t] #THIS DOESNT WORK
      
    }
    else{
      totalRainFall <- totalRainFall + Precip[t]
      snowfall[t]<- 0
      rainfall[t]<- Precip[t]
      # snowMelt[t]<-(1.2*Temp[t])+.226*(1-Albedo)*S[t]
      snowMelt[t] <- 1.2 * Temp[t] + 0.226 * (1 - Albedo) * S[t]
      
    }
    # sumPrecipitation<- totalRainFall + totalSnowFall + totalSublimation + SWE[t]
    SWE[t+1]<- SWE[t] + snowfall[t] - snowMelt[t] - sublimation
  }
  SWE <- SWE[1:Time]
  
  return (list(SWE=SWE, snowMelt=snowMelt, rainfall=rainfall))
}
