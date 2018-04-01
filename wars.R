
dayCount <- function(war){
  return(365*(war[8]-war[5]) + 30*(war[6]-war[3]) + (war[7]-war[4])) 
}


compareDates <- function(date1, date2){
  #date1 has always a lower starting date than date2
  if(date1[1,3]>date2[1,3])
    return (TRUE)
  else if (date1[1,3]<date2[1,3])
    return(FALSE) 
  else if (date1[1,1]>date2[1,1])
    return(TRUE)
  else if (date1[1,1]<date2[1,1])
    return(FALSE)
  return(date1[1,2]>=date2[1,2])
}
mergeWars <- function(war1, war2){
  war <- war1
  if(!compareDates(war1[6:8],war2[6:8]))
    war[6:8] <- war2[6:8]
  return(war)
}


getContinuousPeriods <- function (orderedWars ){
  continuousPeriods <- orderedWars[0,]
  considered <- 0
  for(i in 1:length(orderedWars[[1]])){
    j <- i+considered
    if(j > length(orderedWars[[1]]))
      break
    continuousPeriods <- rbind(continuousPeriods, orderedWars[j,])
    activeIndex <- length(continuousPeriods[[1]])
    j <- j+1
    if(j > length(orderedWars[[1]]))
      break
    while(orderedWars[j,1]==continuousPeriods[activeIndex,1]){
      if(compareDates(continuousPeriods[activeIndex,c(6:8)], orderedWars[j,c(3:5)])){
        continuousPeriods[activeIndex,] <- mergeWars(continuousPeriods[activeIndex,],orderedWars[j,])
        j <- j+1
        considered <- considered +1
      }
      else break
    }
  }
  return(continuousPeriods)
}


getTotalDaysFought <- function(orderedWars, COW_country_codes) {
  
  continuousPeriods <- getContinuousPeriods(orderedWars)
  
  continuousPeriods$dayCount <- dayCount(continuousPeriods)
  
  totalDaysFought <-  aggregate(continuousPeriods$dayCount, by = list(ccode = continuousPeriods$ccode), FUN = sum)
  
  totalDaysFought <- merge.data.frame(COW_country_codes, totalDaysFought, by.x = "CCode", by.y = "ccode") 
  
  totalDaysFought<-`colnames<-`(totalDaysFought, c("ccode","Abbrevation", "Country Name", "Days fought"))
  totalDaysFought["Years Fought"] <- totalDaysFought$`Days fought`/365
  
  
  totalDaysFought$'rounded' <- round(totalDaysFought$`Years Fought`)
  return(totalDaysFought)
}
#assumes csv files are read with RStudio's import dataset utilities

COW_country_codes <- unique(COW_country_codes)
wars <- Inter_StateWarData_v4_0[ , c(4,5, 7:12)]
wars <- rbind(wars, setNames(Inter_StateWarData_v4_0[!(-8==Inter_StateWarData_v4_0[,18]) , c(4,5, 13:18)], names(wars)) )

wars <- rbind(wars, setNames(Extra_StateWarData_v4_0[!(-8==Extra_StateWarData_v4_0[,13]) , c(4,5, 8:13)], names(wars)) )
wars <- rbind(wars, setNames(Extra_StateWarData_v4_0[!(-8==Extra_StateWarData_v4_0[,19]) , c(4,5, 14:19)], names(wars)) )
wars <- rbind(wars, setNames(Extra_StateWarData_v4_0[!(-8==Extra_StateWarData_v4_0[,13]) , c(6,7, 8:13)], names(wars)) )
wars <- rbind(wars, setNames(Extra_StateWarData_v4_0[!(-8==Extra_StateWarData_v4_0[,19]) , c(6,7, 14:19)], names(wars)) )

wars <- rbind(wars, setNames(Intra_StateWarData_v4_1[!(-8==Intra_StateWarData_v4_1[,14]) , c(4,5, 9:14)], names(wars)) )
wars <- rbind(wars, setNames(Intra_StateWarData_v4_1[!(-8==Intra_StateWarData_v4_1[,20]) , c(4,5, 15:20)], names(wars)) )
wars <- rbind(wars, setNames(Intra_StateWarData_v4_1[!(-8==Intra_StateWarData_v4_1[,14]) , c(6,7, 9:14)], names(wars)) )
wars <- rbind(wars, setNames(Intra_StateWarData_v4_1[!(-8==Intra_StateWarData_v4_1[,20]) , c(6,7, 15:20)], names(wars)) )
wars<-na.omit(wars)
wars <- wars[wars[,1]!=-8,]
wars[wars==-9] <- 1
wars[wars[,8]==-7,c(6:8)] <- data.frame(12,31,2007)
#wars that are not ended by 2007 are conisdered ended in the last day of 2007
orderedWars <- wars[ order( as.data.frame(wars[,1]) , as.data.frame(wars[,5]) , as.data.frame(wars[,3]) ,as.data.frame(wars[,4])) , ]

totalDaysFought <- getTotalDaysFought(orderedWars, COW_country_codes)

