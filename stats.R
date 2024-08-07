GetTimeInGlucoseZones <- function(xxx){
  
  GetZone <- function(value){
    zone <- NULL
    for (i in rownames(zones))
      if (value >= zones[i, 'Min'] && value <= zones[i, 'Max'])
      zone <- i
    
    zone
  }
  
  diffTime <- function(first, second){
    earlier <- as.POSIXct(strptime(first, "%Y-%m-%d %H:%M"))
    later <- as.POSIXct(strptime(second, "%Y-%m-%d %H:%M"))
    later - earlier
  }
  
  
  toto <- function(data){
    browser()
    timeduration <- setNames(rep('00:00', 4), nm = LETTERS[seq(4)])
    
  }
  
  df <- zoo::fortify.zoo(data)
  
  ll.days <- unique(strptime(df[, 'Index'], format = '%Y-%m-%d'))
  timeduration <- setNames(rep('00:00', 4), nm = LETTERS[seq(4)])
  
  
  for (i in (nrow(df) - 1)){
    first <- df[i, ]
    second <- df[i+1, ]
    
    value1 <- first$source
    value2 <- second$source
    zone1 <- GetZone(value1)
    zone2 <- GetZone(value2)
    day1 <- strptime(first$Index, format = '%Y-%m-%d')
    day2 <- strptime(second$Index, format = '%Y-%m-%d')
    # 
    if (day1 == day2){
      if (zone1 == zone2){
      duration <- day2 - day1
      
      } else {
        
      }
      
    } else {
      
    }
    
    
  }
  
  
}