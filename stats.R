GetTimeInGlucoseZones <- function(data){
  
  GetZone <- function(value){
    zone <- NULL
    for (i in rownames(zones))
      if (value >= zones[i, 'Min'] && value < zones[i, 'Max'])
      zone <- i
    
    zone
  }
  
  diffTime <- function(first, second){
    earlier <- as.POSIXct(strptime(first, "%Y-%m-%d %H:%M"))
    later <- as.POSIXct(strptime(second, "%Y-%m-%d %H:%M"))
    later - earlier
  }
  
  
  
  df <- zoo::fortify.zoo(data)
  
  ll.days <- unique(strptime(df[, 'Index'], format = '%Y-%m-%d'))
  result <- data.frame(
    A = rep(hms::as_hms('00:00:00'), length(ll.days)), 
    B = rep(hms::as_hms('00:00:00'), length(ll.days)),
    C = rep(hms::as_hms('00:00:00'), length(ll.days)),
    D = rep(hms::as_hms('00:00:00'), length(ll.days))
    )
  rownames(result) <- ll.days
  
  
  result.seconds <- result.percentage <- data.frame(
    A = rep(0, length(ll.days)), 
    B = rep(0, length(ll.days)),
    C = rep(0, length(ll.days)),
    D = rep(0, length(ll.days))
  )
  rownames(result.seconds) <- ll.days
  
  
  
  for (i in seq((nrow(df) - 1))){
    value <- df[i, ]$source
    zone <- GetZone(value)
    day <- strptime(df[i, ]$Index, format = '%Y-%m-%d')
    
    ind.day <- which(rownames(result) == day)
    #if(zone == 'A')
    #  browser()
    ind.zone <- which(colnames(result) == zone)

    #print(paste0(value, ' ', zone, ' ', day, ' ', ind.day, ' ', ind.zone, ' ', result[ind.day, ind.zone]))
    result[ind.day, ind.zone] <- hms::as_hms(result[ind.day, ind.zone] + 60)
    result.seconds[ind.day, ind.zone] <- result.seconds[ind.day, ind.zone] + 60
    }

  result.percentage <- round(prop.table(as.matrix(result.seconds),1),2)
  
  list(
    time = result,
    percentage = result.percentage
  )
}





FindGlucoseRushes <- function(df){
  for (i in seq((nrow(df) - 5))){
    setVal <- df[seq(from=i, to=i+5, by=1), 'source']
    diffval <- min(setVal)
    
    #print(paste0(value, ' ', zone, ' ', day, ' ', ind.day, ' ', ind.zone, ' ', result[ind.day, ind.zone]))
    result[ind.day, ind.zone] <- hms::as_hms(result[ind.day, ind.zone] + 60)
    result.seconds[ind.day, ind.zone] <- result.seconds[ind.day, ind.zone] + 60
  }
  
}