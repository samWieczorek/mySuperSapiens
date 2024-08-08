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





FindGlucoseRushes <- function(df, th = 10){
  date <- rushes <- NULL
  df <- zoo::fortify.zoo(df)
  for (i in seq((nrow(df) - 5))){
    diffval <- as.numeric(df[i+5, 'source']) - as.numeric(df[i, 'source'])

    if (abs(diffval) >= th)
      rushes <- c(rushes, as.numeric(diffval))
    else
      rushes <- c(rushes, 0)
 }
  rushes
}


Compute_PGA <- function(df){
  df <- zoo::fortify.zoo(df)
  df$hour <- format(df$Index, "%H")
  #group by hours in time column and calculate sum of sales
  pga <- aggregate(source ~ hour, df, quantile)
  #
  pga <- df %>%
    group_by(hour) %>%
    #summarize(sum_source = sum(source)) %>%
    summarize(
      quantile_5 = quantile(source)[1],
      quantile_25 = quantile(source)[2],
      quantile_50 = quantile(source)[3],
      quantile_75 = quantile(source)[4],
      quantile_95 = quantile(source)[5]
    )
  
  colnames(pga) <- c('hour', 'Q5', 'Q25', 'Q50', 'Q75', 'Q95')
  pga
  
  
}