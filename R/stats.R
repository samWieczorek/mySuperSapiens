

#' @export
GetTimeInGlucoseZones <- function(data){
  message('GetTimeInGlucoseZones()...')
  
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
    value <- as.numeric(df[i, ]$glycemie)
    zone <- GetZone(value)
    day <- strptime(df[i, ]$Index, format = '%Y-%m-%d')
    
    ind.day <- which(rownames(result) == day)

     # browser()
    ind.zone <- which(colnames(result) == zone)

    #print(paste0(i, ' ', value, ' ', zone, ' ', day, ' ', ind.day, ' ', ind.zone, ' ', result[ind.day, ind.zone]))
    #if (i == 18)
    #  browser()
    
    result[ind.day, ind.zone] <- hms::as_hms(result[ind.day, ind.zone] + 60)
    result.seconds[ind.day, ind.zone] <- result.seconds[ind.day, ind.zone] + 60
    }

  result.percentage <- round(prop.table(as.matrix(result.seconds),1),2)
  list(
    time = result,
    percentage = result.percentage
  )
}




#' @export
FindGlucoseRushes <- function(df, th = 10){
  message('FindGlucoseRushes()...')
  date <- rushes <- NULL
  df <- zoo::fortify.zoo(df)
  df$glycemie <- as.numeric(df$glycemie)
  
  for (i in seq((nrow(df) - 5))){
    diffval <- as.numeric(df[i+5, 'glycemie']) - as.numeric(df[i, 'glycemie'])

    if (abs(diffval) >= th)
      rushes <- c(rushes, as.numeric(diffval))
    else
      rushes <- c(rushes, 0)
 }
  rushes
}

#' @export
Compute_PGA <- function(df){
  message('Compute_PGA()...')
  df <- zoo::fortify.zoo(df)
  df$hour <- as.numeric(format(df$Index, "%H"))
  df$glycemie <- as.numeric(df$glycemie)
  
  
  #group by hours in time column and calculate sum of sales
  #pga <- aggregate(as.numeric(glycemie) ~ hour, df, quantile)
  
  
  pga <- df %>%
    group_by(hour) %>%
    summarize(
      Q5 = quantile(glycemie)[1],
      Q25 = quantile(glycemie)[2],
      Q50 = quantile(glycemie)[3],
      Q75 = quantile(glycemie)[4],
      Q95 = quantile(glycemie)[5]
    )
  
  pga
}



#' @export
GetMeanPerDay <- function(data){
  message('GetMeanPerDay()...')
  df <- zoo::fortify.zoo(data)
  df$day <- format(df$Index, "%Y-%m-%d")
  df$glycemie <- as.numeric(df$glycemie)
  
  res <- aggregate(glycemie ~ day, df, mean)
  res[,'glycemie'] <- round(res[,'glycemie'], 2)
  res
}


#' @export
GetMeanPerHour <- function(data){
  message('GetMeanPerHour()...')
  df <- zoo::fortify.zoo(data)
  df$hour <- format(df$Index, "%H")
  df$glycemie <- as.numeric(df$glycemie)
  
  res <- aggregate(glycemie ~ hour, df, mean)
  res[,'glycemie'] <- round(res[,'glycemie'], 2)
  res
}


#' @export
GetVariancePerDay <- function(data){
  message('GetVariancePerDay()...')
  df <- zoo::fortify.zoo(data)
  df$day <- format(df$Index, "%d")
  df$glycemie <- as.numeric(df$glycemie)
  
  res <- aggregate(glycemie ~ day, df, sd)
  res[,'glycemie'] <- round(res[,'glycemie'], 2)
  res
}


#' @export
GetVariancePerHour <- function(data){
  message('GetVariancePerHour()...')
  df <- zoo::fortify.zoo(data)
  df$hour <- format(df$Index, "%H")
  df$glycemie <- as.numeric(df$glycemie)
  
  res <- aggregate(glycemie ~ hour, df, sd)
  res[,'glycemie'] <- round(res[,'glycemie'], 2)
  res
}


#' @title Heatmap per day and hour
#' @export
GetHeatmapPerHour <- function(df){
  df <- zoo::fortify.zoo(df)
  df$hour <- format(df$Index, "%H")
  df$day <- format(df$Index, "%Y-%m-%d")
  df$glycemie <- as.numeric(df$glycemie)
  
  res2 <- aggregate(glycemie~ day + hour, df, mean)
  res2[, 'glycemie'] <- round(res2[, 'glycemie'], 1)
  res2 <- cbind(res2, zone = unlist(lapply(res2[, 'glycemie'], function(x) GetZone(x))))
  res2
}