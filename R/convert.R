#' @title BuildData
#' @export
#' @import tidyverse
#' @import highcharter
#' @import xts
#' 
BuildData  <- function(filepath, offset = 20){
  
  options(xts_check_TZ = FALSE)
  
  path <- '../../../Documents/Personnel/Suivi Glycemie'
  filepath <- file.path(path, 'glucose.csv')
lines <- readLines(filepath)
lines <- lines[-c(1:2)]

date <- date.tags <- NULL
df.glycemie <- source.tags <- type.tags <- NULL


message('Build original dataset')
splitted <- NULL
for (i in seq(length(lines))){
  splitted <- unlist(strsplit(lines[i], split = ','))
  .date <- as.character(splitted[3])
  tmp <- unlist( strsplit(.date, "[-\\s \\s:]+"))
    year <- as.character(tmp[3])
  month <- as.character(tmp[2])
  day <- as.character(tmp[1])
  hour <- as.character(tmp[4])
  minute <-as.character(tmp[5])
  .date <- paste0(year, '-', month, '-', day, ' ', hour, ':', minute)

  type.info <- as.numeric(splitted[4])
  glycemie.val <- as.numeric(splitted[5])
  num.glycemie <- as.numeric(splitted[6])
  tags <- splitted[14]
 
  if (type.info == 0){
    date <- c(date, .date)
    df.glycemie <- c(df.glycemie, as.numeric(glycemie.val))
  } else if (type.info == 1){
    date.exists <- which(date == .date)
    
    if(is.null(date.exists)){
      date <- c(date, .date)
      df.glycemie <- c(df.glycemie, as.numeric(num.glycemie))
  } else{
    df.glycemie[date.exists] <- as.numeric(num.glycemie)
    }
  }
  
   if (type.info == 6 && tags != ''){
     date.tags <- c(date.tags, .date)
     source.tags <- c(source.tags, tags)
   }

}

date <- as.POSIXct(format(date, format = '%Y-%m-%d %H:%M'), tz = 'Europe/Paris')

date <- date - offset

supersapiens <- data.frame(date, glycemie = df.glycemie)


date.tags <- as.POSIXct(format(date.tags, format = '%Y-%m-%d %H:%M'), tz = 'Europe/Paris')
supersapiens.tags <- data.frame(date = date.tags, tags = source.tags)


supersapiens <- supersapiens[order(supersapiens$date),]

message('Discretise dataset...')
new.data <- Discretise(supersapiens)
supersapiens <- rbind(supersapiens, new.data)


#add columns to this final data.frame
message('Adding new columns to dataset...')
supersapiens <- cbind(supersapiens, 
  rushes.pos = rep(0, nrow(supersapiens)),
  rushes.neg = rep(0, nrow(supersapiens))
  )

supersapiens.tags <- cbind(supersapiens.tags,
  tag.type = rep('', nrow(supersapiens.tags)),
  tag.description = rep('', nrow(supersapiens.tags)),
  tag.description2 = rep('', nrow(supersapiens.tags))
)



message('Convert dataset to xts format...')
supersapiens <- xts::xts(supersapiens[-1],  order.by = date)
supersapiens.tags <- xts::xts(supersapiens.tags[-1], 
  order.by = supersapiens.tags$date)


# message('Fixing missing values in tags...')
# supersapiens.tags <- Fix_tags(supersapiens.tags)

#Compute rushes
message('Compute rushes...')
supersapiens <- AddRushes(supersapiens)


message('Ordering dataset by datetime...')

supersapiens_fit <- BuildFitData()

list.days <- unlist(lapply(supersapiens, function(x)
  format(index(x), format = '%Y-%m-%d')))
list.days <- unique(list.days)
ll.days <- list.days[order(list.days)]

supersapiens_timeInZonesHR <- GetTimeInHRZones(ll.days, supersapiens_fit)


supersapiens_hc_raw <- Build_hc_RawData(supersapiens, supersapiens_fit)
supersapiens_hc_raw <- AddAnnotations(supersapiens_hc_raw, supersapiens.tags)

supersapiens_meanPerDay <- GetMeanPerDay(supersapiens$glycemie)
supersapiens_meanPerHour <- GetMeanPerHour(supersapiens$glycemie)
supersapiens_variancePerDay <- GetVariancePerDay(supersapiens$glycemie)
supersapiens_variancePerHour <- GetVariancePerHour(supersapiens$glycemie)
supersapiens_timeInZones <-  GetTimeInGlucoseZones(supersapiens$glycemie)
supersapiens_pga <- Compute_PGA(supersapiens$glycemie)
supersapiens_heatmapPerHour <- GetHeatmapPerHour(supersapiens$glycemie)


supersapiens_hc_meanPerDay <- view_MeanPerDay(supersapiens_meanPerDay)
supersapiens_hc_meanPerHour <- view_MeanPerHour(supersapiens_meanPerHour)
supersapiens_hc_variancePerDay <- view_VariancePerDay(supersapiens_variancePerDay)
supersapiens_hc_variancePerHour <- view_VariancePerHour(supersapiens_variancePerHour)
supersapiens_hc_timeInZones <-  view_timeInGlucoseZones(supersapiens_timeInZones, 
  supersapiens_timeInZonesHR$result.seconds)
supersapiens_hc_pga <- view_hc_pga(supersapiens_pga)
supersapiens_hc_heatmapPerHour <- view_heatmapPerHour(supersapiens_heatmapPerHour)
supersapiens_hc_view_wholeRushes <- view_wholeRushes(supersapiens)


save(
  supersapiens,
  supersapiens_meanPerDay,
  supersapiens_meanPerHour,
  supersapiens_variancePerDay,
  supersapiens_variancePerHour,
  supersapiens_timeInZones,
  supersapiens_timeInZonesHR,
  supersapiens_pga,
  supersapiens_heatmapPerHour,
  supersapiens.tags,
  supersapiens_hc_raw, 
  supersapiens_hc_meanPerDay, 
  supersapiens_hc_meanPerHour,
  supersapiens_hc_variancePerDay, 
  supersapiens_hc_variancePerHour,
  supersapiens_hc_timeInZones,
  supersapiens_hc_pga,
  supersapiens_hc_heatmapPerHour,
  supersapiens_hc_view_wholeRushes,
  supersapiens_hc_raw,
  file = 'data/supersapiens.RData')

}


#' @title Add rushes information
#' @examples
#' NULL
#' 
#' @export

AddRushes <- function(df.xts){
  th <- 10
  for (i in seq((nrow(df.xts) - 5))){
    diffval <- as.numeric(df.xts[i+5, 'glycemie']) - as.numeric(df.xts[i, 'glycemie'])
    maxval <- max(df.xts[i:i+5, 'glycemie'])
    minval <- min(df.xts[i:i+5, 'glycemie'])
    
    if (diffval >= th && maxval > Zones()[3, 'Max'])
      df.xts$rushes.pos[i] <- as.numeric(diffval)
    else if (diffval <= -th && minval < Zones()[2, 'Min'])
      df.xts$rushes.neg[i] <- as.numeric(diffval)
  }
  df.xts
}


#' @title BuildFitData
#' @examples
#' BuildFitData()
#' 
#' @export
BuildFitData <- function(){
  options(xts_check_TZ = FALSE)
  path <- system.file('extdata/Fit', package = 'mySuperSapiens')
  fit.files <- list.files(path)
  fit.files <- fit.files[grepl('-record.csv', fit.files)]

  lapply(fit.files, function(x) ConvertFitFile(path, x))

}

#' @title BuildFitData
#' @export
ComputeLineEq <- function(t1, v1, t2, v2){
  # Convert times in seconds
  
  a <- (v2 - v1)/as.integer(difftime(t2, t1, units="secs"))
  b <- v1
  
  list(a = a, b = b)
}

#' @title BuildFitData
#' @export
Discretise <- function(df){
  
  date <- glycemie <- c()
  

  for (i in seq(nrow(df)-1)){
    
    first <- df[i, ]
    second <- df[i+1, ]
    
    value1 <- first$glycemie
    value2 <- second$glycemie
    time1 <- index(first)
    time2 <- index(second)
    
    nbMinutes <- abs(as.integer(difftime(time1, time2, units = "mins")))
    
    coeff <- ComputeLineEq(
      t1 = time1, 
      v1 = as.numeric(value1), 
      t2 = time2, 
      v2 = as.numeric(value2)
      )
    
    if (nbMinutes > 1) {
      for (mins in seq(nbMinutes - 1)){
        new.time <- time1 + mins * 60
        new.value <- coeff$a * (mins * 60) + coeff$b
        #browser()
        date <- c(date, format(as.POSIXct(new.time, format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M'))
        #date <- c(date, as.POSIXct(new.time, format = '%Y-%m-%d %H:%M', tz = 'Europe/Paris'))
        glycemie <- c(glycemie, new.value)
      }
    }
  }
  
  #date <- as.POSIXct(format(date, format = '%Y-%m-%d %H:%M'), tz = 'Europe/Paris')
  #browser()
  additionalData <- data.frame(date, glycemie)
 
  additionalData
}

#' #' @title BuildFitData
#' #' @export
Fix_tags <- function(df){
  df[15, 'tag.type'] <- 'Alimentation'


  df
}