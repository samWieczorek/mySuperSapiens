#' @title BuildData
#' @export
BuildData  <- function(filepath, timeoffset = 20){
  
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
  #browser()
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
# basic data.frame
df.supersapiens <- data.frame(date, glycemie = df.glycemie)


df.supersapiens <- df.supersapiens[order(df.supersapiens$date),]

message('Discretise dataset...')
new.data <- Discretise(df.supersapiens)


df.supersapiens <- rbind(df.supersapiens, new.data)
#df.supersapiens <- df.supersapiens[order(df.supersapiens$date),]


#add columns to this final data.frame
message('Adding new columns to dataset...')
df.supersapiens <- cbind(df.supersapiens, 
  rushes.pos = rep(0, nrow(df.supersapiens)),
  rushes.neg = rep(0, nrow(df.supersapiens))
  )
message('Convert dataset to xts format...')
df.supersapiens <- xts::xts(df.supersapiens[-1],  order.by = date)


# ,
#   tag.type = rep('', nrow(df.supersapiens)),
#   tag.description = rep('', nrow(df.supersapiens)),
#   tag.description2 = rep('', nrow(df.supersapiens))
#   )

# 
# message('Adding tags to dataset...')
# for (d in seq(length(date.tags))){
#   ind <- which(df.supersapiens$date == date.tags[d])
#   df.supersapiens[ind, 'tag.description'] <- source.tags[d]
# }



# message('Fixing missing values in tags...')
# df.supersapiens <- Fix_tags(df.supersapiens)

#Compute rushes
message('Compute rushes...')

th <- 10


for (i in seq((nrow(df.supersapiens) - 5))){
  diffval <- as.numeric(df.supersapiens[i+5, 'glycemie']) - as.numeric(df.supersapiens[i, 'glycemie'])
  
  if (diffval >= th)
    df.supersapiens$rushes.pos[i] <- as.numeric(diffval)
  else if (diffval <= -th)
    df.supersapiens$rushes.neg[i] <- as.numeric(diffval)
}



message('Ordering dataset by datetime...')
#df.supersapiens <- df.supersapiens[order(df.supersapiens$date),]


supersapiens_meanPerDay <- GetMeanPerDay(df.supersapiens$glycemie)
supersapiens_meanPerHour <- GetMeanPerHour(df.supersapiens$glycemie)
supersapiens_variancePerDay <- GetVariancePerDay(df.supersapiens$glycemie)
supersapiens_variancePerHour <- GetVariancePerHour(df.supersapiens$glycemie)

supersapiens_timeInZones <-  GetTimeInGlucoseZones(df.supersapiens$glycemie)

supersapiens_pga <- Compute_PGA(df.supersapiens$glycemie)
supersapiens_heatmapPerHour <- GetHeatmapPerHour(df.supersapiens$glycemie)

save(
  supersapiens, 
  supersapiens_meanPerDay, 
  supersapiens_meanPerHour,
  supersapiens_variancePerDay, 
  supersapiens_variancePerHour,
  supersapiens_timeInZones,
  supersapiens_pga,
  supersapiens_heatmapPerHour,
  file = 'data/supersapiens.RData')

}


#' @title BuildFitData
#' @examples
#' BuildFitData()
#' 
#' @export
BuildFitData <- function(){
  
  path <- '../../../Documents/Personnel/Suivi Glycemie/'

  ll.fit <- list.files(path)
  ll.fit <- ll.fit[grepl('-record.csv', ll.fit)]

  for(f in ll.fit)
    ConvertFitFile(path, f)

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

#' @title BuildFitData
#' @export
Fix_tags <- function(df){
  df[15, 'tag.type'] <- 'Alimentation'
  
  
  df
}