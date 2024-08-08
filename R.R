
BuildData  <- function(){
glucose.file <- file.path('../../../Documents/Personnel/Suivi Glycemie/glucose.csv')
lines <- readLines(glucose.file)
lines <- lines[-c(1:2)]

date <- date.tags <- NULL
source <- source.tags <- NULL

splitted <- NULL
for (i in seq(length(lines))){
  splitted <- unlist(strsplit(lines[i], split = ','))
  .date <- strptime(splitted[3], format="%d-%m-%Y %H:%M")
  .date <- as.character(as.POSIXct(.date, format = "%Y-%m-%d %H:%M"))
  type.info <- as.numeric(splitted[4])
  glycemie <- as.numeric(splitted[5])
  num.glycemie <- as.numeric(splitted[6])
  tags <- splitted[14]
  
  if (type.info == 0){
    date <- c(date, .date)
    source <- c(source, as.numeric(glycemie))
  } else if (type.info == 1){
    date.exists <- which(date == .date)
    
    if(is.null(date.exists)){
      date <- c(date, .date)
      source <- c(source, as.numeric(num.glycemie))
  } else{
      source[date.exists] <- as.numeric(num.glycemie)
    }
  }
  
   if (type.info == 6 && tags != ''){
     date.tags <- c(date.tags, .date)
     source.tags <- c(source.tags, tags)
   }

}


df.glycemie <- data.frame(date = date, source = source)
df.glycemie <- df.glycemie[order(df.glycemie$date),]
new.data <- Discretise(df.glycemie)
df.glycemie <- rbind(df.glycemie, new.data)
df.glycemie <- df.glycemie[order(df.glycemie$date),]


#Compute rushes
rushes <- rep(0, nrow(df.glycemie))
th <- 10

for (i in seq((nrow(df.glycemie) - 5))){
  diffval <- as.numeric(df.glycemie[i+5, 'source']) - as.numeric(df.glycemie[i, 'source'])
  
  if (abs(diffval) >= th)
    rushes[i] <- as.numeric(diffval)
}

df.glycemie <- cbind(df.glycemie, rushes = rushes)
df.glycemie <- df.glycemie[order(df.glycemie$date),]


df.glycemie_xts <- xts::xts(df.glycemie[-1],
  order.by = as.POSIXct(
    df.glycemie$date,
    format="%Y-%m-%d %H:%M"))

df.tags <- data.frame(date = date.tags, source = source.tags)


return(list(Glycemie = df.glycemie_xts, Tags = df.tags))
}



ComputeLineEq <- function(t1, v1, t2, v2){
  # Convert times in seconds
  
  a <- (v2 - v1)/as.integer(difftime(t2, t1, units="secs"))
  b <- v1
  
  list(a = a, b = b)
}


Discretise <- function(df){
  
  date <- source <- c()
  
  for (i in seq(nrow(df)-1)){
    
    first <- df[i, ]
    second <- df[i+1, ]
    
    value1 <- first$source
    value2 <- second$source
    day1 <- strptime(first$date, format = '%Y-%m-%d')
    day2 <- strptime(second$date, format = '%Y-%m-%d')
    time1 <- as.POSIXct(first$date, format = '%Y-%m-%d %H:%M')
    time2 <- strptime(second$date, format = '%Y-%m-%d %H:%M')
    
    nbMinutes <- abs(as.integer(difftime(time1, time2, units="mins")))
    
    coeff <- ComputeLineEq(
      t1 = time1, 
      v1 = value1, 
      t2 = time2, 
      v2 = value2)
    
    if (nbMinutes > 1) {
      for (mins in seq(nbMinutes-1)){
        new.time <- time1 + mins * 60
        new.value <- coeff$a * (mins * 60) + coeff$b
        date <- c(date, format(as.POSIXct(new.time, format = '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M:%S'))
        source <- c(source, new.value)
      }
    }
  }
  
  additionalData <- data.frame(date = date, source = source)
  additionalData
}


GetMeanPerDay <- function(data){
  df <- zoo::fortify.zoo(data)
  df$day <- format(df$Index, "%Y-%m-%d")
  aggregate(source ~ day, df, mean)
}

GetMeanPerHour <- function(data){
  df <- zoo::fortify.zoo(data)
  df$hour <- format(df$Index, "%H")
  aggregate(source ~ hour, df, mean)
}

GetVariancePerDay <- function(data){
  df <- zoo::fortify.zoo(data)
  df$day <- format(df$Index, "%d")
  aggregate(source ~ day, df, sd)
}

GetVariancePerHour <- function(data){
  df <- zoo::fortify.zoo(data)
  df$hour <- format(df$Index, "%H")
  aggregate(source ~ hour, df, sd)
}