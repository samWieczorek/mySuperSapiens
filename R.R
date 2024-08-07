
BuildData  <- function(){
glucose.file <- file.path('../../../Documents/Personnel/Suivi Glycemie/glucose.csv')
lines <- readLines(glucose.file)
lines <- lines[-c(1:2)]

date <- date.tags <- NULL
source <- source.tags <- NULL

splitted <- NULL
for (i in seq(length(lines))){
  #print(lines[i])
  #browser()
  splitted <- unlist(strsplit(lines[i], split = ','))
  #print(splitted[3])
  #.date <- as.POSIXct(splitted[3], format="%d-%m-%Y %H:%M")
  #print(.date)
  .date <- splitted[3]
  type.info <- as.numeric(splitted[4])
  glycemie <- as.numeric(splitted[5])
  num.glycemie <- as.numeric(splitted[6])
  tags <- splitted[14]
  
  if (type.info == 0){
    date <- c(date, .date)
    source <- c(source, as.numeric(glycemie))
  } else if (type.info == 1){
    #browser()
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
df.glycemie_xts <- xts(df.glycemie[-1], 
  order.by = as.POSIXct(
    df.glycemie$date, 
    format="%d-%m-%Y %H:%M"))

df.tags <- data.frame(date = date.tags, source = source.tags)


return(list(Glycemie = df.glycemie_xts, Tags = df.tags))
}





GetMeanPerDay <- function(data){
  df <- zoo::fortify.zoo(data)
  df$day <- format(df$Index, "%d")
  aggregate(source ~ day, df, mean)
}


GetVariancePerDay <- function(data){
  df <- zoo::fortify.zoo(data)
  df$day <- format(df$Index, "%d")
  aggregate(source ~ day, df, sd)
}