ReadFit <- function(path){
  fit <- read.csv(path, header = TRUE, sep = ',')
  
  
  #Add two hours
  addTime <- function(ts, addSeconds){
    ts <- as.character(ts)
    date <- unlist(lapply(strsplit(ts, split = 'T'), function(x) x[1]))
    mytime <- unlist(lapply(strsplit(ts, split = 'T'), function(x) x[2]))
    time <- unlist(lapply(strsplit(as.character(mytime), split = '.000Z'),
      function(x)x[1]))
    
    myts <- paste0(date, ' ', time)
    myts <- as.POSIXct(strptime(myts, format = '%Y-%m-%d %H:%M:%S'), format = '%Y-%m-%d %H:%M:%S') + addSeconds
    myts
  }
  
  #Fix timezone
   fit <- cbind(fit, mydate = addTime(fit$timestamp, 2*60*60))

  
  fit
}