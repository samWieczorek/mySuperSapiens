#' @title Convert a FIT file
#' @export
ConvertFitFile <- function(path, name){
  fit <- read.csv(file.path(path, name), header = TRUE, sep = ',')
  filename <- strsplit(name, split = '-record.csv')[[1]][1]
  filename <- gsub('-', '_', filename)

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
   fit <- cbind(fit, date = addTime(fit$timestamp, 2*60*60))

   fit_xts <- xts::xts(fit[-1],
     order.by = as.POSIXct(
       fit$date,
       format="%Y-%m-%d %H:%M:%S"))
   
   assign(filename, fit_xts)
   eval(parse(text = paste0("save('", filename, "', file = 'data/",filename,".RData')")))
}