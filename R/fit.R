#' @title Convert a FIT file
#' @export
ConvertFitFile <- function(path, name){
  options(xts_check_TZ = FALSE)
  fit <- read.csv(file.path(path, name), header = TRUE, sep = ',')
  filename <- strsplit(name, split = '-record.csv')[[1]][1]
  filename <- gsub('-', '_', filename)

  #Add two hours
  convertTime <- function(ts){
    ts <- as.character(ts)
    date <- unlist(lapply(strsplit(ts, split = 'T'), function(x) x[1]))
    mytime <- unlist(lapply(strsplit(ts, split = 'T'), function(x) x[2]))
    time <- unlist(lapply(strsplit(as.character(mytime), split = '.000Z'),
      function(x)x[1]))
    
    myts <- paste0(date, ' ', time)
    myts <- as.POSIXct(format(myts, format = '%Y-%m-%d %H:%M:%S'), tz = 'Europe/Paris')
    myts <- myts + 2 * 60 * 60
    myts
  }
 
  date <- convertTime(fit$timestamp)

   fit <- cbind(date, fit)
  fit <- fit[-2]
   fit_xts <- xts::xts(fit[-1], order.by = date)
   
   ind <- which(second(index(fit_xts)) == 0)
   fit_xts <- fit_xts[ind,]
   
   fit_xts
}