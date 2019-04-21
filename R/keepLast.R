#' Extract the last observation of each time series
#'
#' This function the last observation of each time series from PreSens logging data
#'
#' @details Set the working directory to the directory of PreSens data using \code{\link[base]{setwd}}
#' @author Chih-Lin Wei <chihlinwei@@gmail.com>
#' @export
#' @examples
#' keepLast()

# Function to get last observation of each time series measurements
keepLast <- function(){
  f <- dir(pattern=".csv")
  out <-NULL
  for(j in 1:length(f)){
    d <- read.csv(f[j], skip=1, sep=";", nrows=0) %>% subset(!is.na(Value))
    m <- c(d$delta_t[-1], d$delta_t[length(d$delta_t)])-d$delta_t # Time difference between measurement
    k <- which(m>60) # Time series at least 1 hours apart
    k <- c(k, length(m)+1)
    s <- rep(1, k[1]-1) # Label each continuous time series
    if(length(k)>1){
      for(i in 2:length(k)){
        o <- rep(i, k[i]-k[i-1])
        s <- c(s, o)
      }
      d$Measurement <- factor(s)
    } else d$Measurement <- factor(s)

    # Extract the last measurement of each continuous time series
    d <- cbind(Sample=gsub("_", "-", gsub(".csv", "", f[j])),
               splitBy(~Measurement, data=d) %>% lapply(FUN=function(x)x[dim(x)[1],]) %>% ldply(.id = "Measurement")
    )
    out <- rbind(out, d)
  }
  return(out)
}

