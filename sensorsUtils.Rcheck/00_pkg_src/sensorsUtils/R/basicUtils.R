#' Create a fortified simply dataframe with two columns: date and value.
#'
#' @param df dataframe with a column "dates" in POSIXct format an another column with any value type.
#' @param fInterval a 2-item vector with 'start' and 'end' POSIXct dates respectively. All the values in the given interval will be explicitdly
#' included in the resulting dataframe with NAs. By default the limit dates are extracted from the data.
#' @param by a character string, containing the data increment in terms of "sec", "min", "hour", "day", "DSTday", "week", "month", "quarter" or "year".
#'  This can optionally be preceded by a (positive or negative) integer and a space, or followed by "s".
#'  See \code{\link[base]{seq.POSIXt}} for more info. The increment provided must equivalent to the real data-time increment.
#' @return A fortified dataframe, i.e with all the lost values explicited. The resulting time index its perfectly
#' defined by \code{seq{start,end,by}}, so the dataframes created with this function will have the exactly same index.
#' @author JV Yago
#'
#' @importFrom magrittr %>%
#' @import utils
fortify.col <- function(df, fInterval=NULL, by){
  if (missing(fInterval)){
    fInterval = c(head(df$dates,1), tail(df$dates,1))
  }
  start = fInterval[1]
  end   = fInterval[2]
  seq(start, end, by = by) %>% as.data.frame() -> canonical.df
  canonical.df$VAL <- NA
  colnames(canonical.df) <- c("dates", "VAL")

  df <- df[df$dates>=start & df$dates<=end,]
  col.fortified <- merge(df, canonical.df, by = "dates", all.y = T)[,c(1:ncol(df))]
  return(col.fortified)
}

#' Create a fortified dataframe.
#'
#' Takes a dataframe with silent lost values and makes it explicit with NAs with the help
#' of a security interval defined by \code{fInterval}.
#'
#' @param df a list of dataframes with a dates column in \code{\link[base]{POSIXct}} format an another column with any value type.
#'        It can be a single dataframe with multiple columns too.
#' @param fInterval a 2-item vector with 'start' and 'end' POSIXct dates respectively. All and only all the values in the given interval will be explicitdly
#' included in the resulting dataframe, with NAs in case of missing observations in the input dataframe.  By default the limit dates are extracted from the data, in this case 'start' and end'
#' its the minimun and maximun dates for \strong{all} the dataframes given in \code{dfList}.
#' @param by a character string, containing the data increment in terms of "sec", "min", "hour", "day", "DSTday", "week", "month", "quarter" or "year".
#'  This can optionally be preceded by a (positive or negative) integer and a space, or followed by "s".
#'  See \code{\link[base]{seq.POSIXt}} for more info. The increment provided must equivalent to the real data-time increment.
#' @param names a vector of names for the dataframe \strong{values}. If a character vector are not provided the columns will be renamed with capital letters.
#' @return A fortified dataframe, i.e with all the lost values explicited. The resulting time index its perfectly
#' defined by \code{seq(start,end,by)}, so the columns of the dataframe created with this function will have the same index,
#' i.e the silent lost values of the inputs dataframes (the columns in the new one) will be marked with NAs in the right positions.
#' @author JV Yago
#' @export
#' @examples
#'
#' #One column dataframe with silent lost observations
#' start <- as.POSIXct("2019/01/01")
#' end   <- as.POSIXct("2019/01/02")
#' dates <- seq(start, end, by = "hour")
#' df.simple <- data.frame(dates = dates[-(sample(1:25,5))], values=rep(1,20))
#' fortify_df(df=df.simple, by="hour")
#'
#' #Multiple column dataframe with silent lost observations
#' df.complex <- data.frame(dates = dates[-(sample(1:25,5))], A = 1:20, B = LETTERS[1:20], C = rnorm(1:20))
#' fortify_df(df=df.complex, by="hour")
#'
#' # Multiple datafrems with silent lost observations
#' df.A <- data.frame(dates = dates[-(sample(1:25,5))], a = 1:20)
#' df.B <- data.frame(dates = dates[-(sample(1:25,3))], a = 1:22)
#' df.C <- data.frame(dates = dates[-(sample(1:25,7))], a = 1:18)
#' fortify_df(list(df.A, df.B, df.C), by = "hour")
#' @importFrom magrittr %>%
#' @import utils
fortify_df<-function(df, fInterval, by, names){

  if (class(df) == "list"){
    if(missing(fInterval)){
      datesLists<-lapply(df, '[',1) %>%
        do.call(what=rbind.data.frame) -> alldates
      fInterval = c(min(alldates[,1]), max(alldates[,1]))
    }
    fortifiedCols<-lapply(df, function(x){
      return(fortify.col(x, fInterval, by))
    })

    fortifiedIndex <- fortifiedCols[[1]][,1]

    fortifiedCols<-lapply(fortifiedCols, function(x){
      return(x[,2])
    })

    fortifiedDf<-cbind(fortifiedIndex,
                       do.call(cbind.data.frame,
                               fortifiedCols))

    if (missing(names)){
      names = LETTERS[1:length(df)]
    }

    colnames(fortifiedDf) <- c("dates",names)
  }

  if (class(df) == "data.frame"){
    fortifiedDf = fortify.col(df, fInterval, by)
  }
  return(fortifiedDf)

}

#' Compress dataframe
#'
#' Bla bla
#'
#'@param df asdfasdfasd
#'@param order badsfasdf
#'
#'@author JV Yago
#'
#'@importFrom stats ts
#'@export
compress.col<-function(df, order){
  compress<-df[,2] %>% ts(frequency =1) %>% ma(order)
  compress<-data.frame(dates = df[,1], values = as.numeric(compress))
  compress<-compress[seq(1,nrow(compress),order),]
  return(compress)
}

#'Aggregate dataframe with a function.
#'
#'Performs an aggregation within the interval defined by \code{freq}, by executing
#'a provided function.
#'
#'For security reasons the dataframe must have a number of observations multiple
#'of \code{freq}, also all the time index must be complete i.e without silents losts, see
#' \code{\link[sensorsUtils]{fortify_df}} for more info.
#'Its desirable that the time interval present in the input dataframe include complete periods of time. For example
#'if the time index (first column), takes the range ['2019/01/01 00:00:00', ..., 2019/01/03 20:00:00] will be more
#'convenient to modify it to obtain ['2019/01/01 00:00:00', ..., 2019/01/03 23:00:00] for obtain the last day completely,
#'that effect can be acomplish with the function \code{\link[sensorsUtils]{fortify_df}}, using the parameter \code}{fInterval}.
#'
#'
#'@param df a dataframe, the first column must be the time index.
#'@param freq frequency of the observations in the dataframe, same as \code{\link[stats]{ts}} object.
#'@param fun a function like \code{mean()}, \code{max()}, etc.
#'
#'@importFrom stats ts
#'@importFrom stats aggregate
#'@export
#'@author JV Yago
#'
#'@examples
#' # Mean of 3 day data by days
#'a = as.POSIXct("2019/01/01", tz = "UTC")
#'b = as.POSIXct("2019/01/03 23:00:00", tz = "UTC")
#'df <- data.frame(dates = seq.POSIXt(a,b,"hour"), A = runif(24*3))
#'make.aggregation(df, freq = 24, mean)
#'
#' # The same with multicolumn datadrame
#'a = as.POSIXct("2019/01/01", tz = "UTC")
#'b = as.POSIXct("2019/01/03 23:00:00", tz = "UTC")
#'df<-data.frame(dates = seq.POSIXt(a,b,"hour"),
#'               A = runif(24*3),
#'               B = rep(1:3,each=24))
#'make.aggregation(df, freq = 24, mean)
#'
#' # The previus example but using a 12 hour mean
#'a = as.POSIXct("2019/01/01", tz = "UTC")
#'b = as.POSIXct("2019/01/03 23:00:00", tz = "UTC")
#'df<-data.frame(dates = seq.POSIXt(a,b,"hour"),
#'               A = runif(24*3),
#'               B = rep(1:3,each=24))
#'make.aggregation(df, freq = 12, mean)
make.aggregation<-function(df, freq, fun){
  ts <- ts(df, frequency = freq)
  ts <- aggregate.ts(ts, nfrequency = 1, FUN = fun)

  if (nrow(ts)==1){
    df <- as.data.frame(t(ts[,2:ncol(ts)]))
  }else{
    df <- as.data.frame(ts[,2:ncol(ts)])
  }
  return(df)
}



compress.df<-function(){

}
