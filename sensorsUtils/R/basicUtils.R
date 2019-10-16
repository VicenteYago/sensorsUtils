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
#'
#'
#' @examples
#' fortify.df(df,"30 min")
#' fortify.df(df, c(as.POSIXct("1/01/2019"), as.POSIXct("1/01/2020")), "hour")
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
  col.fortified <- merge(df, canonical.df, by = "dates", all.y = T)[,c(1:2)]
  return(col.fortified)
}

#' Create a fortified dataframe.
#'
#' @param dfList a list of dataframes with a dates column in \code{\link[base]{POSIXct}} format an another column with any value type.
#' @param fInterval a 2-item vector with 'start' and 'end' POSIXct dates respectively. All the values in the given interval will be explicitdly
#' included in the resulting dataframe with NAs. By default the limit dates are extracted from the data, in this case 'start' and end'
#' its the minimun and maximun dates for \strong{all} the dataframes given in \code{dfList}.
#' @param by a character string, containing the data increment in terms of "sec", "min", "hour", "day", "DSTday", "week", "month", "quarter" or "year".
#'  This can optionally be preceded by a (positive or negative) integer and a space, or followed by "s".
#'  See \code{\link[base]{seq.POSIXt}} for more info. The increment provided must equivalent to the real data-time increment.
#' @param names a vector of names for the dataframe \strong{values}.
#' @return A fortified dataframe, i.e with all the lost values explicited. The resulting time index its perfectly
#' defined by \code{seq(start,end,by)}, so the columns of the dataframe created with this function will have the same index,
#' i.e the silent lost values of the inputs dataframes (the columns in the new one) will be marked with NAs in the right positions.
#' @author JV Yago
#'
#'
#'
#' @examples
#' fortify.df(dfList=myDfList, by="30 min", names = c("A","B","Z"))
#' fortify.df(df,c(as.POSIXct("1/01/2019"), as.POSIXct("1/01/2020")),
#'            "hour", names = c("A","B","Z"))
fortify.df<-function(dfList, fInterval, by, names){
  if(missing(fInterval)){
    datesLists<-lapply(dfList, '[',1) %>%
    do.call(what=rbind.data.frame) -> alldates
    fInterval = c(min(alldates[,1]), max(alldates[,1]))
  }

  fortifiedCols<-lapply(dfList, function(x){
    return(fortify.col(x, fInterval, by))
  })

  fortifiedIndex <- fortifiedCols[[1]][,1]

  fortifiedCols<-lapply(fortifiedCols, function(x){
    return(x[,2])
  })

  fortifiedDf<-cbind(fortifiedIndex,
                     do.call(cbind.data.frame,
                             fortifiedCols))

  colnames(fortifiedDf) <- c("dates",names)
  return(fortifiedDf)

}

