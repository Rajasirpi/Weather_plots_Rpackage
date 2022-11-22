#' Daily to annual rainfall conversion
#'
#' The function "d2a" can be used to Daily rainfall values to annual values.
#' @param data A file path of the csv file as a string within double quotes. For example use Prec_data.csv demo file using system.file()
#' @param date Column name which contain date and year as a string within double quotes. Date has to be in year-month-day format.
#' @param rainfall column name which contain rainfall/precipitation values as string within double quotes.
#' @import lubridate
#' @return a data frame
#' @export
#'
#' @examples
#' file= system.file("extdata","Prec_data.csv", package = "Rwplots")
#' d2a(file,"time","Rainfall")
d2a<- function(data,date,rainfall){
  gpr<- read.csv(data)
  df = data.frame(date = ymd(gpr[[date]]), rainfall = gpr[[rainfall]])
  res<- daily2annual(df, FUN=mean, na.rm = TRUE, out.fmt = "%Y") %>% as.data.frame
  return(res)
}
