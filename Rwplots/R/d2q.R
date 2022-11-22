#' Daily to quarterly rainfall conversion
#'
#' The function "d2q" can be used to Daily rainfall values to quarterly values i.e total of four month values .
#' @param data A file path of the csv file as a string within double quotes. For example use Prec_data.csv demo file using system.file()
#' @param date Column name which contain date and year as a string within double quotes. Date has to be in year-month-day format.
#' @param rainfall column name which contain rainfall/precipitation values as string within double quotes.
#' @import lubridate
#' @return a data frame
#' @export
#'
#' @examples
#' file= system.file("extdata","Prec_data.csv", package = "Rwplots")
#' d2q(file,"time","Rainfall")
d2q<- function(data,date,rainfall){
  gpr<- read.csv(data)
  df <- data.frame(date = ymd(gpr[[date]]), rainfall = gpr[[rainfall]])
  Time.gpr<- strptime(df[[1]], format ="%Y-%m-%d")
  dates.gpr<-format(Time.gpr,"%Y-%m-%d")
  gpr<- arrange(df,dates.gpr)
  gpr$qdate<-as.yearqtr(dates.gpr)
  res<- gpr %>% group_by(qdate) %>% summarise_all(mean)
  return (res)
}
