#' Daily to monthly rainfall conversion
#'
#' The function "d2m" can be used to Daily rainfall values to monthly values.
#' @param data A file path of the csv file as a string within double quotes. For example use Prec_data.csv demo file using system.file()
#' @param date Column name which contain date and year as a string within double quotes. Date has to be in year-month-day format.
#' @param rainfall column name which contain rainfall/precipitation values as string within double quotes.
#' @import lubridate
#' @return a data frame
#' @export
#'
#' @examples
#' file= system.file("extdata","Prec_data.csv", package = "Rwplots")
#' d2m(file,"time","Rainfall")
d2m<- function(data,date,rainfall){
  gpr<- read.csv(data)
  df <- data.frame(date = ymd(gpr[[date]]), rainfall = gpr[[rainfall]])
  Time.gpr<- strptime(df[[1]], format ="%Y-%m-%d")
  dates.gpr<-format(Time.gpr,"%Y-%m-%d")
  gpr<- arrange(df,dates.gpr)
  gpr$mdate<-as.yearmon(dates.gpr)
  res<- gpr %>% group_by(mdate) %>% summarise_all(mean)
  return (res)
}
