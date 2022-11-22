#' Matrix plot
#'
#' This function "matrix_plot" is used to convert values to matrix and plot the precipitation values.
#' @param data A file path of the csv file as a string within double quotes. For example use Prec_data.csv demo file using system.file()
#' @param date Column name which contain date and year as a string within double quotes. Date has to be in year-month-day format.
#' @param rainfall column name which contain rainfall/precipitation values as string within double quotes.
#' @import lubridate
#' @return a plot
#' @export
#'
#' @examples
#' file= system.file("extdata","Prec_data.csv", package = "Rwplots")
#' matrix_plot(file,"time","Rainfall")
matrix_plot<- function(data,date,rainfall){
  gpr<- read.csv(data)
  df = data.frame(date = ymd(gpr[[date]]), rainfall = gpr[[rainfall]])
  z <- zoo(df[, -1], df[, 1])
  m <- daily2monthly(z, FUN=mean, na.rm=TRUE)
  M <- matrix(m, ncol=12, byrow=TRUE)
  colnames(M) <- month.abb
  rownames(M) <- unique(format(time(m), "%Y"))
  res= print(matrixplot(M, ColorRamp="Precipitation",
                        main="Monthly precipitation"))
  return(res)
}
