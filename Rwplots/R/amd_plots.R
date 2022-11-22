#' Daily, monthly and annual time series plots
#'
#' The function "amd_plots" can be used to plot time series plots for rainfall data.
#' Daily/monthly/annual/seasonal rainfall values of the time series given as input. Depending on the value of daily, monthly, annual and/or seasonal time series plots, boxplots and histograms are produced.
#' @param data A file path of the csv file as a string within double quotes. For example use Prec_data.csv demo file using system.file()
#' @param date Column name which contain date and year as a string within double quotes. Date has to be in year-month-day format.
#' @param rainfall column name which contain rainfall/precipitation values as string within double quotes.
#' @import hydroTSM zoo
#' @return 9 plots
#' @export
#'
#' @examples
#' file= system.file("extdata","Prec_data.csv", package = "Rwplots")
#' amd_plots(file,"time","Rainfall")

amd_plots<- function(data,date,rainfall){
  gpr<- read.csv(data)
  period <- gpr[[date]]
  prec <- gpr[[rainfall]]
  Time.gpr<- strptime(period, format ="%Y-%m-%d")
  dates.gpr<-format(Time.gpr,"%Y-%m-%d")
  gpr.daily<-aggregate(prec, by=list(dates.gpr),FUN=sum)
  names(gpr.daily)<-c("dates","pr")
  gpr.daily$dates=as.Date(gpr.daily$dates,"%Y-%m-%d")
  gpr.daily.ts= zoo(gpr.daily$pr,order.by = gpr.daily$dates)
  hydroplot(gpr.daily.ts,var.type = "Precipitation",var.unit ="mm", xlab="Time", ylab="precipitation(mm)")
}
