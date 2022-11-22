#' Time series plot of cyclone tracks
#'
#' The function "ts_tracks" can be used to plot  time series of cyclone tracks for the total number of years according to the given data.
#' @param data A file path of the csv file as a string. For example use Tracks.csv demo file using system.file().
#' @param Lon Column name which contain longitude values as a string within double quotes
#' @param Lat Column name which contain latitude values as a string within double quotes
#' @param Name Column name which contain cyclone name values as a string within double quotes
#' @param Year Column name which contain only year as a string within double quotes
#' @return a plot
#' @export
#' @import mapview sf tidyverse ggmap dplyr  purrr ggplot2 hydroTSM tidyr
#' @examples
#' file= system.file("extdata","Tracks.csv", package = "Rwplots")
#' ts_tracks(file,"lon","lat","name","year")

ts_tracks<-function(data,Lon,Lat,Name,Year) {
  cyclone<- read.csv(data,skip = 0, stringsAsFactors = TRUE)
  cyclone<- cyclone %>% filter(!cyclone[[Name]] == "NOT_NAMED")
  substorms.sf <- cyclone %>% st_as_sf(coords = c(Lon, Lat), crs = 4326)
  class(substorms.sf)
  (substorms.nest <- substorms.sf %>% group_by(name,year) %>% nest)
  to_line <- function(tr) st_cast(st_combine(tr), "LINESTRING") %>% .[[1]]
  tracks <- substorms.nest %>% pull(data) %>% map(to_line) %>% st_sfc(crs = 4326)
  cyclone_Tracks <- substorms.nest %>% select(-data) %>% st_sf(geometry = tracks)
  cyclone_Tracks %>% ggplot(aes(color = noquote(Name))) + geom_sf()
  res= mapview(cyclone_Tracks, zcol = Year, legend = TRUE)
  return(res)
}
