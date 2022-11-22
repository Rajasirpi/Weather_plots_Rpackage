#' Yearwise cyclones tracks plotting
#'
#' The function "trackbyyear" can be used to plot cyclone tracks for particular year and also uses different kind of plotting methods rather the one which is used in ts_tracks function. You can also use an external data in this function which should have all the required columns(refer arguments).
#' If you face some error in filter then try rename the columns name exactly like given in the example.
#' @param data A file path of the csv file as a string within double quotes. For example use Tracks.csv demo file using system.file()
#' @param lat Column name which contain latitude values as a string within double quotes
#' @param lon Column name which contain longitude values as a string within double quotes
#' @param wind Column name which contain wind values as a string within double quotes
#' @param name Column name which contain cyclone name values as a string within double quotes
#' @param year Column name which contain only year as a string within double quotes
#' @param val specific year which you want plot as numeric value
#' @param reg The region name around which your data has to be plotted. For examples "India".
#' @import PBSmapping maps
#' @return A plot of particular year
#' @export
#'
#' @examples
#' file= system.file("extdata","Tracks.csv", package = "Rwplots")
#' trackbyyear(file,"lat","lon", "kts", "name","year",2018,"India")

trackbyyear<- function (data,lat,lon, wind, name, year, val, reg){
  # data source
  basin <- read.csv(data, skip = 0, stringsAsFactors = TRUE)
  # cleaner columns
  year <- as.numeric(basin[[year]])
  lat <- as.numeric(gsub("^ ", "", basin[[lat]]))
  lon <- as.numeric(gsub("^ ", "", basin[[lon]]))
  kts <- as.numeric(gsub("^ ", "", basin[[wind]]))
  name <- as.factor(basin[[name]])
  #ISO_TIME <- as.Date(basin$time)
  if(missing(val)) {
    from = min(year)
    to = max(year)
    val = sprintf("%s to %s",from,to)
    sub<- basin %>% filter(year %in% from:to )
  } else{
    sub<- basin %>%  filter( year == val )
  }

  substorms <- sub %>% filter(!name == "NOT_NAMED") %>%
    filter(!lat == -999) %>%
    filter(!lon == -999)
  substorms$ID <- as.factor(paste(substorms$name, sep <- "."))

  ggplot() +
    geom_polygon(data = map_data("world", region = "India"),
                 aes(x = long, y = lat, group = group),
                 fill="lightgray", colour = "white")

  substorms %>%
    group_by(ID) %>%
    summarise(average_winds = mean(kts)) %>%
    arrange(desc(average_winds))

  ggplot(substorms, aes(x = lon, y = lat, group = ID)) +
    geom_polygon(data = map_data("world", region = "India"), aes(x = long, y = lat, group = group), fill="lightgray", colour = "white") +
    geom_path(data = substorms, aes(group = ID))

  if(missing(reg)) {
    wm <- map_data("world")
  } else{
    wm <- map_data("world", region = reg)
  }

  data.table::setnames(wm, c("X","Y","PID","POS","region","subregion"))

  maptitle = sprintf("cyclone track %s", val )

  res= ggplot(substorms, aes(x = lon, y = lat, group = ID)) + geom_tile( mapping = aes(x = lon, y = lat), width = 0.27, height = 0.27)  + ggtitle(maptitle) +
    geom_polygon(data = wm, aes(x = X, y = Y, group = PID),
                 fill="White", colour = "gray") + coord_fixed(1.3)+
    geom_path(data = substorms, aes(group = ID),
              alpha = substorms$kts/50,
              color = "blue") + labs(x = "", y = "", colour = "Wind \n(knots)") +theme(plot.title = element_text(hjust = 0.5, size=15),
                                                                                       axis.text=element_text(size=10), axis.title=element_text(size=10))
  output= res + labs(x = "Longitude", y= "Latitude")
  #ggsave(output, filename = file)
  return(output)
}
