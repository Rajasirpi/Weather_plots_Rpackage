#' Annual precipitation plot
#'
#' The function "Precip_plot" can be used to plot annual precipitation plot with nc file. The resulting plot will looks like large when you see it in the plots tab so click zoom to see it in extract size.
#' @param data A file path of the csv file as a string within double quotes. For example use Rainfall.nc demo file using system.file()
#' @param rainfall Column name which contain Rainfall values as a string within double quotes
#' @param lon Column name which contain longitude values as a string within double quotes
#' @param lat Column name which contain latitude values as a string within double quotes
#' @import ncdf4 grDevices
#' @return A precipitation plot
#' @export
#'
#' @examples
#' file= system.file("extdata","Rainfall.nc", package = "Rwplots")
#' precip_plot(file,"RAINFALL","LONGITUDE", "LATITUDE")

precip_plot<- function(data,rainfall,lon,lat){
  fname = data
  ncin = nc_open(fname)
  prec = ncvar_get(ncin,"RAINFALL")
  Lon = ncvar_get(ncin,"LONGITUDE")
  Lat = ncvar_get(ncin,"LATITUDE")
  nc_close(ncin)

  dim(prec)

  prec.total = apply(prec, 2, rowSums)

  dim(prec.total)

  S = as.matrix(expand.grid(Lon, Lat))

  Y = c(prec.total) / 10

  keep = which(!is.na(Y))
  S = S[keep, ]
  Y = Y[keep]

  data = data.frame(lon = S[ , 1], lat = S[ , 2], value = Y)
  colnames(data) = c("Longitude", "Latitude", "Rainfall")
  #head(data)

  indbox = make_bbox(lon = c(min(S[ , 1]), max(S[ , 1])), lat = c(max(S[ , 2]), min(S[ , 2])), f = .1)

  ind = get_map(location = indbox, zoom = 5, maptype = "terrain")

  p = ggmap(ind)

  data = data.frame(lon = S[ , 1], lat = S[ , 2], value = Y)

  p0 = ggmap(ind) + geom_tile(data, mapping = aes(x = lon, y = lat, fill = value), width = 0.27, height = 0.27) + xlab("Longitude") + ylab("Latitude") + ggtitle("Annual precipitation (2021)") +
    theme(plot.title = element_text(hjust = 0.5, size=25),
          axis.text=element_text(size=20), axis.title=element_text(size=20))

  p1 = p0 + scale_fill_gradientn(colours = topo.colors(30), name = "Rainfall (cm)") + theme(legend.key.size = unit(0.3, "in"), legend.text = element_text(size=15), legend.title = element_text(size=15), legend.position = c(0.8, 0.2))
  p1
  #ggsave(p1, filename = file, height = 8, width = 8)
  return(p1)
}
