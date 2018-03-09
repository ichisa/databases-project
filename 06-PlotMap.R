
library(rgdal)
library(raster)
library(sf)
library(ggplot2)
library(leaflet)
a
ddTiles = function (map, urlTemplate = "**http:**//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                     attribution = NULL, layerId = NULL, group = NULL, options = tileOptions())
{
  options$attribution = attribution
  if (missing(urlTemplate) && is.null(options$attribution)) 
    options$attribution = paste("&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap</a>", 
                                "contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA</a>")
  invokeMethod(map, getMapData(map), "addTiles", urlTemplate, 
               layerId, group, options)
}

#load all the shapes
formosa<-readOGR(dsn="C:/Users/Elisa/Documents/databases/proj/poligons", layer="formosa_poligono")
plots2000<-readOGR(dsn="C:/Users/Elisa/Documents/databases/proj/poligons", layer="lotes_formosa_00_suelos")
plots2005<-readOGR(dsn="C:/Users/Elisa/Documents/databases/proj/poligons", layer="lotes_formosa_05_suelos")
plots2010<-readOGR(dsn="C:/Users/Elisa/Documents/databases/proj/poligons", layer="lotes_formosa_10_suelos")
#change cordinate system
plots2000 <- spTransform(plots2000, CRS("+proj=longlat +datum=WGS84 +no_defs"))
plots2005 <- spTransform(plots2005, CRS("+proj=longlat +datum=WGS84 +no_defs"))
plots2010 <- spTransform(plots2010, CRS("+proj=longlat +datum=WGS84 +no_defs"))
#a
plots <- rbind( plots2000, plots2005, plots2010) #Problemita con el plot2000 no tiene ID


bins <- c(0, 5, 10, 15, 20, 30, 60, 100)
pal <- colorBin("Reds", domain = plots$IND_PROD, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g Productivity Index",
  plots$Fecha, plots$IND_PROD
) %>% lapply(htmltools::HTML)


leaflet(plots) %>%  
  addTiles() %>%
  addPolygons(fillColor = ~pal(IND_PROD), color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 0.8, fillOpacity = 0.5, dashArray = "",
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront =TRUE, 
                                                  dashArray = ""),
              label = labels)
