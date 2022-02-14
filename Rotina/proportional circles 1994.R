library(ggmap)
setwd("/home/bmiyamoto/Documentos/Mapa - Circulos proporcionais/ggmap/")

vp=read.csv("/home/bmiyamoto/Documentos/Mapa - Circulos proporcionais/Google/d",header=T)
colnames(vp)[14]="lat"
colnames(vp)[15]="lon"
colnames(vp)[11]="Vperm1994"

#get_map(location = c(lon = -95.3632715, lat = 29.7632836), zoom = "auto",
 #  scale = "auto", maptype = c("terrain", "terrain-background", "satellite",
  #                                  "roadmap", "hybrid", "toner", "watercolor", "terrain-labels", "terrain-lines",
   #                                 "toner-2010", "toner-2011", "toner-background", "toner-hybrid",
    #                                "toner-labels", "toner-lines", "toner-lite"), source = c("google", "osm",
                  #                                                                           "stamen", "cloudmade"), force = ifelse(source == "google", TRUE, TRUE),
     #   messaging = FALSE, urlonly = FALSE, filename = "ggmapTemp",
      #  crop = TRUE, color = c("color", "bw"), language = "en-EN", api_key)


mapsp=get_map(location = c(lat=-22,lon=-48.5),zoom=6,scale=1)
ggmap(mapsp)


mapPoints <- ggmap(mapsp) +  geom_point(aes(x = lat, y = lon, size = Vperm1994), data = vp, alpha = 0.5,color="green4")

mapPoints 


mapPointsLegend <- mapPoints  + scale_size(breaks = c(0, 5000, 10000, 25000, 50000, 75000), labels = c(0, 5000, 10000, 25000, 50000, 75000), name = "Value 1994")


mapPointsLegend
