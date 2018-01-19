##################################################
# RM01 for Mphil in Environmental Policy
# Is there a Housing Wealth Effect on Energy Consumption?
# @ University of Cambridge
# Lucas Paoli, lp485
##################################################

###########
# Libraries
library(tidyverse)
library(rgdal)
library(geojsonio)
library(sp)
library(maps)
library(ggmap)
library(maptools)
library(rgeos)
###########

############
# Variables
dic_regions = c('North East', 'North West', 'Yorkshire and the Humber', 'East Midlands', 'West Midlands',
                'East of England', 'London', 'South East', 'South West')
names(dic_regions)=c('0','1','2','3','4','5','6','7','8')
color.palette = c("#6a98d3","#bdb13d","#bd59c0","#62ba53","#736bc7","#59853e","#c4608c","#4eb699","#cd464a","#9b8341","#ce7640")
regions <- geojson_read('england_regions.geojson', what = "sp")
scotland <- geojson_read('scotland.geojson', what = "sp")
wales <- geojson_read('wales.geojson', what = "sp")
###########

regions_df <- fortify(regions)
regions_df$Fill = dic_regions[gsub('\\..*','',regions_df$group)]
scotland_df <- fortify(scotland)
scotland_df$Fill = 'Scotland'
wales_df <- fortify(wales)
wales_df$Fill = 'Wales'

plot.positions = data.frame(regions = c(dic_regions,'Wales','Scotland'))
row.names(plot.positions) = plot.positions$regions
plot.positions$lat = NA
plot.positions$long = NA
plot.positions$value = NA
reg_centroids = coordinates(gCentroid(regions,byid=T))
row.names(reg_centroids) = dic_regions
for (i in c(dic_regions)){
  plot.positions[i,'lat']=reg_centroids[i,'y']
  plot.positions[i,'long']=reg_centroids[i,'x']
  plot.positions[i,'value']=sum(!is.na(subset(data.correct, REGION == i)$HSVAL))
}
wales_centroid = coordinates(gCentroid(wales,byid=T))
plot.positions['Wales','lat']=wales_centroid[1,'y']
plot.positions['Wales','long']=wales_centroid[1,'x']
plot.positions['Wales','value']=sum(!is.na(subset(data.correct, REGION == 'Wales')$HSVAL))
scot_centroid = coordinates(gCentroid(scotland,byid=T))
plot.positions['Scotland','lat']=scot_centroid[1,'y']
plot.positions['Scotland','long']=scot_centroid[1,'x']
plot.positions['Scotland','value']=sum(!is.na(subset(data.correct, REGION == 'Scotland')$HSVAL))

#map <- get_map(location = c(lon = -2.5, lat =  54.2), zoom = 6, maptype = "satellite", source = 'google')
#p_map = ggmap(map)+
p_map = ggplot() +
  theme_grey()+
  geom_polygon(data = regions_df,
               aes(x = long, y = lat, group = group, fill = Fill),
               color = "grey20", size = 0.2) + 
  geom_polygon(data = scotland_df,
               aes(x = long, y = lat, group = group, fill = Fill),
               color = "grey20", size = 0.2) + 
  geom_polygon(data = wales_df,
               aes(x = long, y = lat, group = group, fill = Fill),
               color = "grey20", size = 0.2) +
  scale_fill_manual(values=color.palette, name = 'Regions') +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

theta <- seq(pi/8, 2*pi, length.out=16)
xo <- diff(range(plot.positions$long))/200
yo <- diff(range(plot.positions$lat))/200
for (i in theta){
  p_map <- p_map + geom_text(data = plot.positions,
                             aes_q(x = bquote(long+.(cos(i)*xo)),
                                   y = bquote(lat+.(sin(i)*yo)),
                                   label=~value), 
                             size=3, colour='black' )
}
p_map <- p_map + geom_text(data = plot.positions,
                           aes(x=long,
                               y=lat,
                               label=value),
                           size=3, colour='white' )

ggsave(filename='UK_regions.pdf', p_map)


