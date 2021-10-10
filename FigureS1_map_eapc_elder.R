# # pathway change
# mypaths <- .libPaths()
# mypaths <- c(mypaths, 'C:/R_lib')
# mypaths <- c(mypaths[3], mypaths[2], mypaths[1])
# .libPaths(mypaths)

#
gc()
remove(list = ls())
library(dplyr)
library(haven)
library(foreign)
library(reshape2) 
library(zoo)
library(readr)
library(stringr)
library(ggplot2)
library(maps)

#EAPC
setwd('D:/Postdoc/Postdoc/GBD/Request/LiverCancer')
eapc <- read.csv('Output/eapc_iso.csv', stringsAsFactors = F)
colnames(eapc)[5] <- 'eapc_val'

library(RColorBrewer)
myPallette <-
  c(rev(brewer.pal(9, "YlOrRd"))
    , "white"
    , brewer.pal(9, "Blues"))



#############################################EAPC_Map######################################################
eapc_age <- eapc[which(eapc$age_group=='15-29'), ]
mapped_data <- joinCountryData2Map(eapc_age, joinCode = "ISO3", 
                                   nameJoinColumn = "iso_bind")

map <- fortify(mapped_data, region="iso_bind")
map <- left_join(map, eapc_age, by = c('id'='iso_bind'))
limit <- max(abs(map$eapc_val)) * c(-1, 1)

#############################################EAPC_Map######################################################
eapc_age <- eapc[which(eapc$age_group=='30-64'), ]
mapped_data <- joinCountryData2Map(eapc_age, joinCode = "ISO3", 
                                   nameJoinColumn = "iso_bind")

map <- fortify(mapped_data, region="iso_bind")
map <- left_join(map, eapc_age, by = c('id'='iso_bind'))

library(wesanderson)
pal <- wes_palette("Zissou1", 7, type = "continuous")
p1 <- ggplot() + geom_map(data=map, map=map,
                          aes(x=long, y=lat, map_id=id, group=location_name, fill=eapc_val), colour = 'black') + theme(legend.position="left") + theme_bw() +  theme(
                            panel.border = element_blank(), 
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), 
                            axis.line = element_line(colour = "black")
                          ) + 
  theme(line = element_blank(),
        axis.line=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) + scale_fill_distiller(palette = 'PiYG', direction = -1, limits = limit) + theme(legend.position = 'bottom') + labs(fill='EAPC') + theme(plot.title = element_text(hjust = 0.5)) + guides(fill = guide_colorbar(reverse = F, title.position = 'bottom', title.hjust = 0.5, title.vjust = 0.5, direction = 'horizontal', barwidth = 8, barheight = 1.5))
#############################################EAPC_Map######################################################
eapc_age <- eapc[which(eapc$age_group=='65+'), ]
mapped_data <- joinCountryData2Map(eapc_age, joinCode = "ISO3", 
                                   nameJoinColumn = "iso_bind")

map <- fortify(mapped_data, region="iso_bind")
map <- left_join(map, eapc_age, by = c('id'='iso_bind'))

library(wesanderson)
pal <- wes_palette("Zissou1", 9, type = "continuous")
p2 <- ggplot() + geom_map(data=map, map=map,
                          aes(x=long, y=lat, map_id=id, group=location_name, fill=eapc_val), colour = 'black') + theme(legend.position="left") + theme_bw() +  theme(
                            panel.border = element_blank(), 
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), 
                            axis.line = element_line(colour = "black")
                          ) + 
  theme(line = element_blank(),
        axis.line=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) + scale_fill_distiller(palette = 'PiYG', direction = -1, limits = limit)  + theme(legend.position = 'bottom') + labs(fill='EAPC') + theme(plot.title = element_text(hjust = 0.5)) + guides(fill = guide_colorbar(reverse = F, title.position = 'bottom', title.hjust = 0.5, title.vjust = 0.5, direction = 'horizontal', barwidth = 8, barheight = 1.5))


##################################################################
#################Incidence#############3
setwd('D:/Postdoc/Postdoc/GBD/Request/LiverCancer')
china_sum <- read.csv('Output/Country_incidence_byage.csv', stringsAsFactors = F)
incidence_age <- china_sum %>% select(1, 5:9) %>% filter(age_group=='30-64')
incidence_age <- as.data.frame(incidence_age)
library(rworldmap)
mapped_data <- joinCountryData2Map(incidence_age, joinCode = "ISO3", 
                                   nameJoinColumn = "iso_bind")
map <- fortify(mapped_data, region="iso_bind")
map <- left_join(map, incidence_age, by = c('id'='iso_bind'))

map$incidence_change_cat <- factor(map$incidence_change_cat, levels = c('50-85% decrease', '25-50% decrease', '10-25% decrease', '<10% decrease', '<50% increase', '50-100% increase', '100-200% increase', '>200% increase'))
#
library(wesanderson)
pal <- wes_palette("Zissou1", 8, type = "continuous")


p3 <- ggplot() + geom_map(data=map, map=map,
                          aes(x=long, y=lat, map_id=id, group=location_name, fill=incidence_change_cat), colour = 'black') + theme_bw() +  theme(
                            panel.border = element_blank(), 
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), 
                            axis.line = element_line(colour = "black")) + 
  theme(line = element_blank(),        
        axis.line=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  scale_fill_brewer(palette = 'RdYlGn', direction = -1) + theme(legend.position = 'bottom') + labs(fill='') + theme(plot.title = element_text(hjust = 0.5))  + guides(fill = guide_legend(override.aes = list(size = 0.25)))

#################Incidence#############3
setwd('D:/Postdoc/Postdoc/GBD/Request/LiverCancer')
china_sum <- read.csv('Output/Country_incidence_byage.csv', stringsAsFactors = F)
incidence_age <- china_sum %>% select(1, 5:9) %>% filter(age_group=='65+')
incidence_age <- as.data.frame(incidence_age)
library(rworldmap)
mapped_data <- joinCountryData2Map(incidence_age, joinCode = "ISO3", 
                                   nameJoinColumn = "iso_bind")
map <- fortify(mapped_data, region="iso_bind")
map <- left_join(map, incidence_age, by = c('id'='iso_bind'))

map$incidence_change_cat <- factor(map$incidence_change_cat, levels = c('50-85% decrease', '25-50% decrease', '10-25% decrease', '<10% decrease', '<50% increase', '50-100% increase', '100-200% increase', '>200% increase'))
#
library(wesanderson)
pal <- wes_palette("Zissou1", 8, type = "continuous")


p4 <- ggplot() + geom_map(data=map, map=map,
                          aes(x=long, y=lat, map_id=id, group=location_name, fill=incidence_change_cat), colour = 'black') + theme_bw() +  theme(
                            panel.border = element_blank(), 
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), 
                            axis.line = element_line(colour = "black")) + 
  theme(line = element_blank(),        
        axis.line=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  scale_fill_brewer(palette = 'RdYlGn', direction = -1) + labs(fill='') + theme(plot.title = element_text(hjust = 0.5))  + guides(fill = guide_legend(override.aes = list(size = 0.25)))
###########
library(ggpubr)
ggarrange(ggarrange(p3, p4, ncol = 2, labels = c("A", 'B'), common.legend = TRUE, legend = 'bottom'),
          ggarrange(p1, p2, ncol = 2, labels = c("C", 'D'), common.legend = TRUE, legend = 'bottom'), nrow = 2) 


