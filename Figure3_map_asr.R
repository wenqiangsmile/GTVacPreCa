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
eapc <- read.csv('Output/asr_hbv_corr.csv', stringsAsFactors = F)

#############################################EAPC_Map######################################################
library(rworldmap)
mapped_data <- joinCountryData2Map(eapc, joinCode = "ISO3", 
                                   nameJoinColumn = "iso_bind")

##
map <- fortify(mapped_data, region="iso_bind")
map <- left_join(map, eapc, by = c('id'='iso_bind'))

library(wesanderson)
pal <- wes_palette("Zissou1", 100, type = "continuous")
eapc <- eapc %>% mutate(asr_2017_cat=ifelse(asr_2017<2, '0-1', ifelse(asr_2017>=2&asr_2017<4, '2-3',ifelse(asr_2017>=4&asr_2017<7, '4-6', ifelse(asr_2017>=7&asr_2017<10, '7-9', ifelse(asr_2017>=10&asr_2017<20, '10-19', '20-25'))))))

map <- map %>% mutate(asr_2017_cat=ifelse(asr_2017<2, '0-1', ifelse(asr_2017>=2&asr_2017<4, '2-3',ifelse(asr_2017>=4&asr_2017<10, '4-9', ifelse(asr_2017>=10&asr_2017<20, '10-19', '20-25')))))
map$asr_2017_cat <- factor(map$asr_2017_cat, levels = c('0-1', '2-3', '4-9', '10-19', '20-25'))

p1 <- ggplot() + geom_map(data=map, map=map,
                          aes(x=long, y=lat, map_id=id, group=location_name, fill=asr_2017_cat), colour = 'black') + theme_bw() +  theme(
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
        axis.title.y = element_blank()) + 
  scale_fill_brewer(palette = "Reds")  + ggtitle("ASR in 2017 (/10^5)") + theme(legend.position = 'bottom') + labs(fill='') + theme(plot.title = element_text(hjust = 0.5))

p2<- ggplot() + geom_map(data=map, map=map,
                    aes(x=long, y=lat, map_id=id, group=location_name, fill=eapc), colour = 'black') + theme_bw() +  theme(
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
        axis.title.y = element_blank()) + 
  scale_fill_distiller(palette = 'PiYG', direction = -1)  + ggtitle("EAPC from 1990 to 2017") + theme(legend.position = 'bottom') + labs(fill='EAPC') + theme(plot.title = element_text(hjust = 0.5)) + guides(fill = guide_colorbar(reverse = F, title.position = 'bottom', title.hjust = 0.5, title.vjust = 0.5, direction = 'horizontal', barwidth = 8, barheight = 1.5))

p3<- ggplot() + geom_map(data=map, map=map,
                    aes(x=long, y=lat, map_id=id, group=location_name, fill=hbv_cor), colour = 'black') + theme_bw() +  theme(
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
        axis.title.y = element_blank()) + 
  scale_fill_distiller(palette = 'Spectral', direction = -1) + labs(fill = "Correlation coefficient") + ggtitle("Correlation of EAPC with vaccine coverage") + theme(legend.position = 'bottom') + theme(plot.title = element_text(hjust = 0.5)) + guides(fill = guide_colorbar(reverse = F, title.position = 'bottom', title.hjust = 0.5, title.vjust = 0.5, direction = 'horizontal', barwidth = 8, barheight = 1.5))

#################Incidence#############3
setwd('D:/Postdoc/Postdoc/GBD/Request/LiverCancer')
china_sum <- read.csv('Output/Country_incidence_byage.csv', stringsAsFactors = F)
incidence_age <- china_sum %>% select(1, 5:9) %>% filter(age_group=='All')
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
  scale_fill_brewer(palette = 'RdYlGn', direction = -1) + ggtitle("Change of cancer cases from 1990 to 2017 (%)") + theme(legend.position = 'bottom') + labs(fill='') + theme(plot.title = element_text(hjust = 0.5))  + guides(fill = guide_legend(override.aes = list(size = 0.25)))

###########
library(ggpubr)
ggarrange(p1, p4, p2, 
          labels = c("A", "B", 'C'),
          ncol = 1, nrow = 3)

###Nice_palette
browseURL('http://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html')
