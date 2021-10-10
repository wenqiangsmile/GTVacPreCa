# pathway change
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
#GBD
setwd('D:/Postdoc/Postdoc/GBD/Request/LiverCancer')
eapc <- read_csv('Output/EAPC_ByRegion.csv')
eapc <- eapc %>% filter(location_name!='World Bank Income Levels')
eapc <- eapc %>% mutate(se=(eapc_uci-eapc)/1.96)
eapc <- eapc %>% mutate(category=str_detect(location_name, 'SDI'))
eapc <- eapc %>% mutate(category=ifelse(category==TRUE, 'SDI regions', ifelse(location_name=='Global', 'SDI regions', 'Geographical regions')))


#ByVac_group##################
vac_inc <- read.csv('Output/Country_EAPC_VacGroup.csv', stringsAsFactors = F)
vac_inc <- vac_inc %>% mutate(se=(eapc_uci-eapc)/1.96)
vac_inc$age_group <- factor(vac_inc$age_group, levels = c('All', '0-14', '15-29', '30-64', '65+'))
vac_inc <- vac_inc %>% mutate(category='Immunisation catetories')
colnames(vac_inc)[1] <- 'location_name'

##
eapc <- bind_rows(eapc, vac_inc)

eapc$location_name <- factor(eapc$location_name, levels = c('Global', 'Low SDI', 'Low-middle SDI', 'Middle SDI', 'High-middle SDI', 'High SDI', 'East Asia', 'Southeast Asia', 'High-income Asia Pacific', 'Central Asia', 'South Asia', 'Australasia', 'Oceania', 'North Africa and Middle East', 'Central Sub-Saharan Africa', 'Eastern Sub-Saharan Africa', 'Southern Sub-Saharan Africa', 'Western Sub-Saharan Africa',  'Eastern Europe', 'Central Europe', 'Western Europe', 'High-income North America', 'Caribbean',  'Andean Latin America', 'Central Latin America', 'Southern Latin America', 'Tropical Latin America', '0', '20-50', '51-70', '71-80', '81-90', '90+'), labels = c('Global', 'Low SDI', 'Low-middle SDI', 'Middle SDI', 'High-middle SDI', 'High SDI', 'East Asia', 'Southeast Asia', 'High-income Asia Pacific', 'Central Asia', 'South Asia', 'Australasia', 'Oceania', 'North Africa and Middle East', 'Central Sub-Saharan Africa', 'Eastern Sub-Saharan Africa', 'Southern Sub-Saharan Africa', 'Western Sub-Saharan Africa',  'Eastern Europe', 'Central Europe', 'Western Europe', 'High-income North America', 'Caribbean',  'Andean Latin America', 'Central Latin America', 'Southern Latin America', 'Tropical Latin America', 'No universal immunization', 'Vaccine coverage: 20-50%', 'Vaccine coverage: 51-70%', 'Vaccine coverage: 71-80%', 'Vaccine coverage: 81-90%', 'Vaccine coverage: >90%'))

eapc$age_group <- factor(eapc$age_group, levels = c('All', '0-14', '15-29', '30-64', '65+'), labels = c('Overall', '0-14', '15-29', '30-64', '65+'))

sdi <- eapc %>% filter(category=='SDI regions')
geo <- eapc %>% filter(category=='Geographical regions')
########barchart###########
library(wesanderson)
pal <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p1 <- ggplot(eapc %>% filter(category=='SDI regions'&age_group!='Overall'), aes(fill=age_group, y=eapc, x=location_name)) + geom_col(position = position_dodge(), colour = 'black', size = 0.4)  + scale_fill_manual(values=c("#009E73", "#56B4E9", "#0072B2", "#D55E00")) + theme_bw() +  scale_y_continuous(name = 'EAPC', limits = c(-5.1, 5),breaks = seq(-5, 5, 2.5), labels = seq(-5, 5, 2.5))  + scale_x_discrete(name = '')  + guides(fill=guide_legend("")) + theme(axis.text.x = element_text(angle = 45, hjust=0.95, vjust=0.95, colour = 'black')) + theme(axis.text.y = element_text(colour = 'black')) + geom_errorbar(aes(ymin=eapc_lci, ymax=eapc_uci), size=0.4, width=.5, position=position_dodge(0.9), colour = 'black') + theme(axis.text.y = element_text(colour = 'black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank()) + theme(plot.margin = margin(0.1, 0.1, -0.2, 0.05, "cm"))

p2 <- ggplot(eapc %>% filter(category=='Geographical regions'&age_group!='Overall'), aes(fill=age_group, y=eapc, x=location_name)) + geom_col(position = position_dodge(), colour = 'black', size = 0.4)  + scale_fill_manual(values=c("#009E73", "#56B4E9", "#0072B2", "#D55E00")) + theme_bw() +  scale_y_continuous(name = 'EAPC', limits = c(-5.1, 5), breaks = seq(-5, 5, 2.5), labels = seq(-5, 5, 2.5))  + scale_x_discrete(name = '')  + guides(fill=guide_legend("")) + theme(axis.text.x = element_text(angle = 45, hjust=0.95, vjust=0.95, colour = 'black')) + theme(axis.text.y = element_text(colour = 'black')) + geom_errorbar(aes(ymin=eapc_lci, ymax=eapc_uci), size=0.4, width=.5, position=position_dodge(0.9), colour = 'black')  + theme(axis.text.y = element_text(colour = 'black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank()) + theme(plot.margin = margin(0.1, 0.1, -0.5, 0.05, "cm"))

p3 <- ggplot(eapc %>% filter(category=='Immunisation catetories'&age_group!='Overall'), aes(fill=age_group, y=eapc, x=location_name)) + geom_col(position = position_dodge(), colour = 'black', size = 0.4)  + scale_fill_manual(values=c("#009E73", "#56B4E9", "#0072B2", "#D55E00")) + theme_bw() +  scale_y_continuous(name = 'EAPC', limits = c(-5.1, 5), breaks = seq(-5, 5, 2.5), labels = seq(-5, 5, 2.5))  + scale_x_discrete(name = '')  + guides(fill=guide_legend("")) + theme(axis.text.x = element_text(angle = 45, hjust=0.95, vjust=0.95, colour = 'black')) + theme(axis.text.y = element_text(colour = 'black')) + geom_errorbar(aes(ymin=eapc_lci, ymax=eapc_uci), size=0.4, width=.5, position=position_dodge(0.9), colour = 'black')  + theme(axis.text.y = element_text(colour = 'black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank()) + theme(plot.margin = margin(0.1, 0.1, -0.2, 0.05, "cm"))

library(ggpubr)
ggarrange(ggarrange(p1, p3, ncol = 2, labels = c("A", "B"), common.legend = TRUE, legend = 'top'),
          ggarrange(p2, ncol = 1,labels = 'C', legend = 'none'),
          nrow = 2, common.legend = T, legend = 'bottom'
) 
