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
china_pop <- read_csv('LiverCancer_Incidence/LiverCancer_Incidence_RegionLevel.csv')
china_pop <- china_pop %>% filter(cause_name=='Liver cancer due to hepatitis B')

#
age_who <- read.csv('who_age.csv')

china_age <- left_join(china_pop, age_who[, c(1, 3:4)], by = c('age_id'='age_id'))
china_age <- china_age %>% filter(!is.na(weight))
china_age <- china_age %>% mutate(age_weight=val*weight)
china_sum <- china_age %>% group_by(location_name, year, measure_name, sex_name, age_group) %>% summarise(age_sum=sum(val*weight)/sum(weight), age_sum_l=sum(lower*weight)/sum(weight), age_sum_u=sum(upper*weight)/sum(weight))

china_sum$measure_name <- factor(china_sum$measure_name, levels = c('Incidence', 'Deaths'))
china_sum$sex_name <- factor(china_sum$sex_name, levels = c('Both', 'Male', 'Female'))
china_sum$age_group <- factor(china_sum$age_group, levels = c(seq(1, 4, 1)), labels = c('0-14', '15-29', '30-64', '65+'))

#cancer
china_sum <- china_sum %>% filter(location_name=='Global'|location_name=='Low SDI'|location_name=='Low-middle SDI'|location_name=='Middle SDI'|location_name=='High-middle SDI'|location_name=='High SDI')
china_sum$location_name <- factor(china_sum$location_name, levels = c('Global', 'Low SDI', 'Low-middle SDI', 'Middle SDI', 'High-middle SDI', 'High SDI'))


########barchart###########
data1 <- china_sum %>% filter(sex_name=='Both'&measure_name=='Incidence')

library(scales)
p1 <- ggplot(data=china_sum %>% filter(sex_name=='Both'&measure_name=='Incidence'), aes(x=year, y=age_sum, colour=age_group)) + geom_point(size= 0.5) + geom_line() + facet_wrap(.~location_name, nrow = 1) + theme_bw() + ggtitle("") +  scale_x_continuous(name = '', breaks = c(seq(1990, 2010, 5), 2017), labels = c(seq(1990, 2010, 5), 2017)) + guides(colour=guide_legend(""))  + scale_color_manual(values=c("#009E73", "#56B4E9", "#0072B2", "#D55E00")) + scale_y_log10(name = 'ASR (/10^5)', limits = c(1e-2,260), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + theme(axis.text.x = element_text(angle = 45, hjust=0.95, vjust=0.95, colour = 'black')) + theme(axis.text.y = element_text(colour = 'black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank()) + theme(plot.margin = margin(0, 0.15, -0.5, 0.15, "cm"))


#GBD
setwd('D:/Postdoc/Postdoc/GBD/Request/LiverCancer')
china_pop <- read_csv('LiverCancer_Incidence/LiverCancer_Number_RegionLevel.csv')
china_pop <- china_pop %>% filter(cause_name=='Liver cancer due to hepatitis B')


############################AgeGroup########################################
age_who <- read.csv('who_age.csv')
china_age <- left_join(china_pop, age_who[, c(1, 3:4)], by = c('age_id'='age_id'))
china_age <- china_age %>% filter(!is.na(weight))
china_sum <- china_age %>% group_by(location_name,year, measure_name, sex_name, age_group) %>% summarise(age_sum=sum(val)/1000)

china_sum$measure_name <- factor(china_sum$measure_name, levels = c('Incidence', 'Deaths'))
china_sum$sex_name <- factor(china_sum$sex_name, levels = c('Both', 'Male', 'Female'))
china_sum$age_group <- factor(china_sum$age_group, levels = c(seq(1, 4, 1)), labels = c('0-14', '15-29', '30-64', '65+'))

#point
china_sum <- china_sum %>% filter(location_name=='Global'|location_name=='Low SDI'|location_name=='Low-middle SDI'|location_name=='Middle SDI'|location_name=='High-middle SDI'|location_name=='High SDI')
china_sum$location_name <- factor(china_sum$location_name, levels = c('Global', 'Low SDI', 'Low-middle SDI', 'Middle SDI', 'High-middle SDI', 'High SDI'))


#stack_bar
data1 <- china_sum %>% filter(sex_name=='Both'&measure_name=='Incidence')

p2 <- ggplot(china_sum %>% filter(sex_name=='Both'&measure_name=='Incidence'), aes(colour=age_group, y=age_sum, x=year)) + geom_point(size=0.5) + geom_line() + facet_wrap(.~location_name, nrow = 1) +  scale_x_continuous(name = '', breaks = c(seq(1990, 2010, 5), 2017), labels = c(seq(1990, 2010, 5), 2017)) + scale_colour_manual(values=c("#009E73", "#56B4E9", "#0072B2", "#D55E00")) + theme_bw() +  guides(colour=guide_legend(""))  + scale_y_log10(name = 'Cases (x10^3)',  limits = c(1e-2,260), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + theme(axis.text.x = element_text(angle = 45, hjust=0.95, vjust=0.95, colour = 'black')) + theme(axis.text.y = element_text(colour = 'black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank()) + theme(plot.margin = margin(0, 0.15, -0.5, 0.15, "cm"))


#####################################VAC##############################
#GBD
setwd('D:/Postdoc/Postdoc/GBD/Request/LiverCancer')
vac_inc <- read.csv('Output/Country_Incidence_VacGroup.csv', stringsAsFactors = F)

vac_inc$age_group <- factor(vac_inc$age_group, levels = c('All', '0-14', '15-29', '30-64', '65+'), labels = c('Overall', '0-14', '15-29', '30-64', '65+'))
vac_inc$vac_cat <- factor(vac_inc$vac_cat, levels = c('0', '20-50', '51-70', '71-80', '81-90', '90+'), labels = c('No universal immunization', 'Vaccine coverage: 20-50%', 'Vaccine coverage: 51-70%', 'Vaccine coverage: 71-80%', 'Vaccine coverage: 81-90%', 'Vaccine coverage: >90%'))
########barchart###########
library(scales)
data1 <- vac_inc %>% filter(sex_name=='Both'&measure_name=='Incidence')

p3 <- ggplot(data=vac_inc %>% filter(sex_name=='Both'&measure_name=='Incidence'&age_group!='Overall'), aes(x=year, y=age_sum, colour=age_group)) + geom_point(size= 0.5) + geom_line() + facet_wrap(.~vac_cat, nrow = 1) + theme_bw() + ggtitle("") +  scale_x_continuous(name = '', breaks = c(seq(1990, 2010, 5), 2017), labels = c(seq(1990, 2010, 5), 2017)) + guides(colour=guide_legend(""))  + scale_color_manual(values=c("#009E73", "#56B4E9", "#0072B2", "#D55E00")) + scale_y_log10(name = 'Cases (x10^3)', limits = c(1e-2,260), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + theme(axis.text.x = element_text(angle = 45, hjust=0.95, vjust=0.95, colour = 'black')) + theme(axis.text.y = element_text(colour = 'black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank()) + theme(plot.margin = margin(0, 0.15, -0.5, 0.15, "cm"))


#ASR
vac_inc <- read.csv('Output/Country_ASR_VacGroup.csv', stringsAsFactors = F)
vac_inc$age_group <- factor(vac_inc$age_group, levels = c('All', '0-14', '15-29', '30-64', '65+'), labels = c('Overall', '0-14', '15-29', '30-64', '65+'))
vac_inc$vac_cat <- factor(vac_inc$vac_cat, levels = c('0', '20-50', '51-70', '71-80', '81-90', '90+'), labels = c('No universal immunization', 'Vaccine coverage: 20-50%', 'Vaccine coverage: 51-70%', 'Vaccine coverage: 71-80%', 'Vaccine coverage: 81-90%', 'Vaccine coverage: >90%'))

p4 <- ggplot(vac_inc %>% filter(sex_name=='Both'&measure_name=='Incidence'&age_group!='Overall'), aes(colour=age_group, y=age_sum, x=year)) + geom_point(size=0.5) + geom_line() + facet_wrap(.~vac_cat, nrow = 1) +  scale_x_continuous(name = '', breaks = c(seq(1990, 2010, 5), 2017), labels = c(seq(1990, 2010, 5), 2017)) + scale_colour_manual(values=c( "#009E73", "#56B4E9", "#0072B2", "#D55E00")) + theme_bw() +  guides(colour=guide_legend(""))  + scale_y_log10(name = 'ASR (/10^5)', limits = c(1e-2,260), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + theme(axis.text.x = element_text(angle = 45, hjust=0.95, vjust=0.95, colour = 'black')) + theme(axis.text.y = element_text(colour = 'black'))  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank()) + theme(plot.margin = margin(0, 0.15, -0.5, 0.15, "cm"))
##
library(ggpubr)

ggarrange(p2, p1, p3, p4,
          labels = c("A", "B", 'C', 'D'), ncol = 1, nrow = 4, common.legend = T, align = 'hv', legend = 'bottom', font.label = list(size = 12))

