# pathway change
mypaths <- .libPaths()
mypaths <- c(mypaths, 'C:/R_lib')
mypaths <- c(mypaths[3], mypaths[2], mypaths[1])
.libPaths(mypaths)

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
library(tidyr)
library(broom)

#EAPC
setwd('D:/Postdoc/Postdoc/GLOBOCAN/GLOBOCAN_CervicalCancer/dataset/input')

cervical_incidence <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN/GLOBOCAN_CervicalCancer/dataset/output/CervicalCancer_2020_hpv&riskfactor.csv', stringsAsFactors = F)

##########Region_mapping#############
who_region <- read.csv('WHO_region.csv', stringsAsFactors = F, header = T)
colnames(who_region) <- c('country_name', 'who_region')
who_region <- who_region %>% mutate(country_name_abb=str_replace_all(country_name, " ", ""))

who_2020_incidence <- read.csv('WHO_2020/WHO_CervicalCancer_2020_incidence_allages.csv', stringsAsFactors = F, header = T)
colnames(who_2020_incidence) <- c('iso_code', 'country_name_abb', 'asir')
who_2020_incidence <- left_join(who_2020_incidence, who_region, by = c('country_name_abb'='country_name_abb'))

cervical_incidence <- left_join(cervical_incidence, who_2020_incidence[, c(1, 5)], by = c('ISO_code'='iso_code'))
cervical_incidence$who_region <- factor(cervical_incidence$who_region, levels = c('Eastern Africa', 'Middle Africa', 'Northern Africa', 'Southern Africa', 'Western Africa', 'Northern America', 'Central America', 'Caribbean', 'South America', 'Eastern Asia', 'South-Eastern Asia', 'South-Central Asia', 'Western Asia', "Northern Europe", "Central and Eastern Europe", "Western Europe", "Southern Europe", "Australia and New Zealand", "Melanesia", "Polynesia", "Micronesia"), labels = c(rep('Africa', 5), rep('America', 4), rep('Asia', 4), rep('Europe', 4), rep('Oceania', 4)))

cor.test(cervical_incidence$asir_cervicalcancer_female_allages, cervical_incidence$HPV)
cor.test(cervical_incidence$asmr_cervicalcancer_female_allages, cervical_incidence$HPV)
cor.test(cervical_incidence$asir_cervicalcancer_female_allages, cervical_incidence$HIV)
cor.test(cervical_incidence$asmr_cervicalcancer_female_allages, cervical_incidence$HIV)
cor.test(cervical_incidence$asir_cervicalcancer_female_allages, cervical_incidence$smoking_female)
cor.test(cervical_incidence$asmr_cervicalcancer_female_allages, cervical_incidence$smoking_female)

####Figure###
require(ggrepel)
library(scales)
p1 <- ggplot(cervical_incidence %>% filter(!is.na(HPV)), aes(HPV, asir_cervicalcancer_female_allages)) + geom_point(aes(shape = factor(who_region), colour = factor(who_region))) + stat_smooth(method = lm) + theme_bw() + labs(x="HPV (%)", y = "Age-standardized rate per 100,000") + theme(text = element_text(size = 10, colour = 'black', face = 'plain')) + theme(axis.text = element_text(color = 'black', size = 10, face = 'plain')) + ggtitle('Cervical cancer incidence') + theme(plot.title = element_text(hjust = 0.5)) + guides(shape=guide_legend(""), color=guide_legend('')) + scale_color_brewer(palette = 'Set1') #+ geom_text_repel(data = subset(cervical_incidence, HPV > 25), aes(HPV,asir_cervicalcancer_female_allages,label=location))

p2 <- ggplot(cervical_incidence %>% filter(!is.na(HPV)), aes(HPV, asmr_cervicalcancer_female_allages)) + geom_point(aes(shape = factor(who_region), colour = factor(who_region))) + stat_smooth(method = lm) + theme_bw() + labs(x="HPV (%)", y = "Age-standardized rate per 100,000") + theme(text = element_text(size = 10, colour = 'black', face = 'plain')) + theme(axis.text = element_text(color = 'black', size = 10, face = 'plain')) + ggtitle('Cervical cancer mortality') + theme(plot.title = element_text(hjust = 0.5)) + guides(shape=guide_legend(""), color=guide_legend('')) + scale_color_brewer(palette = 'Set1')

p3 <- ggplot(cervical_incidence %>% filter(!is.na(HIV)), aes(HIV, asir_cervicalcancer_female_allages)) + geom_point(aes(shape = factor(who_region), colour = factor(who_region))) + stat_smooth(method = lm) + theme_bw() + labs(x="HIV (%)", y = "Age-standardized rate per 100,000") + theme(text = element_text(size = 10, colour = 'black', face = 'plain')) + theme(axis.text = element_text(color = 'black', size = 10, face = 'plain')) + ggtitle('Cervical cancer incidence') + theme(plot.title = element_text(hjust = 0.5)) + geom_text_repel(data = subset(cervical_incidence, HIV > 7), aes(HIV,asir_cervicalcancer_female_allages,label=location)) + guides(shape=guide_legend(""), color=guide_legend('')) + scale_color_brewer(palette = 'Set1')

p4 <- ggplot(cervical_incidence %>% filter(!is.na(HIV)), aes(HIV, asmr_cervicalcancer_female_allages)) + geom_point(aes(shape = factor(who_region), colour = factor(who_region))) + stat_smooth(method = lm) + theme_bw() + labs(x="HIV (%)", y = "Age-standardized rate per 100,000") + theme(text = element_text(size = 10, colour = 'black', face = 'plain')) + theme(axis.text = element_text(color = 'black', size = 10, face = 'plain')) + ggtitle('Cervical cancer mortality') + theme(plot.title = element_text(hjust = 0.5)) + geom_text_repel(data = subset(cervical_incidence, HIV > 7), aes(HIV,asmr_cervicalcancer_female_allages,label=location)) + guides(shape=guide_legend(""), color=guide_legend('')) + scale_color_brewer(palette = 'Set1') #+ scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))

p5 <- ggplot(cervical_incidence %>% filter(!is.na(smoking_female)), aes(smoking_female, asir_cervicalcancer_female_allages)) + geom_point(aes(shape = factor(who_region), colour = factor(who_region))) + stat_smooth(method = lm) + theme_bw()+labs(x="Smoking (%)", y = "Age-standardized rate per 100,000") + theme(text = element_text(size = 10, colour = 'black', face = 'plain')) + theme(axis.text = element_text(color = 'black', size = 10, face = 'plain')) + ggtitle('Cervical cancer incidence') + theme(plot.title = element_text(hjust = 0.5)) + guides(shape=guide_legend(""), color=guide_legend('')) + scale_color_brewer(palette = 'Set1')

p6 <- ggplot(cervical_incidence %>% filter(!is.na(smoking_female)), aes(smoking_female, asmr_cervicalcancer_female_allages)) + geom_point(aes(shape = factor(who_region), colour = factor(who_region))) + stat_smooth(method = lm) + theme_bw() + labs(x="Smoking (%)", y = "Age-standardized rate per 100,000") + theme(text = element_text(size = 10, colour = 'black', face = 'plain')) + theme(axis.text = element_text(color = 'black', size = 10, face = 'plain'))  + ggtitle('Cervical cancer mortality') + theme(plot.title = element_text(hjust = 0.5)) + guides(shape=guide_legend(""), color=guide_legend('')) + scale_color_brewer(palette = 'Set1')

#screening_coverage
cervical_incidence$coverage_2015_cat <- factor(cervical_incidence$coverage_2015_cat, labels = c('<10', '10-50', '50-70', '70+', 'Unknown'))

p7 <- ggplot(cervical_incidence %>% filter(!is.na(asir_cervicalcancer_female_allages)&!is.na(coverage_2015_cat)), aes(x=coverage_2015_cat, y=asir_cervicalcancer_female_allages, color=who_region)) + geom_boxplot(color='black', outlier.shape = NA) + geom_jitter(aes(shape = factor(who_region), colour = factor(who_region)), position=position_jitter(0.2)) + theme_bw() + labs(x="Cervical cancer screening coverage (2015)", y = "Age-standardized rate per 100,000") + theme(text = element_text(size = 10, colour = 'black', face = 'plain')) + theme(axis.text = element_text(color = 'black', size = 10, face = 'plain'))  + ggtitle('Cervical cancer incidence') + theme(plot.title = element_text(hjust = 0.5)) + guides(shape=guide_legend(""), color=guide_legend('')) + scale_color_brewer(palette = 'Set1') + theme(legend.position = 'none')

res.aov <- aov(asir_cervicalcancer_female_allages ~ coverage_2015_cat, data = cervical_incidence %>% filter(!is.na(asir_cervicalcancer_female_allages)&!is.na(coverage_2015_cat)))
summary(res.aov)

p8 <- ggplot(cervical_incidence %>% filter(!is.na(asmr_cervicalcancer_female_allages)&!is.na(coverage_2015_cat)), aes(x=coverage_2015_cat, y=asmr_cervicalcancer_female_allages, color=who_region)) + geom_boxplot(color='black', outlier.shape = NA) + geom_jitter(aes(shape = factor(who_region), colour = factor(who_region)), position=position_jitter(0.2)) + theme_bw() + labs(x="Cervical cancer screening coverage (2015)", y = "Age-standardized rate per 100,000") + theme(text = element_text(size = 10, colour = 'black', face = 'plain')) + theme(axis.text = element_text(color = 'black', size = 10, face = 'plain'))  + ggtitle('Cervical cancer mortality') + theme(plot.title = element_text(hjust = 0.5)) + guides(shape=guide_legend(""), color=guide_legend('')) + scale_color_brewer(palette = 'Set1') + theme(legend.position = 'none')

res.aov <- aov(asmr_cervicalcancer_female_allages ~ coverage_2015_cat, data = cervical_incidence %>% filter(!is.na(asmr_cervicalcancer_female_allages)&!is.na(coverage_2015_cat)))
summary(res.aov)


###########
library(ggpubr)
ggarrange(p5, p6, p1, p2, p3, p4, p7, p8,
          labels = c("A", "B", 'C', 'D', 'E', 'F', 'G', 'H'),
          ncol = 2, nrow = 4, common.legend = T, legend = 'bottom')
##ref
browseURL('https://ggplot2.tidyverse.org/reference/scale_brewer.html')
