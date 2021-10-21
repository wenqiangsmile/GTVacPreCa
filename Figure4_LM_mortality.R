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
library(tidyr)
library(broom)

###########################
setwd('D:/Postdoc/Postdoc/GLOBOCAN_CervicalCancer/dataset/output')

CI5plus <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN_CervicalCancer/dataset/output/CervicalCancer_mortality_ci5&seer&nordic.csv', stringsAsFactors = F)[, 1:7]
CI5plus <- CI5plus %>% mutate(exclusion=ifelse(name=='Iceland'&Year<=1985, 'yes', 'no'))
CI5plus <- CI5plus %>% filter(exclusion=='no') %>% select(1:7)

CI5plus <- CI5plus %>% mutate(se=asir/sqrt(n))
CI5plus <- CI5plus %>% arrange(region, name)
CI5plus$name_code <- as.integer(as.factor(CI5plus$name))

CI5plus_all <- CI5plus
CI5plus_all <- CI5plus_all %>% mutate(group='all')

###aged15to49####
CI5plus <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN_CervicalCancer/dataset/output/CervicalCancer_mortality_ci5&seer&nordic_15to49.csv', stringsAsFactors = F)

CI5plus <- CI5plus %>% mutate(se=asir/sqrt(n))
CI5plus <- CI5plus %>% arrange(region, name)
CI5plus$name_code <- as.integer(as.factor(CI5plus$name))

CI5plus_young <- CI5plus
CI5plus_young <- CI5plus_young %>% mutate(group='young')

###aged50to85####
CI5plus <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN_CervicalCancer/dataset/output/CervicalCancer_mortality_ci5&seer&nordic_50to85.csv', stringsAsFactors = F)

CI5plus <- CI5plus %>% mutate(se=asir/sqrt(n))
CI5plus <- CI5plus %>% arrange(region, name)
CI5plus$name_code <- as.integer(as.factor(CI5plus$name))

CI5plus_old <- CI5plus
CI5plus_old <- CI5plus_old %>% mutate(group='old')

#############################Binding#####
CI5plus <- bind_rows(CI5plus_all, CI5plus_young, CI5plus_old)

CI5plus <- CI5plus %>% filter(name!='United Kingdom, England and Wales'&name!='United Kingdom, Northern Ireland'&name!='United Kingdom, Scotland'&name!='Faroe Islands')
CI5plus <- CI5plus %>% mutate(country_name=ifelse(name=='Republic of Korea', 'South Korea', ifelse(name=='Hong Kong SAR', 'Hong Kong, China', ifelse(name=='United States of America', 'United States', ifelse(name=='Russian Federation', 'Russia', name)))))

CI5plus <- CI5plus[, c(11, 7, 9:10)]
colnames(CI5plus)[1] <- 'name_country'

CI5plus$region <- factor(CI5plus$region, levels = c('Asia', 'Oceania', 'Northern America', 'Southern America', 'Northern Europe', 'Western Europe', 'Southern Europe', 'Eastern Europe'), labels = c('Asia', 'Oceania', 'Northern America', 'Southern America', 'Northern Europe', 'Western Europe', 'Southern Europe', 'Eastern Europe'))


############Predicted##################
aapc <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN_CervicalCancer/dataset/output/aapc/mortality/years_only_who_quality/jp_output_mortality_who&nordic_data.csv')
aapc <- aapc %>% mutate(group='all')

aapc_young <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN_CervicalCancer/dataset/output/aapc/mortality/years_only_who_quality/jp_output_mortality_who&nordic_15to49_data.csv')
aapc_young <- aapc_young %>% mutate(group='young')

aapc_old <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN_CervicalCancer/dataset/output/aapc/mortality/years_only_who_quality/jp_output_mortality_who&nordic_50to85_data.csv')
aapc_old <- aapc_old %>% mutate(group='old')

aapc <- bind_rows(aapc, aapc_young, aapc_old)

aapc <- aapc %>% select(1:4, 9) 
colnames(aapc) <- c('name_code', 'year', 'asr', 'asr_predict', 'group')

aapc_country <- left_join(CI5plus, aapc, by = c('name_code'='name_code', 'group'='group'))

aapc_country <- aapc_country %>% group_by(region, name_country) %>% arrange(region, name_country)

##############
aapc_country <- aapc_country %>% filter(name_country!='Turkey')

aapc_country$group <- factor(aapc_country$group, levels = c('all', 'young', 'old'), labels = c('0 to 85+', '15 to 49', '50 to 85+'))

country_name <- aapc_country %>% group_by(region, name_country) %>% summarise(n=n())
aapc_country$name_country <- factor(aapc_country$name_country, levels = country_name$name_country) 

aapc_country <- aapc_country %>% filter(year>=1990)
ggplot(aapc_country, aes(x = year, y = asr, color = group)) + geom_point(size=0.05) + geom_line(aes(x = year, y = asr_predict), size=0.05) + facet_wrap(~name_country, scales = 'free', ncol = 6) + theme_bw() + theme(legend.position = 'bottom') + xlab('Calender year') + ylab('Age-standardized rate of mortality')  + theme(text = element_text(size = 8, colour = 'black', face = 'plain')) + theme(axis.text = element_text(color = 'black', size = 7, face = 'plain')) + scale_color_brewer(palette = 'Set1') + scale_x_continuous(limits=c(1990,2018), breaks = c(1990, 2000, 2010, 2018), labels = c(1990, 2000, 2010, 2018)) + scale_y_continuous(limits=c(0,NA)) + labs(color = 'Age in years') + theme(axis.line = element_line(colour = 'black', size = 0.1), axis.ticks = element_line(color = 'black', size = 0.1), strip.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())

