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

CI5plus <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN_CervicalCancer/dataset/output/CervicalCancer_incidence_ci5&seer&nordic.csv', stringsAsFactors = F)

CI5plus <- CI5plus %>% mutate(se=asir/sqrt(n))
CI5plus <- CI5plus %>% arrange(region, registry_country)
CI5plus$registry_country_code <- as.integer(as.factor(CI5plus$registry_country))
CI5plus <- CI5plus %>% select(1, 7, 9) %>% group_by(registry_country) %>% slice(which.min(registry_country_code))

CI5plus_all <- CI5plus
CI5plus_all <- CI5plus_all %>% mutate(group='all')

#####################15to49###################
CI5plus <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN_CervicalCancer/dataset/output/CervicalCancer_incidence_ci5&seer&nordic_15to49.csv', stringsAsFactors = F)

CI5plus <- CI5plus %>% mutate(se=asir/sqrt(n))
CI5plus <- CI5plus %>% arrange(region, registry_country)
CI5plus$registry_country_code <- as.integer(as.factor(CI5plus$registry_country))
CI5plus <- CI5plus %>% select(1, 7, 9) %>% group_by(registry_country) %>% slice(which.min(registry_country_code))

CI5plus_young <- CI5plus
CI5plus_young <- CI5plus_young %>% mutate(group='young')

#####################50to85###################
CI5plus <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN_CervicalCancer/dataset/output/CervicalCancer_incidence_ci5&seer&nordic_50to85.csv', stringsAsFactors = F)

CI5plus <- CI5plus %>% mutate(se=asir/sqrt(n))
CI5plus <- CI5plus %>% arrange(region, registry_country)
CI5plus$registry_country_code <- as.integer(as.factor(CI5plus$registry_country))
CI5plus <- CI5plus %>% select(1, 7, 9) %>% group_by(registry_country) %>% slice(which.min(registry_country_code))

CI5plus_old <- CI5plus
CI5plus_old <- CI5plus_old %>% mutate(group='old')

############Region#####
CI5plus <- bind_rows(CI5plus_all, CI5plus_young, CI5plus_old)

CI5plus <- CI5plus %>% mutate(registry_country_edit=ifelse(registry_country=='Costa-Rica', 'Costa Rica', ifelse(registry_country=='Czech', 'Czech Republic', ifelse(registry_country=='England', 'UK: England', ifelse(registry_country=='HongKong', 'Hong Kong SAR', ifelse(registry_country=='New-Zealand', 'New Zealand', ifelse(registry_country=='Northern-Ireland', 'UK: Northern Ireland', ifelse(registry_country=='Scotland', 'UK: Scotland', ifelse(registry_country=='USA', 'USA', registry_country)))))))))
CI5plus <- CI5plus[, c(5, 2:4)]
colnames(CI5plus)[1] <- 'registry_country'

CI5plus$region <- factor(CI5plus$region, levels = c('Asia', 'Oceania', 'Northern America', 'Southern America', 'Northern Europe', 'Western Europe', 'Southern Europe', 'Eastern Europe', 'Africa'), labels = c('Asia', 'Oceania', 'Northern America', 'Southern America', 'Northern Europe', 'Western Europe', 'Southern Europe', 'Eastern Europe', 'Africa'))


####predicted_incidence######
aapc <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN_CervicalCancer/dataset/output/aapc/allyears_ci5&seer&nordic/jp_output_ci5plus&seer&nordic_data.csv')
aapc <- aapc %>% mutate(group='all')

aapc_young <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN_CervicalCancer/dataset/output/aapc/allyears_ci5&seer&nordic/jp_output_ci5plus&seer&nordic_15to49_data.csv')
aapc_young <- aapc_young %>% mutate(group='young')

aapc_old <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN_CervicalCancer/dataset/output/aapc/allyears_ci5&seer&nordic/jp_output_ci5plus&seer&nordic_50to85_data.csv')
aapc_old <- aapc_old %>% mutate(group='old')

aapc <- bind_rows(aapc, aapc_young, aapc_old)

aapc <- aapc %>% select(1:4, 9) 
colnames(aapc) <- c('name_code', 'year', 'asr', 'asr_predict', 'group')

aapc_country <- left_join(CI5plus, aapc, by = c('registry_country_code'='name_code', 'group'='group'))

aapc_country <- aapc_country %>% group_by(region, registry_country) %>% arrange(region, registry_country)

##############
aapc_country$region <- factor(aapc_country$region, levels = c('Asia', 'Oceania', 'Northern America', 'Southern America', 'Northern Europe', 'Western Europe', 'Southern Europe', 'Eastern Europe', 'Africa'), labels = c('Asia', 'Oceania', 'Northern America', 'Southern America', 'Northern Europe', 'Western Europe', 'Southern Europe', 'Eastern Europe', 'Africa'))
aapc_country <- aapc_country %>% filter(registry_country!='UK: Northern Ireland'&registry_country!='UK: Scotland'&registry_country!='USA-Black'&registry_country!='USA-White')

aapc_country <- aapc_country %>% mutate(registry_country_edit=ifelse(registry_country=='Hong Kong SAR', 'Hong Kong, China', ifelse(registry_country=='USA', 'United States', ifelse(registry_country=='UK: England', 'United Kingdom: England', ifelse(registry_country=='Korea', 'South Korea', registry_country)))))

aapc_country$group <- factor(aapc_country$group, levels = c('all', 'young', 'old'), labels = c('0 to 85+', '15 to 49', '50 to 85+'))

country_name <- aapc_country %>% group_by(region, registry_country_edit) %>% summarise(n=n())
aapc_country$registry_country_edit <- factor(aapc_country$registry_country_edit, levels = country_name$registry_country_edit) 

aapc_country <- aapc_country %>% filter(year>=1990)

ggplot(aapc_country, aes(x = year, y = asr, color = group)) + geom_point(size=0.05) + geom_line(aes(x = year, y = asr_predict), size=0.05) + facet_wrap(~registry_country_edit, scales = 'free', ncol = 6) + theme_bw() + theme(legend.position = 'bottom') + xlab('Calender year') + ylab('Age-standardized rate of incidence')  + theme(text = element_text(size = 8, colour = 'black', face = 'plain')) + theme(axis.text = element_text(color = 'black', size = 7, face = 'plain')) + scale_color_brewer(palette = 'Set1') + scale_x_continuous(limits=c(1990,2016), breaks = c(1990, 2000, 2010, 2016), labels = c(1990, 2000, 2010, 2016)) + scale_y_continuous(limits=c(0,NA)) + labs(color = 'Age in years') + theme(axis.line = element_line(colour = 'black', size = 0.1), axis.ticks = element_line(color = 'black', size = 0.1), strip.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())

