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

###########################Incidence#############
setwd('D:/Postdoc/Postdoc/GLOBOCAN/GLOBOCAN_CervicalCancer/dataset/output')

CI5plus <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN/GLOBOCAN_CervicalCancer/dataset/output/CervicalCancer_incidence_ci5&seer&nordic_15to34.csv', stringsAsFactors = F)

CI5plus <- CI5plus %>% mutate(se=asir/sqrt(n))
CI5plus <- CI5plus %>% arrange(region, registry_country)
CI5plus$registry_country_code <- as.integer(as.factor(CI5plus$registry_country))

CI5plus <- CI5plus %>% select(1, 7, 9) %>% group_by(registry_country) %>% slice(which.min(registry_country_code))

CI5plus <- CI5plus %>% mutate(registry_country_edit=ifelse(registry_country=='Costa-Rica', 'Costa Rica', ifelse(registry_country=='Czech', 'Czech Republic', ifelse(registry_country=='England', 'United Kingdom', ifelse(registry_country=='HongKong', 'Hong Kong SAR', ifelse(registry_country=='New-Zealand', 'New Zealand', ifelse(registry_country=='Northern-Ireland', 'United Kingdom, Northern Ireland', ifelse(registry_country=='Scotland', 'United Kingdom, Scotland', ifelse(registry_country=='USA', 'United States of America', registry_country)))))))))
CI5plus <- CI5plus[, c(4, 2:3)]
colnames(CI5plus)[1] <- 'registry_country'

CI5plus$region <- factor(CI5plus$region, levels = c('Asia', 'Oceania', 'Northern America', 'Southern America', 'Northern Europe', 'Western Europe', 'Southern Europe', 'Eastern Europe', 'Africa'), labels = c('Asia', 'Oceania', 'Northern America', 'Southern America', 'Northern Europe', 'Western Europe', 'Southern Europe', 'Eastern Europe', 'Africa'))

####
aapc <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN/GLOBOCAN_CervicalCancer/dataset/output/aapc/allyears_ci5&seer&nordic/jp_output_ci5plus&seer&nordic_aapc_15to34.csv')
aapc <- aapc %>% select(1, 3, 6:8) %>% filter(X.AAPC.Index.==1)
colnames(aapc) <- c('registry_country_code', 'aapc_index', 'aapc', 'aapc_lci', 'aapc_uci')

aapc_country <- left_join(CI5plus, aapc[, c(1, 3:5)], by = c('registry_country_code'='registry_country_code'))

aapc_country <- aapc_country %>% group_by(region, registry_country) %>% arrange(region, registry_country)

aapc_country <- aapc_country %>% filter(registry_country!='United Kingdom, England and Wales'&registry_country!='United Kingdom, Northern Ireland'&registry_country!='United Kingdom, Scotland'&registry_country!='USA-Black'&registry_country!='USA-White')
aapc_country <- aapc_country %>% mutate(country_name=ifelse(registry_country=='Korea', 'South Korea', ifelse(registry_country=='Hong Kong SAR', 'Hong Kong, China', ifelse(registry_country=='United States of America', 'United States', ifelse(registry_country=='Russian Federation', 'Russia', ifelse(registry_country=='China', 'China', registry_country))))))
aapc_country <- aapc_country %>% mutate(trend_grp=ifelse(aapc_lci>=0, 'increase', ifelse(aapc_uci<=0, 'decrease', 'stable')))
trend_sum <- aapc_country %>% group_by(trend_grp) %>% summarise(n=n())

####Figure####
library(ggplot2)

p1 <- ggplot(aapc_country, aes(x = reorder(country_name, -aapc), y = aapc, group = region)) + geom_bar(stat = 'identity', width = 0.75, fill = '#0072B2') + coord_flip() + theme_bw() + facet_grid(region~., scales = 'free_y', space = 'free_y') + theme(strip.text.y = element_text(angle = 0)) + scale_x_discrete(name = '')  + scale_y_continuous(name = 'AAPC in the last 5 years') + geom_errorbar(aes(ymin=aapc_lci, ymax=aapc_uci), size=0.4, width=.25, position=position_dodge(0.9), colour = 'black')


#####################15to49###################
CI5plus <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN/GLOBOCAN_CervicalCancer/dataset/output/CervicalCancer_incidence_ci5&seer&nordic_15to49.csv', stringsAsFactors = F)

CI5plus <- CI5plus %>% mutate(se=asir/sqrt(n))
CI5plus <- CI5plus %>% arrange(region, registry_country)
CI5plus$registry_country_code <- as.integer(as.factor(CI5plus$registry_country))

CI5plus <- CI5plus %>% select(1, 7, 9) %>% group_by(registry_country) %>% slice(which.min(registry_country_code))

CI5plus <- CI5plus %>% mutate(registry_country_edit=ifelse(registry_country=='Costa-Rica', 'Costa Rica', ifelse(registry_country=='Czech', 'Czech Republic', ifelse(registry_country=='England', 'United Kingdom', ifelse(registry_country=='HongKong', 'Hong Kong SAR', ifelse(registry_country=='New-Zealand', 'New Zealand', ifelse(registry_country=='Northern-Ireland', 'United Kingdom, Northern Ireland', ifelse(registry_country=='Scotland', 'United Kingdom, Scotland', ifelse(registry_country=='USA', 'United States of America', registry_country)))))))))
CI5plus <- CI5plus[, c(4, 2:3)]
colnames(CI5plus)[1] <- 'registry_country'

CI5plus$region <- factor(CI5plus$region, levels = c('Asia', 'Oceania', 'Northern America', 'Southern America', 'Northern Europe', 'Western Europe', 'Southern Europe', 'Eastern Europe', 'Africa'), labels = c('Asia', 'Oceania', 'Northern America', 'Southern America', 'Northern Europe', 'Western Europe', 'Southern Europe', 'Eastern Europe', 'Africa'))

####
aapc <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN/GLOBOCAN_CervicalCancer/dataset/output/aapc/allyears_ci5&seer&nordic/jp_output_ci5plus&seer&nordic_aapc_15to49.csv')
aapc <- aapc %>% select(1, 3, 6:8) %>% filter(X.AAPC.Index.==2)
colnames(aapc) <- c('registry_country_code', 'aapc_index', 'aapc', 'aapc_lci', 'aapc_uci')

aapc_country <- left_join(CI5plus, aapc[, c(1, 3:5)], by = c('registry_country_code'='registry_country_code'))

aapc_country <- aapc_country %>% group_by(region, registry_country) %>% arrange(region, registry_country)

aapc_country <- aapc_country %>% filter(registry_country!='United Kingdom, England and Wales'&registry_country!='United Kingdom, Northern Ireland'&registry_country!='United Kingdom, Scotland'&registry_country!='USA-Black'&registry_country!='USA-White')
aapc_country <- aapc_country %>% mutate(country_name=ifelse(registry_country=='Korea', 'South Korea', ifelse(registry_country=='Hong Kong SAR', 'Hong Kong, China', ifelse(registry_country=='United States of America', 'United States', ifelse(registry_country=='Russian Federation', 'Russia', ifelse(registry_country=='China', 'China', registry_country))))))

aapc_country <- aapc_country %>% mutate(trend_grp=ifelse(aapc_lci>=0, 'increase', ifelse(aapc_uci<=0, 'decrease', 'stable')))
trend_sum <- aapc_country %>% group_by(trend_grp) %>% summarise(n=n())
trend_sum <- aapc_country %>% group_by(region, trend_grp) %>% summarise(n=n())

####Figure####
library(ggplot2)
require(forcats)
p2 <- ggplot(aapc_country, aes(x = fct_rev(country_name), y = aapc, group = region)) + geom_bar(stat = 'identity', width = 0.75, fill = '#0072B2') + coord_flip() + theme_bw() + facet_grid(region~., scales = 'free_y', space = 'free_y') + theme(strip.text.y = element_text(angle = 0)) + scale_x_discrete(name = '')  + scale_y_continuous(name = 'AAPC in the last 5 years', limits=c(-25, 20), breaks=c(-25, -20, -15, -10, -5, 0, 5, 10, 15, 20), labels=c('-25', '-20', '-15', '-10', '-5', "0", '5', "10", '15', '20')) + geom_errorbar(aes(ymin=aapc_lci, ymax=aapc_uci), size=0.4, width=.25, position=position_dodge(0.9), colour = 'black') + theme(axis.text.y = element_text(hjust = 0))


#####################50to85###################
CI5plus <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN/GLOBOCAN_CervicalCancer/dataset/output/CervicalCancer_incidence_ci5&seer&nordic_50to85.csv', stringsAsFactors = F)

CI5plus <- CI5plus %>% mutate(se=asir/sqrt(n))
CI5plus <- CI5plus %>% arrange(region, registry_country)
CI5plus$registry_country_code <- as.integer(as.factor(CI5plus$registry_country))

CI5plus <- CI5plus %>% select(1, 7, 9) %>% group_by(registry_country) %>% slice(which.min(registry_country_code))

CI5plus <- CI5plus %>% mutate(registry_country_edit=ifelse(registry_country=='Costa-Rica', 'Costa Rica', ifelse(registry_country=='Czech', 'Czech Republic', ifelse(registry_country=='England', 'United Kingdom', ifelse(registry_country=='HongKong', 'Hong Kong SAR', ifelse(registry_country=='New-Zealand', 'New Zealand', ifelse(registry_country=='Northern-Ireland', 'United Kingdom, Northern Ireland', ifelse(registry_country=='Scotland', 'United Kingdom, Scotland', ifelse(registry_country=='USA', 'United States of America', registry_country)))))))))
CI5plus <- CI5plus[, c(4, 2:3)]
colnames(CI5plus)[1] <- 'registry_country'

CI5plus$region <- factor(CI5plus$region, levels = c('Asia', 'Oceania', 'Northern America', 'Southern America', 'Northern Europe', 'Western Europe', 'Southern Europe', 'Eastern Europe', 'Africa'), labels = c('Asia', 'Oceania', 'Northern America', 'Southern America', 'Northern Europe', 'Western Europe', 'Southern Europe', 'Eastern Europe', 'Africa'))

####
aapc <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN/GLOBOCAN_CervicalCancer/dataset/output/aapc/allyears_ci5&seer&nordic/jp_output_ci5plus&seer&nordic_aapc_50to85.csv')
aapc <- aapc %>% select(1, 3, 6:8) %>% filter(X.AAPC.Index.==2)
colnames(aapc) <- c('registry_country_code', 'aapc_index', 'aapc', 'aapc_lci', 'aapc_uci')

aapc_country <- left_join(CI5plus, aapc[, c(1, 3:5)], by = c('registry_country_code'='registry_country_code'))

aapc_country <- aapc_country %>% group_by(region, registry_country) %>% arrange(region, registry_country)

aapc_country <- aapc_country %>% filter(registry_country!='United Kingdom, England and Wales'&registry_country!='United Kingdom, Northern Ireland'&registry_country!='United Kingdom, Scotland'&registry_country!='USA-Black'&registry_country!='USA-White'&registry_country!='Bahrain')
aapc_country <- aapc_country %>% mutate(country_name=ifelse(registry_country=='Korea', 'South Korea', ifelse(registry_country=='Hong Kong SAR', 'Hong Kong, China', ifelse(registry_country=='United States of America', 'United States', ifelse(registry_country=='Russian Federation', 'Russia', ifelse(registry_country=='China', 'China', registry_country))))))

aapc_country <- aapc_country %>% mutate(trend_grp=ifelse(aapc_lci>=0, 'increase', ifelse(aapc_uci<=0, 'decrease', 'stable')))
trend_sum <- aapc_country %>% group_by(trend_grp) %>% summarise(n=n())

####Figure####
library(ggplot2)

p3 <- ggplot(aapc_country, aes(x = fct_rev(country_name), y = aapc, group = region)) + geom_bar(stat = 'identity', width = 0.75, fill = '#0072B2') + coord_flip() + theme_bw() + facet_grid(region~., scales = 'free_y', space = 'free_y') + theme(strip.text.y = element_text(angle = 0)) + scale_x_discrete(name = '')  + scale_y_continuous(name = 'AAPC in the last 5 years', limits=c(-25, 20), breaks=c(-25, -20, -15, -10, -5, 0, 5, 10, 15, 20), labels=c('-25', '-20', '-15', '-10', '-5', "0", '5', "10", '15', '20')) + geom_errorbar(aes(ymin=aapc_lci, ymax=aapc_uci), size=0.4, width=.25, position=position_dodge(0.9), colour = 'black') + theme(axis.text.y = element_text(hjust = 0))

library(ggpubr)
ggarrange(p1, p2, ncol = 2, labels = c("A", "B"))

ggarrange(p2, p3, ncol = 2, labels = c("A", "B"))


###################Mortality##################
setwd('D:/Postdoc/Postdoc/GLOBOCAN/GLOBOCAN_CervicalCancer/dataset/output')

CI5plus <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN/GLOBOCAN_CervicalCancer/dataset/output/CervicalCancer_mortality_ci5&seer&nordic_15to34.csv', stringsAsFactors = F)
CI5plus <- CI5plus %>% mutate(exclusion=ifelse(name=='Iceland'&Year<=1985, 'yes', 'no'))
CI5plus <- CI5plus %>% filter(exclusion=='no') %>% select(1:7)

CI5plus <- CI5plus %>% mutate(se=asir/sqrt(n))
CI5plus <- CI5plus %>% arrange(region, name)
CI5plus$name_code <- as.integer(as.factor(CI5plus$name))

CI5plus <- CI5plus %>% select(1, 7, 9) %>% group_by(name) %>% slice(which.min(name_code))

CI5plus$region <- factor(CI5plus$region, levels = c('Asia', 'Oceania', 'Northern America', 'Southern America', 'Northern Europe', 'Western Europe', 'Southern Europe', 'Eastern Europe'), labels = c('Asia', 'Oceania', 'Northern America', 'Southern America', 'Northern Europe', 'Western Europe', 'Southern Europe', 'Eastern Europe'))


###aapc####
aapc <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN/GLOBOCAN_CervicalCancer/dataset/output/aapc/mortality/years_only_who_quality/jp_output_mortality_who&nordic_aapc_15to34.csv')
aapc <- aapc %>% select(1, 3, 6:8) %>% group_by(X.name_code.)%>% slice(which.max(X.AAPC.Index.))
colnames(aapc) <- c('name_code', 'aapc_index', 'aapc', 'aapc_lci', 'aapc_uci')

aapc_country <- left_join(CI5plus, aapc[, c(1, 3:5)], by = c('name_code'='name_code'))

aapc_country <- aapc_country %>% group_by(region, name) %>% arrange(region, name)

aapc_country <- aapc_country %>% filter(name!='United Kingdom, England and Wales'&name!='United Kingdom, Northern Ireland'&name!='United Kingdom, Scotland')
aapc_country <- aapc_country %>% mutate(country_name=ifelse(name=='Republic of Korea', 'South Korea', ifelse(name=='Hong Kong SAR', 'Hong Kong, China', ifelse(name=='United States of America', 'United States', ifelse(name=='Russian Federation', 'Russia', name)))))

####Figure####
p1 <- ggplot(aapc_country, aes(x = fct_rev(country_name), y = aapc, group = region)) + geom_bar(stat = 'identity', width = 0.75, fill = '#D55E00') + coord_flip() + theme_bw() + facet_grid(region~., scales = 'free_y', space = 'free_y') + theme(strip.text.y = element_text(angle = 0)) + scale_x_discrete(name = '')  + scale_y_continuous(name = 'AAPC in the last 5 years') + geom_errorbar(aes(ymin=aapc_lci, ymax=aapc_uci), size=0.4, width=.25, position=position_dodge(0.9), colour = 'black') + theme(axis.text.y = element_text(hjust = 0))

###########15to49###############
CI5plus <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN/GLOBOCAN_CervicalCancer/dataset/output/CervicalCancer_mortality_ci5&seer&nordic_15to49.csv', stringsAsFactors = F)
CI5plus <- CI5plus %>% mutate(exclusion=ifelse(name=='Iceland'&Year<=1985, 'yes', 'no'))
CI5plus <- CI5plus %>% filter(exclusion=='no') %>% select(1:7)

CI5plus <- CI5plus %>% mutate(se=asir/sqrt(n))
CI5plus <- CI5plus %>% arrange(region, name)
CI5plus$name_code <- as.integer(as.factor(CI5plus$name))

CI5plus <- CI5plus %>% select(1, 7, 9) %>% group_by(name) %>% slice(which.min(name_code))

CI5plus$region <- factor(CI5plus$region, levels = c('Asia', 'Oceania', 'Northern America', 'Southern America', 'Northern Europe', 'Western Europe', 'Southern Europe', 'Eastern Europe'), labels = c('Asia', 'Oceania', 'Northern America', 'Southern America', 'Northern Europe', 'Western Europe', 'Southern Europe', 'Eastern Europe'))


###aapc####
aapc <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN/GLOBOCAN_CervicalCancer/dataset/output/aapc/mortality/years_only_who_quality/jp_output_mortality_who&nordic_aapc_15to49.csv')
aapc <- aapc %>% select(1, 3, 6:8) %>% group_by(X.name_code.)%>% slice(which.max(X.AAPC.Index.))
colnames(aapc) <- c('name_code', 'aapc_index', 'aapc', 'aapc_lci', 'aapc_uci')

aapc_country <- left_join(CI5plus, aapc[, c(1, 3:5)], by = c('name_code'='name_code'))

aapc_country <- aapc_country %>% group_by(region, name) %>% arrange(region, name)

aapc_country <- aapc_country %>% filter(name!='United Kingdom, England and Wales'&name!='United Kingdom, Northern Ireland'&name!='United Kingdom, Scotland'&name!='Cyprus')
aapc_country <- aapc_country %>% mutate(country_name=ifelse(name=='Republic of Korea', 'South Korea', ifelse(name=='Hong Kong SAR', 'Hong Kong, China', ifelse(name=='United States of America', 'United States', ifelse(name=='Russian Federation', 'Russia', name)))))

aapc_country <- aapc_country %>% mutate(trend_grp=ifelse(aapc_lci>=0, 'increase', ifelse(aapc_uci<=0, 'decrease', 'stable')))
trend_sum <- aapc_country %>% group_by(trend_grp) %>% summarise(n=n())
trend_sum <- aapc_country %>% group_by(region, trend_grp) %>% summarise(n=n())
####Figure####

p2 <- ggplot(aapc_country, aes(x = fct_rev(country_name), y = aapc, group = region)) + geom_bar(stat = 'identity', width = 0.75, fill = '#E41A1C') + coord_flip() + theme_bw() + facet_grid(region~., scales = 'free_y', space = 'free_y') + theme(strip.text.y = element_text(angle = 0)) + scale_x_discrete(name = '')  + scale_y_continuous(name = 'AAPC in the last 5 years', limits=c(-20, 22.5), breaks=c(-20, -15, -10, -5, 0, 5, 10, 15, 20), labels=c('20', '15', '10', '5', "0", '5', "10", '15', "20")) + geom_errorbar(aes(ymin=aapc_lci, ymax=aapc_uci), size=0.4, width=.25, position=position_dodge(0.9), colour = 'black') + theme(axis.text.y = element_text(hjust = 0))

###########50to85###############
CI5plus <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN/GLOBOCAN_CervicalCancer/dataset/output/CervicalCancer_mortality_ci5&seer&nordic_50to85.csv', stringsAsFactors = F)
CI5plus <- CI5plus %>% mutate(exclusion=ifelse(name=='Iceland'&Year<=1985, 'yes', 'no'))
CI5plus <- CI5plus %>% filter(exclusion=='no') %>% select(1:7)

CI5plus <- CI5plus %>% mutate(se=asir/sqrt(n))
CI5plus <- CI5plus %>% arrange(region, name)
CI5plus$name_code <- as.integer(as.factor(CI5plus$name))

CI5plus <- CI5plus %>% select(1, 7, 9) %>% group_by(name) %>% slice(which.min(name_code))

CI5plus$region <- factor(CI5plus$region, levels = c('Asia', 'Oceania', 'Northern America', 'Southern America', 'Northern Europe', 'Western Europe', 'Southern Europe', 'Eastern Europe'), labels = c('Asia', 'Oceania', 'Northern America', 'Southern America', 'Northern Europe', 'Western Europe', 'Southern Europe', 'Eastern Europe'))


###aapc####
aapc <- read.csv('D:/Postdoc/Postdoc/GLOBOCAN/GLOBOCAN_CervicalCancer/dataset/output/aapc/mortality/years_only_who_quality/jp_output_mortality_who&nordic_aapc_50to85.csv')
aapc <- aapc %>% select(1, 3, 6:8) %>% group_by(X.name_code.)%>% slice(which.max(X.AAPC.Index.))
colnames(aapc) <- c('name_code', 'aapc_index', 'aapc', 'aapc_lci', 'aapc_uci')

aapc_country <- left_join(CI5plus, aapc[, c(1, 3:5)], by = c('name_code'='name_code'))

aapc_country <- aapc_country %>% group_by(region, name) %>% arrange(region, name)

aapc_country <- aapc_country %>% filter(name!='United Kingdom, England and Wales'&name!='United Kingdom, Northern Ireland'&name!='United Kingdom, Scotland'&name!='Cyprus'&name!='Greenland')
aapc_country <- aapc_country %>% mutate(country_name=ifelse(name=='Republic of Korea', 'South Korea', ifelse(name=='Hong Kong SAR', 'Hong Kong, China', ifelse(name=='United States of America', 'United States', ifelse(name=='Russian Federation', 'Russia', name)))))

aapc_country <- aapc_country %>% mutate(trend_grp=ifelse(aapc_lci>=0, 'increase', ifelse(aapc_uci<=0, 'decrease', 'stable')))
trend_sum <- aapc_country %>% group_by(trend_grp) %>% summarise(n=n())
trend_sum <- aapc_country %>% group_by(region, trend_grp) %>% summarise(n=n())

####Figure####
p3 <- ggplot(aapc_country, aes(x = fct_rev(country_name), y = aapc, group = region)) + geom_bar(stat = 'identity', width = 0.75, fill = '#E41A1C') + coord_flip() + theme_bw() + facet_grid(region~., scales = 'free_y', space = 'free_y') + theme(strip.text.y = element_text(angle = 0)) + scale_x_discrete(name = '')  + scale_y_continuous(name = 'AAPC in the last 5 years', limits=c(-21, 30), breaks=c(-20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 30), labels=c('20', '15', '10', '5', "0", '5', "10", '15', "20", '25','30')) + geom_errorbar(aes(ymin=aapc_lci, ymax=aapc_uci), size=0.4, width=.25, position=position_dodge(0.9), colour = 'black') + theme(axis.text.y = element_text(hjust = 0))

library(ggpubr)
ggarrange(p1, p2, ncol = 2, labels = c("A", "B"))
ggarrange(p2, p3, ncol = 2, labels = c("A", "B"))

