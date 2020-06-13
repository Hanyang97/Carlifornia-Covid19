require(data.table)
require(ggplot2)

indir <- 'F:/m2r/m2rcovid/'

# Deaths are from
# https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/?utm_source=MailChimp&utm_campaign=census-covid2
infile.deaths <- file.path(indir,'data','covid_deaths_usafacts.csv')

# County Population is from
# https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/?utm_source=MailChimp&utm_campaign=census-covid2
infile.pop <- file.path(indir,'data','covid_county_population_usafacts.csv')

# Mobility data
# https://www.unacast.com/covid19/social-distancing-scoreboard?view=county&fips=06115

# Google mobility reports
# https://www.google.com/covid19/mobility/
# python extractor https://github.com/datasciencecampus/google-mobility-reports-data


#	read & select death data
dd <- as.data.table( read.csv(infile.deaths, stringsAsFactors=FALSE) ) 
setnames(dd,"﻿countyFIPS",'countyFIPS')
#luanma

dd <- melt(dd, id.vars=c('countyFIPS','County.Name','State','stateFIPS'), variable.name='DATE', value.name='CDEATHS')
setnames(dd, colnames(dd), gsub('\\.','_',toupper(colnames(dd))))
dd <- subset(dd, STATE=='CA')
dd <- subset(dd, COUNTYFIPS!=0 & !grepl('Cruise',COUNTY_NAME))
dd[, DATE:= gsub('^X','',DATE)]
dd[, DATE:= as.Date(DATE, format='%m.%d.%y')]

#	calculate deaths per day and add 
tmp <- dd[, {
		deaths <- CDEATHS[ 2:length(CDEATHS)]	- CDEATHS[ 1:(length(CDEATHS)-1)]
		dates <- DATE[ 1:(length(CDEATHS)-1) ]
		list(DATE=dates, DEATHS=deaths)
		}, by='COUNTYFIPS']
dd <- merge(dd, tmp, by=c('COUNTYFIPS','DATE'), all.x=TRUE)

#	read county pop & merge with death data 
dp <- as.data.table( read.csv(infile.pop, stringsAsFactors=FALSE) )
setnames(dp,"﻿countyFIPS",'countyFIPS')
#luanma
setnames(dp, colnames(dp), gsub('\\.','_',toupper(colnames(dp))))
dd <- merge(dd, dp, by=c('STATE','COUNTYFIPS','COUNTY_NAME'))
dd[, CDEATHS_RATE:= CDEATHS/POPULATION]	

#	How many counties
dd[, length(unique(COUNTY_NAME))]
#	58

#	length of time series
dd[, length(unique(DATE))]
#	124 days

#	plot time series: deaths (dev mode)
tmp <- subset(dd, COUNTYFIPS==6001)
ggplot(tmp, aes(x=DATE, y=CDEATHS)) + 
		geom_line() +
		theme_bw() +
		labs(x='Date', y='COVID19 cumulated deaths (USAFacts.org)')
#	plot time series: deaths (all counties)
ggplot(dd, aes(x=DATE, y=CDEATHS)) + 
		geom_line() +
		theme_bw() +
		labs(x='Date', y='COVID19 cumulated deaths (USAFacts.org)') +
		facet_wrap(~COUNTY_NAME, ncol=5, scales='free')
ggsave(file=file.path(indir,'data','CA_cdeaths.pdf'), w=15,h=15)

#	plot death rate per capita on 2020-05-24
tmp <- subset(dd, DATE=='2020-05-24')
ggplot(tmp, aes(y=CDEATHS_RATE, x=COUNTY_NAME, fill=COUNTY_NAME)) +
		geom_bar(stat='identity') +
		theme_bw() +
		coord_flip() +
		labs(x='CA counties', y='COVID19 death rate per capita (USAFacts.org)') +
		guides(fill=FALSE)
ggsave(file=file.path(indir,'data','CA_cdeathsrate_200524.pdf'), w=8,h=15)

#	plot daily deaths and cumulative deaths for LA
tmp <- subset(dd, grepl('Angeles',COUNTY_NAME))
tmp <- melt(tmp, id.vars=c('COUNTY_NAME','DATE'), measure.vars=c('DEATHS','CDEATHS'))
tmp[, VAR:= 'COVID19 deaths']
set(tmp, tmp[, which(grepl('CDEATHS',variable))], 'VAR', 'COVID19 cumulated deaths')
ggplot(tmp, aes(x=DATE, y=value, colour=VAR)) +
		geom_line() +
		geom_point() +
		facet_grid(VAR~COUNTY_NAME, scales='free') +
		theme_bw() +
		labs(x='Date',y='') +
		guides(colour=FALSE)
ggsave(file=file.path(indir,'data','CA_deaths_cdeaths_LA.pdf'), w=15,h=8)

