library(rstan)
library(data.table)
library(lubridate)
library(gdata)
library(dplyr)
library(tidyr)
library(EnvStats)
library(scales)
library(stringr)
library(abind)
library(scales)
library(zoo)
library(matrixStats)
library(optparse)


indir <- './Carlifornia-Covid19/m2rco'
setwd(indir)

source('usa/code/utils/read-data-usa.r')
source('usa/code/process-covariates.r')

#GFNAME_county_population <<-'covid_county_population_usafacts.csv'
#GFNAME_county_death <<- 'covid_deaths_usafacts.csv'

# Deaths are from
# https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/?utm_source=MailChimp&utm_campaign=census-covid2
infile.deaths <- file.path(indir,'data/covid_deaths_usafacts.csv')

# County Population is from
# https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/?utm_source=MailChimp&utm_campaign=census-covid2
infile.pop <- file.path(indir,'data/covid_county_population_usafacts.csv')

#	read & select death data
dd <- as.data.table( read.csv(infile.deaths, stringsAsFactors=FALSE) ) 
colnames(dd)[1] <- "countyFIPS"
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
  dates <- DATE[ 2:(length(CDEATHS)) ]
  list(DATE=dates, DEATHS=deaths)
}, by='COUNTYFIPS']
dd <- merge(dd, tmp, by=c('COUNTYFIPS','DATE'), all.x=TRUE)

#	read county pop & merge with death data 
dp <- as.data.table( read.csv(infile.pop, stringsAsFactors=FALSE) )
colnames(dp)[1] <- "countyFIPS"

setnames(dp, colnames(dp), gsub('\\.','_',toupper(colnames(dp))))
dd <- merge(dd, dp, by=c('STATE','COUNTYFIPS','COUNTY_NAME'))

#states<-county
code<-unique(dd$COUNTYFIPS)
states<-unique(dd$COUNTY_NAME)

statecode<-cbind(code,states)
colnames(statecode)<-c('code','sub_region_1')


#ifr county data
ifr_by_state <- readRDS('data/weighted_ifr_states.RDS')
CA_ifr<-ifr_by_state[ifr_by_state$code=='CA',]
BA_ifr<-CA_ifr
for(i in 1:(length(states)-1)){
  BA_ifr<-rbind(BA_ifr,CA_ifr)
}
CA_ifr<-BA_ifr
CA_ifr<-CA_ifr[c(2,3,4)]
CA_ifr<-cbind(CA_ifr,states,code)





#case https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
infile.case <- file.path(indir,'data','covid_confirmed_usafacts.csv')
cc <- as.data.table( read.csv(infile.case, stringsAsFactors=FALSE) )
cc = cc[,-c('X')]
colnames(cc)[1] <- "countyFIPS"
cc <- melt(cc, id.vars=c('countyFIPS','County.Name','State','stateFIPS'), variable.name='DATE', value.name='CASE')
setnames(cc, colnames(cc), gsub('\\.','_',toupper(colnames(cc))))
cc <- subset(cc, STATE=='CA')
cc <- subset(cc, COUNTYFIPS!=0 & !grepl('Cruise',COUNTY_NAME))
cc[, DATE:= gsub('^X','',DATE)]
cc[, DATE:= as.Date(DATE, format='%m.%d.%y')]

tmpc <- cc[, {
  cases <- CASE[ 2:length(CASE)]	- CASE[ 1:(length(CASE)-1)]
  dates <- DATE[ 2:(length(CASE)) ]
  list(DATE=dates, DAILY_CASE=cases)
}, by='COUNTYFIPS']
cc <- merge(cc, tmpc, by=c('COUNTYFIPS','DATE'), all.x=TRUE)


if (max(dd$DATE)<max(cc$DATE)){
  cc<-cc[cc$DATE<=max(dd$DATE),]  
}
dd <- merge(dd, cc, by=c('STATE','COUNTYFIPS','COUNTY_NAME','DATE','STATEFIPS'))
dd$region_census_sub_revised<-'Pacific'

#density
den<-read.csv(file.path(indir,'data/pop_den.csv'))
den<-den[,c(1,4)]
colnames(den)<-c('COUNTYFIPS','pop_density')
dd<-merge(dd,den,by=c('COUNTYFIPS'),all.x=TRUE)

dd<-dd[,c(1,3:12)]
setnames(dd,colnames(dd),c('code','state_name','date','region_code','cumulative_deaths',
                           'daily_deaths','pop_count','cumulative_cases','daily_cases',
                           'region_census_sub_revised','pop_density'))



# choose minimum date

mindate<-as.Date('2020-02-01',format='%Y-%m-%d')
dd<-dd[dd$date>=mindate]

#mobility
ca_mob<-read.csv(file.path(indir,'data/Mobility_for_California.csv'), stringsAsFactors = FALSE)
ca_mob$date<- as.Date(ca_mob$date, format = '%Y-%m-%d')
ca_mob<-ca_mob[,c(1,4,7:13)]
names(ca_mob) <- c( "country_region", "sub_region_1",
                            "date", "retail.recreation", "grocery.pharmacy", "parks", "transitstations",
                            "workplace", "residential")
#ca_mob$sub_region_2<-''
#ca_mob$country_region_code<-'US'
ca_mob<-merge(ca_mob,statecode,by=c('sub_region_1'),all.x=TRUE)
ca_mob[, c(4:9)] <- ca_mob[, c(4:9)]/100
ca_mob[, c(4:8)] <- ca_mob[, c(4:8)] * -1

max_date <- max(ca_mob$date)
dd<-dd[which(dd$date <= max_date),]

min_date <- min(ca_mob$date)
dd <- dd[which(dd$date >= min_date),]

#zero death
zerod<-dd[dd$date==max_date]
zerod<-zerod[zerod$cumulative_deaths<5]
zerodeath<-unique(zerod$code)
code<-setdiff(code,zerodeath)
death_data <- as.data.frame(dd)

#as character
death_data$code<-as.character(death_data$code)
code<-as.character(code)
ca_mob$code<-as.character(ca_mob$code)
CA_ifr$code<-as.character(CA_ifr$code)

# read interventions
interventions <- readRDS('data/covariates.RDS')
# read interventions lifted date
interventions_lifted <- readRDS('data/covariates_ended.RDS')
# Number of days to forecast
forecast <- 0

num_days_sim <- (max(death_data$date) - min(death_data$date) + 1 + forecast)[[1]]

args = c('base-usa',
         '~ -1 + averageMobility + I(transit * transit_use) + residential',
         '~ 1 +  averageMobility',
         '~ -1 + I(transit * transit_use)'
)

formula = as.formula(args[2])
formula_partial_regional = as.formula(args[3])
formula_partial_state = as.formula(args[4])
StanModel = args[1]

source('usa/code/process-covariates.r')

processed_data <- process_covariates(states = sigcode[,1], 
                                     mobility = ca_mob,
                                     death_data = death_data , 
                                     ifr_by_state = CA_ifr, 
                                     num_days_sim = num_days_sim, 
                                     interventions = interventions, 
                                     interventions_lifted = interventions_lifted,
                                     formula = formula, formula_partial_regional = formula_partial_regional,
                                     formula_partial_state = formula_partial_state)

stan_data <- processed_data$stan_data

dates <- processed_data$dates
reported_deaths <- processed_data$reported_deaths
reported_cases <- processed_data$reported_cases
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
m <- stan_model(paste0('usa/code/stan-models/',StanModel,'.stan'))
JOBID = Sys.getenv("PBS_JOBID")

fit = sampling(m,data=stan_data,iter=100,warmup=50,chains=4,thin=1,control = list(adapt_delta = 0.95, max_treedepth = 10))

#intervention for county
ca_int<-interventions[interventions$StatePostal=='CA',]
ba_int<-ca_int
for(i in 1:(length(code)-1)){
  ba_int<-rbind(ba_int,ca_int)
}
ca_int<-ba_int
ca_int$StatePostal<-code
ca_mob<-ca_mob[ca_mob$code %in% code,]

covariate_data = list(ca_int, ca_mob)

out <- rstan::extract(fit)
estimated_cases_raw <- out$prediction
estimated_deaths_raw <- out$E_deaths
estimated_deaths_cf <- out$E_deaths0

#
states<-code
save(fit, dates, reported_cases, reported_deaths, states,
     estimated_cases_raw, estimated_deaths_raw, estimated_deaths_cf,
     formula, formula_partial_regional,formula_partial_state, stan_data,covariate_data, JOBID,
     file=paste0('usa/results/',StanModel,'-',JOBID,'-stanfit.Rdata'))


# Makes three panel plots and rt plots for data
source("usa/code/plotting/make-plots.r")
grouping<-death_data[(death_data$date==max_date),]
grouping<-grouping[c(1,10,2)]
colnames(grouping)<-c('state','groupings','state_name')

make_plots_all(paste0('usa/results/', StanModel, '-', JOBID, '-stanfit.Rdata'), 
               last_date_data = max(dates[[1]]),
               ext = ".pdf",groupings=grouping)

