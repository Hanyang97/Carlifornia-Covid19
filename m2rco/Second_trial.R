library(rstan)
library(data.table)
library(lubridate)
library(gdata)
library(dplyr)
library(tidyr)
library(EnvStats)
library(scales)
library(stringr)
library(imputeTS)
library(readr)


# from process_data.R
# indir <- file directory\
# indir <- './Carlifornia-Covid19/m2rco'
# indir <- '/Users/or105/git/Carlifornia-Covid19/m2rco'
setwd(indir)



# Deaths are from
# https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/?utm_source=MailChimp&utm_campaign=census-covid2
infile.deaths <- file.path(indir, 'data','covid_deaths_usafacts.csv')


# County Population is from
# https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/?utm_source=MailChimp&utm_campaign=census-covid2
infile.pop <- file.path(indir,'data', 'covid_county_population_usafacts.csv')

# stan_data
load("stan_data.RData")


#	read & select death data
dd <- as.data.table( read.csv(infile.deaths, stringsAsFactors=FALSE) ) 
colnames(dd)[1] <- 'countyFIPS'
setnames(dd,"countyFIPS",'countyFIPS', skip_absent = TRUE)

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
colnames(dp)[1] <- 'countyFIPS'

#luanma
setnames(dp, colnames(dp), gsub('\\.','_',toupper(colnames(dp))))
dd <- merge(dd, dp, by=c('STATE','COUNTYFIPS','COUNTY_NAME'))

#states<-county

code<-unique(dd$COUNTYFIPS)
states<-unique(dd$COUNTY_NAME)
statecode<-cbind(code,states)
colnames(statecode)<-c('code','sub_region_1')

#ifr county data
ifr_by_state <- readRDS('data/weighted_ifr_states.RDS')
# CA_ifr<-ifr_by_state[ifr_by_state$code=='CA',]
# BA_ifr<-CA_ifr
# for(i in 1:(length(states)-1)){
#   BA_ifr<-rbind(BA_ifr,CA_ifr)
# }
# 
# CA_ifr<-BA_ifr
# CA_ifr<-CA_ifr[c(2,3,4)]
# CA_ifr<-cbind(CA_ifr,states,code)

IFR <- subset(ifr_by_state, code=='CA')$ifr

# various distributions required for modeling
mean1 = 5.1; cv1 = 0.86; # infection to onset
mean2 = 18.8; cv2 = 0.45 # onset to death
x1 = rgammaAlt(1e7,mean1,cv1) # infection-to-onset distribution
x2 = rgammaAlt(1e7,mean2,cv2) # onset-to-death distribution

ecdf.saved = ecdf(x1+x2)

# IFR is the overall probability of dying given infection
convolution = function(u) (IFR * ecdf.saved(u))

f = rep(0,N2) # f is the probability of dying on day i given infection
f[1] = (convolution(1.5) - convolution(0))
for(i in 2:N2) {
  f[i] = (convolution(i+.5) - convolution(i-.5)) 
}

#case https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
infile.case <- file.path(indir, 'data','covid_confirmed_usafacts.csv')
cc <- as.data.table( read.csv(infile.case, stringsAsFactors=FALSE) ) 
colnames(cc)[1] <- 'countyFIPS'
cc[,166] <- NULL
cc <- melt(cc, id.vars=c('countyFIPS','County.Name','State','stateFIPS'), variable.name='DATE', value.name='CASE')
setnames(cc, colnames(cc), gsub('\\.','_',toupper(colnames(cc))))
cc <- subset(cc, STATE=='CA')
cc <- subset(cc, COUNTYFIPS!=0 & !grepl('Cruise',COUNTY_NAME))
cc[, DATE:= gsub('^X','',DATE)]
cc[, DATE:= as.Date(DATE, format='%m.%d.%y')]

tmpc <- cc[, {
  cases <- CASE[ 2:length(CASE)]	- CASE[ 1:(length(CASE)-1)]
  dates <- DATE[ 1:(length(CASE)-1) ]
  list(DATE=dates, DAILY_CASE=cases)
}, by='COUNTYFIPS']

cc <- merge(cc, tmpc, by=c('COUNTYFIPS','DATE'), all.x=TRUE)
cc <- subset(cc, is.na(DATE)==FALSE)

if (max(dd$DATE)<max(cc$DATE)){
  cc<-cc[cc$DATE<=max(dd$DATE),]  
}

dd <- merge(dd, cc, by=c('STATE','COUNTYFIPS','COUNTY_NAME','DATE','STATEFIPS'))
dd$region_census_sub_revised<-'Pacific'

#density
den<-read_csv('data/pop_den.csv')
den<-den[,c(1,4)]
colnames(den)<-c('COUNTYFIPS','pop_density')
dd<-merge(dd,den,by=c('COUNTYFIPS'),all.x=TRUE)
dd<-dd[,c(1,3:12)]

setnames(dd,colnames(dd),c('code','state_name','date','region_code','cumulative_deaths',
                           
                           'daily_deaths','pop_count','cumulative_cases','daily_cases',
                           
                           'region_census_sub_revised','pop_density'))

mindate<-as.Date('2020-02-01',format='%Y-%m-%d')

dd<-dd[dd$date>=mindate]



#mobility
ca_mob <- read_csv("data/Mobility_for_California.csv")
ca_mob$date<- as.Date(ca_mob$date, format = '%Y-%m-%d')
ca_mob<-ca_mob[,c(3,4,7:13)]
names(ca_mob) <- c( "state", "sub_region_1",
                    "date", "retail.recreation", "grocery.pharmacy", "parks", "transitstations",
                    "workplace", "residential")

ca_mob<-merge(ca_mob,statecode,by='sub_region_1')
ca_mob[, c(4:9)] <- ca_mob[, c(4:9)]/100
ca_mob[, c(4:8)] <- ca_mob[, c(4:8)] * -1



max_date <- max(ca_mob$date)

dd$date = as.Date(dd$date, format = '%Y-%m-%d')

dd<-dd[which(dd$date <= max_date),]

ca_mob1 <- read.csv("data/Overall_Mobility_California.csv")

#ca_mob1$date<-as.Date(ca_mob1$date, format = "%Y/%m/%d")

ca_mob1<-ca_mob1[,c(3,4,7:13)]
names(ca_mob1) <- c( "state", "sub_region_1",
                     "date", "retail.recreation", "grocery.pharmacy", "parks", "transitstations",
                     "workplace", "residential")
ca_mob1[, c(4:9)] <- ca_mob1[, c(4:9)]/100
ca_mob1[, c(4:8)] <- ca_mob1[, c(4:8)] * -1
ca_mob1$avg <- rowMeans(ca_mob1[,c(4:6,8)])



######################################


total_deaths <- rep(0,58)

for(i in 1:58) {
  s <- subset(dd, state_name==states[i])
  total_deaths[i] <- max(s$cumulative_deaths)
}

# choose minimum deaths
sig <- states[which(total_deaths>10)]

dd1 <- subset(dd, state_name %in% sig)

# no. of counties
M = length(sig)

P = 3

P_partial_state = 3

N0 = 6

N <- rep(134, M)

N2 = 134

x2 <- subset(ca_mob, sub_region_1 %in% sig)

x2$grocery.pharmacy <- na_interpolation(x2$grocery.pharmacy, option = "linear")

x2$avg <- rowMeans(x2[,c(4:6,8)])

min_date <- min(x2$date)

dd1 <- subset(dd1, date>=min_date)

deaths <- matrix(data=dd1$daily_deaths, nrow = N2, ncol = M)

f <- matrix(f, nrow = N2, ncol = M)

X_partial <- list()

for (i in 1:M) {
  s <- subset(x2, sub_region_1==sig[i])
  X_partial[[i]] <- as.matrix(s[, c(11,7,9)], ncol=3)
}

X_partial_state <- as.matrix(X_partial)

X1 <- ca_mob1[,c(10,7,9)]

X <- as.matrix(X1)

EpidemicStart <- rep(31,M)

pop <- unique(dd1$pop_count)

SI <- stan_data[["SI"]][1:N2]

data1 <- c("M", "P", "P_partial_state", "N0", "N", "N2", "deaths", "f", "X", "X_partial_state", "EpidemicStart", "pop", "SI")

options(mc.cores = parallel::detectCores())

rstan_options(auto_write = TRUE)

mod1 <- stan_model("usa/code/stan-models/base-usa-simple.stan")

fit <- sampling(mod1, data=data1, iter=1000, chains=4, control=list(adapt_delta=0.9, max_treedepth=15))

# data = list('M' = M,
#             'P' = P,
#             'P_partial_state' = P_partial_state,
#             'N0' = N0,
#             'N' = N,
#             'N2' = N2,
#             'deaths' = deaths,
#             'f' = f,
#             'X' = X,
#             "X_partial_state" = X_partial_state,
#             "EpidemicStart" = EpidemicStart, 
#             "pop" = pop, 
#             "SI" = SI)
# 
# fit1 = stan(file = 'usa/code/stan-models/base-usa-simple.stan', data=data, iter=5000, warmup=2500, chains = 4,thin=1,control = list(adapt_delta = 0.99, max_treedepth = 10))
