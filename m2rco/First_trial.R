library(rstan)
library(data.table)
library(lubridate)
library(gdata)
library(dplyr)
library(tidyr)
library(EnvStats)
library(scales)
library(stringr)
library(parallel)

# from process_data.R

# indir <- file where the data folder is
indir = '/Users/liuhanyang/Desktop/UROP-Stats/M2R/Carlifornia-Covid19/m2rco'
setwd(indir)

# Deaths are from
# https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/?utm_source=MailChimp&utm_campaign=census-covid2
infile.deaths <- file.path(indir, 'data','covid_deaths_usafacts.csv')

# County Population is from
# https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/?utm_source=MailChimp&utm_campaign=census-covid2
infile.pop <- file.path(indir,'data', 'covid_county_population_usafacts.csv')

#	read & select death data
dd <- as.data.table( read.csv(infile.deaths, stringsAsFactors=FALSE) ) 

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
CA_ifr<-ifr_by_state[ifr_by_state$code=='CA',]
BA_ifr<-CA_ifr
for(i in 1:(length(states)-1)){
  BA_ifr<-rbind(BA_ifr,CA_ifr)
}
CA_ifr<-BA_ifr
CA_ifr<-CA_ifr[c(2,3,4)]
CA_ifr<-cbind(CA_ifr,states,code)

#case https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
infile.case <- file.path(indir, 'data','covid_confirmed_usafacts.csv')
cc <- as.data.table( read.csv(infile.case, stringsAsFactors=FALSE) ) 
cc = cc[,-c('X')]
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


# density
# den<-read_csv(url("https://raw.githubusercontent.com/Joezzb/Joezzb/m2r/m2rco/pop_den.csv"))
den = read.csv(file.path(indir, 'data/pop_den.csv'))
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
ca_mob <- read.csv("data/Mobility_for_California.csv")
ca_mob$date<-as.Date(ca_mob$date, format = "%Y-%m-%d")
ca_mob<-ca_mob[,c(3,4,7:13)]
names(ca_mob) <- c( "state", "sub_region_1",
                    "date", "retail.recreation", "grocery.pharmacy", "parks", "transitstations",
                    "workplace", "residential")
ca_mob<-merge(ca_mob,statecode,by='sub_region_1')
ca_mob[, c(4:9)] <- ca_mob[, c(4:9)]/100
ca_mob[, c(4:8)] <- ca_mob[, c(4:8)] * -1

max_date <- max(ca_mob$date)
dd<-dd[which(dd$date <= max_date),]

######################################
# stan data
# complete function used to create missing dates for mobility data
# fill function used to impute missing mobility data since NA not allowed in stan data

M <- 58
P <- 3
P_partial_county <- 3
N0 <- 6
N <- rep(148,58)
N2 <- max(N)

deaths <- matrix(data=dd$daily_deaths, nrow = N2, ncol = M)
f <- matrix(CA_ifr$ifr, nrow = N2, ncol = M)
ca_mob$avg <- rowMeans(ca_mob[,c(4:6,8)])

x <- ca_mob %>%
  mutate(date = as.Date(date)) %>%
  complete(sub_region_1=statecode[,2]) %>%
  complete(date = seq.Date(min(dd$date), max(dd$date), by="day"), sub_region_1)

x <- subset(x, is.na(date)==FALSE)

x <- x %>%
  distinct() %>%
  fill(colnames(ca_mob[,3:11]), .direction = "updown")

colnames(x)[2] <- "state_name"
ca_mob1 <- read.csv("data/Overall_Mobility_California.csv")

ca_mob1$date<-as.Date(ca_mob1$date, format = "%Y-%m-%d")
ca_mob1<-ca_mob1[,c(3,4,7:13)]
names(ca_mob1) <- c( "state", "sub_region_1",
                     "date", "retail.recreation", "grocery.pharmacy", "parks", "transitstations",
                     "workplace", "residential")

ca_mob1[, c(4:9)] <- ca_mob1[, c(4:9)]/100
ca_mob1[, c(4:8)] <- ca_mob1[, c(4:8)] * -1
ca_mob1$avg <- rowMeans(ca_mob1[,c(4:6,8)])
ca_mob1 <- ca_mob1 %>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(min(dd$date), max(dd$date), by="day"))

X1 <- ca_mob1[,c(10,7,9)]
X1 <- X1 %>%
  fill(colnames(X1), .direction = "up")

X1 <- as.matrix(X1)
X_partial <- list()

for (i in 1:58) {
  s <- subset(x, state_name==states[i])
  X_partial[[i]] <- as.matrix(s[, c(11,7,9)], ncol=3)
}

X_partial_county <- as.matrix(X_partial)

EpidemicStart <- rep(31,58)

pop <- unique(dd$pop_count)

W <- 22

x <- rep(1,148)
for (i in 1:22) {
  for (n in 1:7) {
    x[n+7*(i-1)] <- i
  }
}
x <- x[1:148]

week_index <- matrix(rep(x,58), nrow=58, ncol=148, byrow = TRUE)

load("stan_data.RData")

SI <- stan_data[["SI"]]
data <- c("M", "P", "P_partial_county", "N0", "N", "N2", "deaths", "f", "X1", "X_partial_county", "EpidemicStart", "pop", "W", "week_index", "SI")
# options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

mod <- stan_model("test.stan")
fit <- sampling(mod, data=data, iter=100, warmup=50, chains = 4,thin=1,control = list(adapt_delta = 0.95, max_treedepth = 10))

data = list('M' = M,
            'P' = P,
            'P_partial_county' = P_partial_county,
            'N0' = N0,
            'N' = N,
            'N2' = N2,
            'deaths' = deaths,
            'f' = f,
            'X1' = X1,
            "X_partial_county" = X_partial_county,
            "EpidemicStart" = EpidemicStart, 
            "pop" = pop, 
            "W" = W, 
            "week_index" = week_index, 
            "SI" = SI)

fit1 = stan(file = 'test.stan', data=data, iter=100, warmup=50, chains = 4,thin=1,control = list(adapt_delta = 0.95, max_treedepth = 10))
