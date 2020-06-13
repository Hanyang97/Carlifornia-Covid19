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

source('usa/code/utils/read-data-usa.r')
source('usa/code/utils/process-covariates.r')

death_data <- read_death_data(source = "jhu", smooth = FALSE)
CA<-death_data[death_data$code=='CA',]
#CA data

ifr_by_state <- read_ifr_data()
CA_ifr<-ifr_by_state[ifr_by_state$code=='CA',]
##ifr

mobility <- read_google_mobility()
CA_mobility<-mobility[mobility$code=='CA',]
CA_mobility<-na.omit(CA_mobility)
# Read google mobility

google_pred <- read.csv('usa/data/google-mobility-forecast.csv', stringsAsFactors = FALSE)
google_pred$date <- as.Date(google_pred$date, format = '%Y-%m-%d') 
google_pred$sub_region_2 <- ""
google_pred$country_region <- "United States"
google_pred$country_region_code <- "US"
colnames(google_pred)[colnames(google_pred) == 'state'] <- 'sub_region_1'
CA_pred<-google_pred[google_pred$sub_region_1=='California',]
##CA_pred
if (max(CA_pred$date) > max(CA_mobility$date)){
  CA_pred <- CA_pred[CA_pred$date > max(mobility$date),]
  CA_pred$code = "CA"
  mobility <- rbind(as.data.frame(CA_mobility),as.data.frame(CA_pred[,colnames(CA_mobility)]))
}
max_date <- max(mobility$date)
CA <- CA[which(CA$date <= max_date),]

# read interventions
interventions <- readRDS('usa/data/covariates.RDS')
# read interventions lifted date
interventions_lifted <- readRDS('usa/data/covariates_ended.RDS')

CA_interventions<-interventions[interventions$StatePostal=='CA',]
CA_interventions_lifted<-interventions_lifted[interventions_lifted$StatePostal=='CA',]
# Number of days to forecast
forecast <- 0

#formula
args = c('base-usa',
         '~ -1 + averageMobility + I(transit * transit_use) + residential',
         '~ 1 +  averageMobility',
         '~ -1 + I(transit * transit_use)'
)
StanModel<-args[1]
num_days_sim <- (max(CA$date) - min(CA$date) + 1 + forecast)[[1]]
formula = as.formula(args[2])
formula_partial_regional = as.formula(args[3])
formula_partial_state = as.formula(args[4])
#process
states=c('CA')
processed_data <- process_covariates(states = states ,
                                     mobility = mobility,
                                     death_data = CA , 
                                     ifr_by_state = CA_ifr, 
                                     num_days_sim = num_days_sim, 
                                     interventions = CA_interventions, 
                                     interventions_lifted = CA_interventions_lifted,
                                     formula = formula, formula_partial_regional = formula_partial_regional,
                                     formula_partial_state = formula_partial_state)
stan_data <- processed_data$stan_data

dates <- processed_data$dates
reported_deaths <- processed_data$reported_deaths
reported_cases <- processed_data$reported_cases
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
m <- stan_model(paste0('usa/code/stan-models/',StanModel,'.stan'))
JOBID = as.character(abs(round(rnorm(1) * 1000000)))
#fit model
fit = sampling(m,data=stan_data,iter=100,warmup=50,chains=4,thin=1,control = list(adapt_delta = 0.95, max_treedepth = 10))

covariate_data = list(interventions, mobility)

out <- rstan::extract(fit)
estimated_cases_raw <- out$prediction
estimated_deaths_raw <- out$E_deaths
estimated_deaths_cf <- out$E_deaths0

save(fit, dates, reported_cases, reported_deaths, states,
     estimated_cases_raw, estimated_deaths_raw, estimated_deaths_cf,
     formula, formula_partial_regional,formula_partial_state, stan_data,covariate_data, JOBID,
     file=paste0('ca/','ca',JOBID,'-stanfit.Rdata'))

#plot
source("usa/code/plotting/make-plots.r")
make_plots_all(paste0('ca/','ca',JOBID,'-stanfit.Rdata'), 
               last_date_data = max(dates[[1]]), 
               ext = ".pdf")
source("usa/code/plotting/infectiousness-plots.r")
source("usa/code/utils/calculate-infectiousness.r")
out <- rstan::extract(fit)
calculate_infectiousness(states,out,JOBID)

plot_infectiousness_regions(JOBID = JOBID, StanModel= StanModel, ext = ".pdf",
                            last_date_data = max(dates[[1]]),
                            individual = TRUE)

