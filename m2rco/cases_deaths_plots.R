########### Cases/deaths plots ##########

# indir <- '/Carlifornia-Covid19/m2rco'
setwd(indir)

source("usa/code/plotting/make-plots.r")
source('usa/code/process-covariates.r')

load("processed_data.RData")
load("death_data.RData")

#load stanout fit
filename <- "C:/Users/Vidoushee/Desktop/Covid USA/fit2.RDS"
fit <- readRDS(filename)

grouping<-death_data[(death_data$date==max_date),]
grouping<-grouping[c(1,10,2)]
colnames(grouping)<-c('state','groupings','state_name')

states<-unique(death_data$state_name)
code <- unique(death_data$code)
statecode <- cbind(code, states)

total_deaths <- rep(0,58)

for(i in 1:58) {
  s <- subset(death_data, state_name==states[i])
  total_deaths[i] <- max(s$cumulative_deaths)
}

# choose minimum deaths
sig <- states[which(total_deaths>400)]
sigcode <- subset(statecode, states %in% sig)

states <- sigcode[,1]

dates <- processed_data$dates
reported_deaths <- processed_data$reported_deaths
reported_cases <- processed_data$reported_cases

out <- rstan::extract(fit)
estimated_cases_raw <- out$prediction
estimated_deaths_raw <- out$E_deaths
#estimated_deaths_cf <- out$E_deaths0

JOBID = Sys.getenv("PBS_JOBID")

make_plots_all(filename, 
               last_date_data = max(dates[[1]]),
               ext = ".pdf", groupings = grouping)
