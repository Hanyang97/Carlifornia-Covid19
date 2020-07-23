library(rstan)
library(data.table)
library(bayesplot)
library(ggplot2)

# set indir as directory to Carlifornia-Covid19 folder
indir <- "/Users/liuhanyang/Desktop/UROP-Stats/M2R/Carlifornia-Covid19"
# create trace_plots folder under data folder
# we will save all outputs in trace_plots folder
dir.create(file.path(indir, 'm2rco/data/trace_plots'))

# load input for this script
`-stanout-fit` <- readRDS(file.path(indir, 'm2rco/data', "-stanout-fit.RDS"))
fit=`-stanout-fit`

# make table of parameters
tmp <- fit@par_dims
fit.pars <- data.table(name= names(tmp))


cat("\n ----------- calculate pars with small neff: start ----------- \n")
#
# extract neff 
summary.par = summary(fit)$summary
neff <- as.numeric(summary.par[, which(colnames(summary.par) == "n_eff")])
Rhat <- summary.par[, which(colnames(summary.par) == "Rhat")]

bound <- 500
pars.with.small.neff <- summary.par[which(neff < bound),]
saveRDS(pars.with.small.neff, file=paste0(indir, '/m2rco/data/trace_plots/', '-pars-with-small-neff.rds'))
cat("\n ----------- calculate pars with small neff: end ----------- \n")

cat("\n ----------- report sampler diagnostics: start ----------- \n")
sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
sampler_diagnostics <- data.table()
for (i in colnames(sampler_params[[1]])) {
  tmp <- data.table(t(sapply(sampler_params, function(x) quantile(x[, i],probs = c(0.025,0.5,0.975)))))
  tmp[, diagnostics:=i ]
  tmp[, chain:= seq_len(length(sampler_params))]
  sampler_diagnostics <- rbind(sampler_diagnostics, tmp)
}

saveRDS(sampler_diagnostics,file=paste0(indir, '/m2rco/data/trace_plots/', '-sampler_diagnostics.rds'))
cat("\n ----------- report sampler diagnostics: end ----------- \n")


cat("\n ----------- make trace plots: start ----------- \n")

#load function 
make_trace_plot <- function(fit, target.pars, indir, fig.type='.png')
{
  color_scheme_set("mix-blue-red")
  target.pars2 <- names(fit)[ grepl(paste(paste0('^',target.pars),collapse = '|'),names(fit)) ]
  if(length(target.pars2)==0 & any(grepl('\\[',target.pars)))
  {
    target.pars2 <- target.pars
  }	    
  p <- rstan::traceplot(fit, 
                        pars= target.pars2,  
                        inc_warmup=TRUE, 
                        ncol = 1)
  cat('\n write trace plot', paste0(indir, "-trace_plot",fig.type))
  ggsave(file=paste0(indir, "-trace_plot",fig.type),p,w=20, h=length(target.pars2)*3,limitsize = FALSE)
}

# make trace plots
tryCatch({
  tmp <- subset(fit.pars)
  for(x in tmp$name)
  {
    make_trace_plot(fit, x, paste0(indir, '/m2rco/data/trace_plots/',x))	
  }
  
  # tmp <- subset(fit.pars)
  # make_trace_plot(fit, tmp$name, paste0(indir, '/m2rco/data/trace_plots/'), '.png')		
})
cat("\n ----------- make trace plots: end ----------- \n")

