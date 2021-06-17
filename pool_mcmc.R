mcmc=list()
mcmc$m1 = read.csv(paste0('working/',dat$doy[length(dat$doy)],'/mcmc1_ICU.csv'))

mcmc$m2 = read.csv(paste0('working/',dat$doy[length(dat$doy)],'/mcmc2_cases_imported.csv'))
mcmc$m3 = read.csv(paste0('working/',dat$doy[length(dat$doy)],'/mcmc3_cases_local.csv'))

mcmc$m4 = read.csv(paste0('working/',dat$doy[length(dat$doy)],'/mcmc4_cases_foreign.csv'))
mcmc$m5 = read.csv(paste0('working/',dat$doy[length(dat$doy)],'/mcmc5_deaths.csv'))
save('mcmc',file=paste0('working/',dat$doy[length(dat$doy)],'/mcmc.rdata'))

rm(mcmc)
