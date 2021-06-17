set.seed(54235)
library(mvtnorm)
source('code/sir_functions.R')

BURNIN=1000

pars = initialise(NULL,dat$cases_dorms,NFWs)
pars$seir = SEIR(pars)
pars$LL = loglikelihood(pars,dat$cases_dorms)
  
  
z=rep(0,MCMCITS)
storage = list(beta=z,E0=z,
                 interventioneffect=z,
                 sig=z,mu=z,#b1=z,
                 p=z, threshold=z
                 )
rm(z)


SUBITS=10
for(iteration in (-BURNIN):MCMCITS){
   if(iteration%%1000==0)cat('Iteration ',iteration,' of ',MCMCITS,': ',pars$LL,' (R0 = ',
                              pars$beta/pars$mu,
                              ')\n',sep='') # if verbose #pars$beta/pars$mu,
  for(subit in 1:SUBITS)  {
    oldp = pars; 
    pars$beta=rnorm(1,pars$beta,0.2)#;pars$seir = SEIR(pars); pars = mh(oldp,pars,dat$ct3)#infection
    pars$E0=rnorm(1,pars$E0,0.1)#;pars$seir = SEIR(pars); pars = mh(oldp,pars,dat$ct3)#infection
    pars$interventioneffect = rnorm(1,pars$interventioneffect,0.2)
    pars$sig = rnorm(1,pars$sig,0.2)
    pars$mu = rnorm(1,pars$mu,0.01)
    pars$p = rnorm(1,pars$p,0.1)
    pars$threshold = rnorm(1,pars$threshold,50)
      
    pars$seir = SEIR(pars)
    pars = mh(oldp,pars,dat$cases_dorms)      
      
    }
    if(iteration>0)
    {
      storage$beta[iteration] = pars$beta
      storage$sig[iteration] = pars$sig
      storage$mu[iteration] = pars$mu
      storage$E0[iteration] = pars$E0
      storage$interventioneffect[iteration] = pars$interventioneffect
      storage$p[iteration] = pars$p
      storage$threshold[iteration] = pars$threshold

    }
  }
  
  
storage = as.data.frame(storage)
  
write.csv(storage,paste0('working/',dat$doy[length(dat$doy)],'/mcmc4_cases_foreign.csv'),row.names = FALSE)



 rm(oldp,pars,storage,iteration,subit,SUBITS,initialise,loglikelihood,mh,SEIR,delta)
