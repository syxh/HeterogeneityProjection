
set.seed(98798)
dat$cases = dat$cases_community+dat$cases_dorms+dat$cases_imported



pcumsum=function(inp,d)
{
  foo = c(rep(0,d),inp)
  outp = 0*inp
  j=1:length(inp) -1
  for(i in 1:d)outp = outp + foo[j+i]
  return(outp)
}


head(dat)

#plot(dat$doy,dat$icu)
#lines(dat$doy,pcumsum(dat$it,17))


muf=function(dat,pars)
{
  return((pars$risk/pars$duration)*pcumsum(dat$icu,pars$duration))
}


logl = function(dat,pars)
{
  LL = sum(dpois(dat$deaths,muf(dat,pars),log=TRUE))
  pars$LL = LL
  return(pars)
}

mh = function(oldp,newp,dat)
{
  reject = FALSE
  if(newp$duration<1)reject=TRUE
  if(newp$duration>10)reject=TRUE
  if(newp$risk<0)reject=TRUE
  
  if(!reject)
  {
    newp = logl(dat,newp)
    la = newp$LL - oldp$LL
    lu = -rexp(1)
    if(lu>la)reject=TRUE
  }
  if(reject)return(oldp)
  return(newp)
}

pars = list(duration=1,risk=0.018)
pars = logl(dat,pars)


storage=list(
  duration=rep(0,MCMCITS),
  risk=rep(0,MCMCITS),
  LL=rep(0,MCMCITS)
)
SUBITS=10
for(iteration in 1:MCMCITS)
{
  if(iteration%%1000==0)cat(iteration,'in',MCMCITS,'d =',pars$duration,'LL:',round(pars$LL),'\n')
  for(subit in 1:SUBITS)
  {
    oldpars = pars; pars$risk = rnorm(1,pars$risk,0.1) ; pars = mh(oldpars,pars,dat)
  }
  storage$duration[iteration] = pars$duration
  storage$risk[iteration] = pars$risk
  storage$LL[iteration] = pars$LL
}


storage = as.data.frame(storage)

plot(dat$doy,(dat$deaths))
lines(dat$doy,(muf(dat,pars)),col=2)

write.csv(storage,paste0('working/',dat$doy[length(dat$doy)],'/mcmc5_deaths.csv'),row.names = FALSE)


rm(iteration,subit,SUBITS,logl,mh,oldpars,pars,storage,pcumsum,muf)
# 
