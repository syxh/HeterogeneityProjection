
set.seed(98798)

pcumsum=function(inp,d)
{
  foo = c(rep(0,d),inp)
  outp = 0*inp
  j=1:length(inp) -1
  for(i in 1:d)outp = outp + foo[j+i]
  return(outp)
}

muf=function(dat,pars)
{
  foo0 = pars$const
  foo1 = (pars$a1*exp(-pars$b1*dat$doy))*pcumsum(dat$cases_community,pars$duration1)+
    (pars$a2*exp(-pars$b2*dat$doy))*pcumsum(dat$cases_imported,pars$duration2)+
    (pars$a3*exp(-pars$b3*dat$doy))*pcumsum(dat$cases_dorms,pars$duration3)

  output = foo1+foo0 #+foo2
  output[output<0]=0
  return(output)
}


logl = function(dat,pars)
{
  mu = muf(dat,pars)
  LL = sum(dpois(dat$icu,mu,log=TRUE))
  pars$LL = LL
  return(pars)
}

mh = function(oldp,newp,dat)
{
  reject = FALSE
  if(newp$duration1<1)reject=TRUE
  if(newp$duration1>25)reject=TRUE
  if(newp$duration2<1)reject=TRUE
  if(newp$duration2>25)reject=TRUE
  if(newp$duration3<1)reject=TRUE
  if(newp$duration3>25)reject=TRUE
  if(newp$a1>1)reject=TRUE
  if(newp$b1>1)reject=TRUE
  if(newp$a2>1)reject=TRUE
  if(newp$b2>1)reject=TRUE
  if(newp$a3>1)reject=TRUE
  if(newp$b3>1)reject=TRUE
  if(newp$a1<0)reject=TRUE
  if(newp$b1<0)reject=TRUE
  if(newp$a2<0)reject=TRUE
  if(newp$b2<0)reject=TRUE
  if(newp$a3<0)reject=TRUE
  if(newp$b3<0)reject=TRUE
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

pars = list(
  const=1,duration1=14,duration2=10, duration3=20,
  a1=0.5,a2=0.5,a3=0.1,b1=0.05,b2=0.05,b3=0.01)
pars = logl(dat,pars)

storage=list(
  const=rep(0,MCMCITS),
  duration1=rep(0,MCMCITS),
  duration2=rep(0,MCMCITS),
  duration3=rep(0,MCMCITS),
  a1=rep(0,MCMCITS),
  a2=rep(0,MCMCITS),
  a3=rep(0,MCMCITS),
  b1=rep(0,MCMCITS),
  b2=rep(0,MCMCITS),
  b3=rep(0,MCMCITS),
  LL=rep(0,MCMCITS)
)
SUBITS=10
for(iteration in 1:MCMCITS)
{
  if(iteration%%1000==0)cat(iteration,'in',MCMCITS,'d =',pars$duration,'LL:',round(pars$LL),'\n')
  for(subit in 1:SUBITS)
  {
    oldpars = pars; pars$duration1 = round(rnorm(1,pars$duration1,1)) ; pars = mh(oldpars,pars,dat)
    oldpars = pars; pars$duration2 = round(rnorm(1,pars$duration2,1)) ; pars = mh(oldpars,pars,dat)
    oldpars = pars; pars$duration3 = round(rnorm(1,pars$duration3,1)) ; pars = mh(oldpars,pars,dat)
    
    
    oldpars = pars; pars$const = rnorm(1,pars$const,0.1) ; pars = mh(oldpars,pars,dat)
    
    oldpars = pars; pars$a1 = rnorm(1,pars$a1,0.1) ; pars = mh(oldpars,pars,dat)
    oldpars = pars; pars$a2 = rnorm(1,pars$a2,0.1) ; pars = mh(oldpars,pars,dat)
    oldpars = pars; pars$a3 = rnorm(1,pars$a3,0.01) ; pars = mh(oldpars,pars,dat)
    oldpars = pars; pars$b1 = rnorm(1,pars$b1,0.1) ; pars = mh(oldpars,pars,dat)
    oldpars = pars; pars$b2 = rnorm(1,pars$b2,0.1) ; pars = mh(oldpars,pars,dat)
    oldpars = pars; pars$b3 = rnorm(1,pars$b3,0.01) ; pars = mh(oldpars,pars,dat)
  }
  storage$duration1[iteration] = pars$duration1
  storage$duration2[iteration] = pars$duration2
  storage$duration3[iteration] = pars$duration3
  storage$const[iteration] = pars$const
  storage$a1[iteration] = pars$a1
  storage$a2[iteration] = pars$a2  
  storage$a3[iteration] = pars$a3
  storage$b1[iteration] = pars$b1
  storage$b2[iteration] = pars$b2
  storage$b3[iteration] = pars$b3
  storage$LL[iteration] = pars$LL
}


storage = as.data.frame(storage)

plot(dat$doy,dat$icu)
lines(dat$doy,muf(dat,pars),col=2)

write.csv(storage,paste0('working/',dat$doy[length(dat$doy)],'/mcmc1_ICU.csv'),row.names = FALSE)
# plot(storage$LL)


rm(iteration,subit,SUBITS,logl,mh,oldpars,pars,pcumsum,muf)

