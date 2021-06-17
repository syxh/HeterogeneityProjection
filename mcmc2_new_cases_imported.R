
set.seed(5425)


pcumsum=function(inp,d)
{
  foo = c(rep(0,d),inp)
  outp = 0*inp
  j=1:length(inp) -1
  for(i in 1:d)outp = outp + foo[j+i]
  return(outp)
}



muf = function(dat,pars)
{
  mu = pars$a1+pars$a2*dnorm(dat$doy,pars$a3,pars$a4) +
    pars$a5*(dat$doy>=234) +
    (pars$a5-pars$a1)*as.numeric(dat$doy>182 & dat$doy<234)*c(rep(0,length(which(dat$doy<=182))),
                                                              (dat$doy[(dat$doy>182)&(dat$doy<234)]-182)/(234-182))
  return(mu)
}


logl = function(dat,pars)
{
  LL = sum(dpois(dat$cases_imported,muf(dat,pars),log=TRUE))
  pars$LL = LL
  return(pars)
}

mh = function(oldp,newp,dat)
{
  reject = FALSE
  if(newp$a1<0)reject=TRUE
  if(newp$a2<0)reject=TRUE
  if(newp$a3<0)reject=TRUE
  if(newp$a4<0)reject=TRUE
  if(newp$a5<0)reject=TRUE
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

pars = list(a1=0.8,a2=500,a3=78,a4=6, a5=0.9)
pars = logl(dat,pars)


storage=list(
  a1=rep(0,MCMCITS),
  a2=rep(0,MCMCITS),
  a3=rep(0,MCMCITS),
  a4=rep(0,MCMCITS),
  a5=rep(0,MCMCITS),
  LL=rep(0,MCMCITS)
)
SUBITS=10
for(iteration in 1:MCMCITS)
{
  if(iteration%%1000==0)cat(iteration,'in',MCMCITS,'\n')
  for(subit in 1:SUBITS)
  {
    oldpars = pars; pars$a1 = rnorm(1,pars$a1,0.1) ; pars = mh(oldpars,pars,dat)
    oldpars = pars; pars$a2 = rnorm(1,pars$a2,20) ; pars = mh(oldpars,pars,dat)
    oldpars = pars; pars$a3 = rnorm(1,pars$a3,0.5) ; pars = mh(oldpars,pars,dat)
    oldpars = pars; pars$a4 = rnorm(1,pars$a4,0.25) ; pars = mh(oldpars,pars,dat)
    oldpars = pars; pars$a5 = rnorm(1,pars$a5,0.1) ; pars = mh(oldpars,pars,dat)
  }
  storage$a1[iteration] = pars$a1
  storage$a2[iteration] = pars$a2
  storage$a3[iteration] = pars$a3
  storage$a4[iteration] = pars$a4
  storage$a5[iteration] = pars$a5
  storage$LL[iteration] = pars$LL
}


storage = as.data.frame(storage)

write.csv(storage,paste0('working/',dat$doy[length(dat$doy)],'/mcmc2_cases_imported.csv'),row.names = FALSE)
# 
# plot(muf(dat,pars),type='l')
# points(dat$cases_imported,col=2)

rm(iteration,subit,SUBITS,logl,mh,oldpars,pars,storage,pcumsum,muf)

