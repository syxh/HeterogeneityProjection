
set.seed(5425)


mu_local = function(dat,pars)
{
  foo0=pars$a00
  alpha = exp(pars$a0 
              + pars$a1*(mobility$residential_percent_change_from_baseline[1:nrow(dat)]/100)
              + pars$a2*(mobility$transit_stations_percent_change_from_baseline[1:nrow(dat)]/100)
              + pars$a3*(mobility$grocery_and_pharmacy_percent_change_from_baseline[1:nrow(dat)]/100)
              + pars$a4*(mobility$parks_percent_change_from_baseline[1:nrow(dat)]/100)
              + pars$a5*(mobility$workplaces_percent_change_from_baseline[1:nrow(dat)]/100)
              + pars$a6*(mobility$retail_and_recreation_percent_change_from_baseline[1:nrow(dat)]/100)
              )
  
  foo2=dcumsum(alpha*dat$cases_community,pars$duration2) # day 100 is 9 Apr, hotel based SHN 
  cb = as.numeric((dat$doy>=98)*(dat$doy<=153))
  foo2[cb==1] = foo2[cb==1]*pars$cbe

  output = foo0+foo2
  return(output)
}


pcumsum=function(inp,d)
{
  foo = c(rep(0,d),inp)
  outp = 0*inp
  j=1:length(inp) -1
  for(i in 1:d)outp = outp + foo[j+i]
  return(outp)
}

dcumsum=function(inp,d)
{
  foo = c(rep(0,d),inp)
  outp = foo[1:length(inp)]#0*inp
  return(outp)
}


logl = function(dat,pars)
{
  LL = sum(dpois(dat$cases_community,mu_local(dat,pars),log=TRUE))
  pars$LL = LL
  return(pars)
}

mh = function(oldp,newp,dat)
{
  reject = FALSE
  if(newp$a00<0)reject=TRUE
  if(newp$cbe<0)reject=TRUE
  if(newp$duration2<1)reject=TRUE
  if(newp$duration2>14)reject=TRUE
  
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
pars = list(a00=1,a0=0.1,a1=0.2,a2=0.2,a3=0.2, a4=0.2,a5=0.2, a6=0.2,cbe=1,duration2=2)#,a3=0.2)
pars = logl(dat,pars)


storage=list(
  a00=rep(0,MCMCITS),
  a0=rep(0,MCMCITS),
  a1=rep(0,MCMCITS),
  a2=rep(0,MCMCITS),
  a3=rep(0,MCMCITS),
  a4=rep(0,MCMCITS),
  a5=rep(0,MCMCITS),
  a6=rep(0,MCMCITS),
  cbe=rep(0,MCMCITS),
  duration2=rep(0,MCMCITS),
  LL=rep(0,MCMCITS)
)
SUBITS=10
for(iteration in 1:MCMCITS)
{
  if(iteration%%1000==0)cat(iteration,'in',MCMCITS,'\n')
  for(subit in 1:SUBITS)
  {
    oldpars = pars;
    pars$a00 = rnorm(1,pars$a00,0.2)# ; pars = mh(oldpars,pars,dat)
    pars$a0 = rnorm(1,pars$a0,0.1)# ; pars = mh(oldpars,pars,dat)
    pars$a1 = rnorm(1,pars$a1,0.2)# ; pars = mh(oldpars,pars,dat)
    pars$a2 = rnorm(1,pars$a2,0.2)# ; pars = mh(oldpars,pars,dat)
    pars$a3 = rnorm(1,pars$a3,0.2)# ; pars = mh(oldpars,pars,dat)
    pars$a4 = rnorm(1,pars$a4,0.2)# ; pars = mh(oldpars,pars,dat)
    pars$a5 = rnorm(1,pars$a5,0.2)# ; pars = mh(oldpars,pars,dat)
    pars$a6 = rnorm(1,pars$a6,0.2)# ; pars = mh(oldpars,pars,dat)
    pars$cbe = rnorm(1,pars$cbe,0.1)# ; pars = mh(oldpars,pars,dat)
    pars$duration2 = round(rnorm(1,pars$duration2,1)) ; 
    pars = mh(oldpars,pars,dat)
  }
  storage$a00[iteration] = pars$a00
  storage$a0[iteration] = pars$a0
  storage$a1[iteration] = pars$a1
  storage$a2[iteration] = pars$a2
  storage$a3[iteration] = pars$a3
  storage$a4[iteration] = pars$a4
  storage$a5[iteration] = pars$a5
  storage$a6[iteration] = pars$a6
  storage$cbe[iteration] = pars$cbe
  storage$duration2[iteration] = pars$duration2
  storage$LL[iteration] = pars$LL
}

storage = as.data.frame(storage)

write.csv(storage,paste0('working/',dat$doy[length(dat$doy)],'/mcmc3_cases_local.csv'),row.names = FALSE)



# plot(dat$doy,dat$cases_community)
# lines(dat$doy,mu_local(dat,pars),col=2)


rm(iteration,subit,SUBITS,logl,mh,oldpars,pars,storage,pcumsum,dcumsum,mu_local)
