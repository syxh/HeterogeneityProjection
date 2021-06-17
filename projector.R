set.seed(123)

# mcmc --------------------------------------------------------------------
load(paste0('working/',dat$doy[length(dat$doy)],'/mcmc.rdata'))

##############################################################################
source('code/sir_functions.R')

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
  outp = foo[1:length(inp)]
  return(outp)
}

recumsum = function(inp,d)
{
  foo = c(inp[-c(1:d)],rep(0,d))
  outp = foo[1:length(inp)]
  return(outp)
}
muf=function(dats,pars,type='icu')
{
  if(type=='icu')
  {

    foo0 = pars$const
    foo1 = (pars$a1*exp(-pars$b1*dats$doy))*pcumsum(dats$cases_community,pars$duration1)+
      (pars$a2*exp(-pars$b2*dats$doy))*pcumsum(dats$cases_imported,pars$duration2)+
      (pars$a3*exp(-pars$b3*dats$doy))*pcumsum(dats$cases_dorms,pars$duration3)
    output = foo1+foo0 #+foo2
    output[output<0]=0
    return(output)
    
    output[output<0]=0
    return(output)
  }
  
  if(type=='local')
  {
    foo0=pars$a00
    alpha = exp(pars$a0 + pars$a1*(mobility$residential_percent_change_from_baseline[1:length(dats$cases_community)]/100)
                + pars$a2*(mobility$transit_stations_percent_change_from_baseline[1:length(dats$cases_community)]/100)
                + pars$a3*(mobility$grocery_and_pharmacy_percent_change_from_baseline[1:length(dats$cases_community)]/100)
                + pars$a4*(mobility$grocery_and_pharmacy_percent_change_from_baseline[1:length(dats$cases_community)]/100)
                + pars$a5*(mobility$parks_percent_change_from_baseline[1:length(dats$cases_community)]/100)
                + pars$a6*(mobility$retail_and_recreation_percent_change_from_baseline[1:length(dats$cases_community)]/100)
    )

    if(any(alpha>1)){
      alpha[which(alpha>=1)] = sample(seq(0.1,1,0.2),length(alpha[which(alpha>=1)]),prob = rep(1/length(seq(0.1,1,0.2)),length(seq(0.1,1,0.2))),replace = T)
    }
    foo2=alpha*dcumsum(dats$cases_community,pars$duration2)
    
    cb = as.numeric((dats$doy>=98)*(dats$doy<=153))
    foo2[cb==1] = foo2[cb==1]*pars$cbe
    output = foo0+foo2
    return(output)
  }
  
  if(type=='imported')
  {
    output = pars$a1+pars$a2*dnorm(dats$doy,pars$a3,pars$a4) +  pars$a5*(dats$doy>=234) + 
      (pars$a5-pars$a1)*as.numeric((dats$doy>182 & dats$doy<234))*c(rep(0,length(which(dats$doy<=182))),
                                                                    (dats$doy[(dats$doy>182)&(dats$doy<234)]-182)/(234-182))#c(rep(0,159),(pars$a5*(dat$doy[160:length(dat$doy)]-178)))#pars$a5*dnorm(dat$doy,pars$a6,pars$a7)
    return(output)
  }
  
  if(type=='deaths')
  {
    output = (pars$risk/pars$duration)*pcumsum(dats$icu,pars$duration)
    return(output)
  }
  
}




projector = function(iteration,tdelta=14)
{
  M=mcmc

  proj = as.list(dat)
  proj$cases = proj$cases_community+proj$cases_dorms +proj$cases_imported
  T0 = length(dat$doy)
  tmax = dat$doy[T0]
  proj$doy = c(proj$doy,max(proj$doy)+(1:tdelta))
  proj$date = c(proj$date,max(proj$date)+(1:tdelta))
  proj$icu = c(proj$icu,rep(0,tdelta))
  proj$deaths = c(proj$deaths,rep(0,tdelta))
  proj$cases = c(proj$cases,rep(0,tdelta))
  proj$cases_imported = c(proj$cases_imported,rep(0,tdelta))
  proj$cases_community = c(proj$cases_community,rep(0,tdelta))
  proj$cases_dorms = c(proj$cases_dorms,rep(0,tdelta))
  proj$infns_dorms = c(proj$infns_dorms,rep(0,tdelta))
  
  sirpars = initialise(data=proj$doy,S0 = NFWs)
  sirpars$E0=M$m4$E0[iteration]
  sirpars$beta=M$m4$beta[iteration]
  sirpars$sig = M$m4$sig[iteration]
  sirpars$p =M$m4$p[iteration]
  sirpars$threshold = M$m4$threshold[iteration]
  sirpars$interventioneffect=M$m4$interventioneffect[iteration]
  epidemic_foreign=SEIR(sirpars)$dIcalib
  proj$infns_dorms = epidemic_foreign
  
  for(td in 1:tdelta)
  {
    I_m = epidemic_foreign#[T0+td]
    toohigh = I_m > (sirpars$threshold/sirpars$p)
    mu = sirpars$p*I_m*(!toohigh) + sirpars$threshold*(toohigh)
    proj$cases_dorms[T0+td] = rpois(1,mu[T0+td])
    proj$infns_dorms[T0+td] = I_m[T0+td]
    
    mu = muf(proj,M$m3[iteration,],'local')
    proj$cases_community[T0+td] = rpois(1,mu[T0+td])
    mu = muf(proj,M$m2[iteration,],'imported')
    proj$cases_imported[T0+td] = rpois(1,mu[T0+td])
    
    
    mu = muf(proj,M$m1[iteration,],'icu')
    proj$icu[T0+td] = round(rpois(1,mu[T0+td])
    );if(proj$icu[T0+td]<0)proj$icu[T0+td]=0
    mu=muf(proj,M$m5[iteration,],'deaths')
    proj$deaths[T0+td] = mu[T0+td]
    

  }
  

  
  
  proj$cases_imported[1:T0] = rpois(T0,muf(dat,M$m2[iteration,],'imported'))
  proj$cases_community[1:T0] = rpois(T0,muf(dat,M$m3[iteration,],'local'))

  I_m = epidemic_foreign[1:T0]
  toohigh = I_m > (sirpars$threshold/sirpars$p)
  mu = sirpars$p*I_m*(!toohigh) + sirpars$threshold*(toohigh)
  proj$cases_dorms[1:T0] = rpois(1:T0,mu)
  proj$infns_dorms[1:T0] = I_m
  
  proj$cases = proj$cases_community+proj$cases_dorms +proj$cases_imported
  mu=muf(dat,M$m1[iteration,],'icu')
  proj$icu[1:T0] = rpois(T0,mu)
  proj$icu[proj$icu<0]=0
  proj$deaths[1:T0]=muf(dat,M$m5[iteration,],'deaths')
  
  return(proj)
}


p=projector(100,projectionwindow)
sampleit = seq(10,MCMCITS,10)
M=matrix(0,length(sampleit),length(p$icu))
output = list(t=p$doy,icu=M,deaths=M,
              cases_imported=M,cases_community=M,cases_dorms=M,infns_dorms=M,cuminfns_dorms=M,
              cumcases_imported=M,cumcases_community=M,cumcases_dorms=M)

for(i in seq(sampleit))
{
  p=projector(sampleit[i],projectionwindow)
  output$icu[i,]=p$icu
  output$deaths[i,]=p$deaths
  output$cumcases_imported[i,]=cumsum(p$cases_imported)
  output$cumcases_community[i,]=cumsum(p$cases_community)
  output$cumcases_dorms[i,]=cumsum(p$cases_dorms)
  output$cases_imported[i,]=(p$cases_imported)
  output$cases_community[i,]=(p$cases_community)
  output$cases_dorms[i,]=(p$cases_dorms)
  output$infns_dorms[i,]=p$infns_dorms
  output$cuminfns_dorms[i,]=cumsum(p$infns_dorms)
}

projections = output
#projections
save('projections',file = paste0('working/',dat$doy[length(dat$doy)],'/projections.rdata'))


 rm(dat,M,mcmc,output,p,projections,i,sampleit,dcumsum,pcumsum,projector)
 rm(initialise,loglikelihood,mh,SEIR,muf)

