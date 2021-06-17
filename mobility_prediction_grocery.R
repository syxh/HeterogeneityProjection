
mu_mobility = function(move_data,pars_m) # move_dat here is a vector, identify the mobility value of the same day of week (days).
{
  foo0=pars_m$b0 
  singlepred1 = pars_m$a1*dgamma(seq_along(move_data), pars_m$b1, pars_m$c1)
  foo1=pars_m$d1*rnorm(move_data,singlepred1,pars_m$sd)
  
  singlepred2 = pars_m$a2*dgamma(seq_along(move_data), pars_m$b2, pars_m$c2)
  foo2=pars_m$d2*rnorm(move_data,singlepred2,pars_m$sd)
  # cb = as.numeric((dailydata$doy>=98)*(dailydata$doy<=153))
  t=135 # June 1, stop cb
  if(length(move_data)>t){
    post_cb = c(rep(0,135),rep(1,(length(move_data)-135)))
    
  }else{
    post_cb = c(rep(0,length(move_data)))
  }
  # foo1=pars$b1*rnorm(1:length(move_dat),move_dat,pars$sd)
  output = foo0+foo1*(1-post_cb)+foo2*(post_cb)
  # output[output<0]=0
  return(output)
}

logl = function(move_data,pars_m)
{
  # dat_m=mobility$residential_percent_change_from_baseline[which(mobility$dow==1)]
  LL = sum(dnorm(move_data,mu_mobility(move_data,pars_m),log=TRUE))
  pars_m$LL = LL
  return(pars_m)
}


mh = function(oldp,newp,dat)
{
  reject = FALSE
  # if(newp$b0<0)reject=TRUE
  if(newp$sd<0)reject=TRUE
  if(newp$b1<0)reject=TRUE
  if(newp$c1<0)reject=TRUE
  if(newp$b2<0)reject=TRUE
  if(newp$c2<0)reject=TRUE
  
  # if(newp$b1<0)reject=TRUE
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

mcmc_mobility <- function(MCMCITS)
{
  set.seed(126)
  # pars_m = list(b0=0,b1=10,sd=0.1,a=-1000,b=15,c=0.1)
  pars_m = list(b0=1,d1=10,sd=0.1,a1=-1000,b1=15,c1=0.1,
                d2=12,sd=0.1,a2=-1200,b2=10,c2=0.2)
  move_data=mobility$grocery_and_pharmacy_percent_change_from_baseline#[which(mobility$dow==weekday)]
  pars_m = logl(move_data,pars_m)
  
  storage=list(
    b0=rep(0,MCMCITS),
    a1=rep(0,MCMCITS),
    b1=rep(0,MCMCITS),
    c1=rep(0,MCMCITS),
    d1=rep(0,MCMCITS),
    a2=rep(0,MCMCITS),
    b2=rep(0,MCMCITS),
    c2=rep(0,MCMCITS),
    d2=rep(0,MCMCITS),
    sd=rep(0,MCMCITS),
    LL=rep(0,MCMCITS)
  )
  SUBITS=10
  for(iteration in 1:MCMCITS)
  {
    if(iteration%%1000==0)cat(iteration,'in',MCMCITS,'\n')
    for(subit in 1:SUBITS)
    {
      oldpars = pars_m;
      pars_m$b0 = rnorm(1,pars_m$b0,0.2)# ; pars = mh(oldpars,pars,dat)
      pars_m$a1 = rnorm(1,pars_m$a1,10)# ; pars = mh(oldpars,pars,dat)
      pars_m$b1 = rnorm(1,pars_m$b1,0.5)# ; pars = mh(oldpars,pars,dat)
      pars_m$c1 = rnorm(1,pars_m$c1,0.1)# ; pars = mh(oldpars,pars,dat)
      pars_m$d1 = rnorm(1,pars_m$d1,1)# ; pars = mh(oldpars,pars,dat)
      pars_m$a2 = rnorm(1,pars_m$a2,10)# ; pars = mh(oldpars,pars,dat)
      pars_m$b2 = rnorm(1,pars_m$b2,2)# ; pars = mh(oldpars,pars,dat)
      pars_m$c2 = rnorm(1,pars_m$c2,0.2)# ; pars = mh(oldpars,pars,dat)
      pars_m$d2 = rnorm(1,pars_m$d2,0.5)# ; pars = mh(oldpars,pars,dat)
      pars_m$sd = rnorm(1,pars_m$sd,0.05)# ; pars = mh(oldpars,pars,dat)
      
      pars_m = mh(oldpars,pars_m,move_data)
    }
    storage$b0[iteration] = pars_m$b0
    storage$a1[iteration] = pars_m$a1
    storage$b1[iteration] = pars_m$b1
    storage$c1[iteration] = pars_m$c1
    storage$d1[iteration] = pars_m$d1
    storage$a2[iteration] = pars_m$a2
    storage$b2[iteration] = pars_m$b2
    storage$c2[iteration] = pars_m$c2
    storage$d2[iteration] = pars_m$d2
    storage$LL[iteration] = pars_m$LL
    storage$sd[iteration] = pars_m$sd
  }
  # plot(storage$a,type='l')
  # plot(storage$LL,type='l')
  # plot(storage$b,type='l')
  # plot(storage$c,type='l')
  # plot(storage$b0,type='l')
  # plot(storage$b1,type='l')
  # plot(dat_m)
  # lines(1:26,rnorm(1:26,pars_m$b1*pars_m$a*dgamma(1:26, pars_m$b, pars_m$c),0.1)+pars_m$b0)
  return(data.frame(storage))
}

projector_mobility <- function(wdelta,iteration)
{ 
  dat_m=mobility$grocery_and_pharmacy_percent_change_from_baseline#[which(mobility$dow==weekday)]
  seed=987
  set.seed(seed)
  proj=list()
  proj$mobility = dat_m
  T0 = length(dat_m)
  tmax = T0
  # proj$doy = c(1:length(dat_m),length(dat_m)+(1:tdelta))
  # proj$date = c(proj$date,max(proj$date)+(1:tdelta))
  proj$mobility = c(proj$mobility,rep(0,wdelta*7))
  
  for(td in 1:(wdelta*7))
  {
    mu = mu_mobility(proj$mobility,pars[iteration,])
    proj$mobility[T0+td] = -rpois(1,-mu[T0+td])#rnorm(1,mu[T0+td-1],3)
  }
  # proj$dow=rep(weekday,length(proj$mobility))
  proj$mobility[1:T0] = -rpois(T0,-mu_mobility(dat_m,pars[iteration,]))
  proj$mobility[is.na(proj$mobility)]=0
  
  # plot(proj$mobility,main = weekday)
  # lines(dat_m)
  # lines((length(dat_m)):length(proj$mobility),proj$mobility[(length(dat_m)):length(proj$mobility)],col='red')
  return(proj)
}
proj_mobility_data = function(wdelta,iteration)
{
  mob_col = c("date","dow","grocery_percent_change_from_baseline")
  # proj_date = seq.Date(from = (mobility$date[nrow(mobility)]+1),to = (mobility$date[nrow(mobility)]+wdelta*7),by=1)
  proj_date = seq.Date(from = (mobility$date[1]),to = (mobility$date[nrow(mobility)]+wdelta*7),by=1)
  
  proj_mobility = data.frame(date = proj_date,
                             dow = wday(proj_date),
                             grocery_percent_change_from_baseline = 0)
  proj_mobility$grocery_percent_change_from_baseline = projector_mobility(wdelta,iteration)$mobility
  # proj_tmp <- lapply(1:7,function(i){projector_mobility(i,wdelta,iteration)})
  # for(i in 1:7)
  # {
  #   proj_mobility$residential_percent_change_from_baseline[which(proj_mobility$dow==i)]=
  #     proj_tmp[[i]]$mobility
  # }
  mobility_update = data.frame(proj_mobility)
  mobility_update
}



# mobility = read.csv("data/SG_0615.csv")
# mobility = mobility[,-c(1:6)]
# mobility$date <- as.Date(as.character(mobility$date),"%Y/%m/%d")
# 
# # dailydata = read.csv('data/daily_data_2020-05-12.csv',as.is=TRUE)
# fulldata = read.csv('data/daily_data_2020-07-02.csv',as.is=TRUE)
# 
# # dailydata$date=as.Date(dailydata$date,format = '%d/%m/%y')
# fulldata$date=as.Date(fulldata$date,format = '%d-%m-%y')
# 
# # no matter what date the dailydata stopped, the following id_fill is for Jan 19 till Feb 14
# id_fill <- which(fulldata$date < as.Date("2020-02-15","%Y-%m-%d") & !fulldata$date%in%mobility$date)
# x = as.data.frame(do.call('cbind', lapply(2:length(colnames(mobility)),function(i){rep(0,length(id_fill))})))
# names(x) <- colnames(mobility)[-1]
# mobility_extr <- cbind(date=fulldata$date[id_fill],x)
# mobility <- rbind(mobility_extr,mobility)
# rm("id_fill","mobility_extr")


library(lubridate)
date <- mobility$date
mobility$dow = wday(mobility$date)#default week starts from sunday, i.e. dow==1 identify the Sunday
# day=wday(date)
# mon=as.numeric(day==2)
# tue=as.numeric(day==3)
# wed=as.numeric(day==4)
# thu=as.numeric(day==5)
# fri=as.numeric(day==6)
# sat=as.numeric(day==7)
# sun=as.numeric(day==2)
wdelta = projectionwindow/7

# MCMCITS=1000000

pars = mcmc_mobility(MCMCITS)
p_m=proj_mobility_data(wdelta,100)
sampleit = seq(10,MCMCITS,10)
#sampleit = seq(1,MCMCITS,1)
M=matrix(0,length(sampleit),length(p_m$dow))
output = list(t=p_m$dow,move=M)

for(i in seq(sampleit))
{
  p_m=proj_mobility_data(wdelta,i)
  output$move[i,]=p_m$grocery_percent_change_from_baseline
}


# plot(mobility$transit_stations_percent_change_from_baseline,type='l',col=2)
# lines(output$move[10000,],type='l')
mobility_projection = output
save(mobility_projection,file=paste0('working/',dat$doy[length(dat$doy)],'/movement_projection_grocery.rdata'))





# plot(mobility_update$residential_percent_change_from_baseline)
# lines(mobility$residential_percent_change_from_baseline)
# proj_x <- c(length(mobility$residential_percent_change_from_baseline):length(mobility_update$residential_percent_change_from_baseline))
# lines(x= proj_x,
#       y=mobility_update$residential_percent_change_from_baseline[proj_x])
# rm(iteration,logl,mh,oldpars,pars,mcmc_mobility,M,output,p_m,sampleit, wdelta,pcumsum,dcumsum,mobility_projection,SUBITS,mu_mobility)

