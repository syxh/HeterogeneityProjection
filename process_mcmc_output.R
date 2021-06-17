smoother = function(x,y)
{
  require(mgcv)
  oldd = data.frame(x=x,y=y)
  newd = data.frame(x=min(x):max(x))
  fit = gam(y~s(x,k=15),data=oldd)
  pred = as.vector(predict(fit,newdata=newd))
  pred[pred<0]=0
  return(pred)
}

 load(paste0('working/',dat$doy[length(dat$doy)],'/projections.rdata'))
  

  PROJECTIONS=projections

  for(KKK in 1:3)
  {
    output = matrix(0,length(PROJECTIONS$t),14)
    output[,1]=PROJECTIONS$t
    for(typeid in 1:13)
    {
      
      type=c('icu','deaths','ci','cc','cd','id','cci','ccc','ccd','logcci','logccc','logccd','cid')[typeid]
    
      if(type=='icu'){z = PROJECTIONS$icu}
      if(type=='deaths'){z = PROJECTIONS$deaths}
      if(type=='cci'){z = PROJECTIONS$cumcases_imported}
      if(type=='ccc'){z = PROJECTIONS$cumcases_community}
      if(type=='ccd'){z = PROJECTIONS$cumcases_dorms}
      if(type=='ci'){z = PROJECTIONS$cases_imported}
      if(type=='cc'){z = PROJECTIONS$cases_community}
      if(type=='cd'){z = PROJECTIONS$cases_dorms}
      if(type=='id'){z = PROJECTIONS$infns_dorms}
      if(type=='logcci'){z = PROJECTIONS$cumcases_imported;z[z<1]=0.5;z=log10(z);}
      if(type=='logccc'){z = PROJECTIONS$cumcases_community;z[z<1]=0.5;z=log10(z);}
      if(type=='logccd'){z = PROJECTIONS$cumcases_dorms;z[z<1]=0.5;z=log10(z);}
      if(type=='cid'){z = PROJECTIONS$cuminfns_dorms}
      
      CI=matrix(0,dim(z)[2],3)
      for(j in 1:dim(z)[2])CI[j,]=quantile(z[,j],c(25,500,975)/1000)
    
      hat = CI[,KKK]#smoother(PROJECTIONS$t,CI[,KKK])
      
      output[,typeid+1]=hat
      
    }
  
    output = as.data.frame(output)
    names(output) = c("t", "icu", "deaths", "cases_imported", "cases_community", "cases_dorms", "infns_dorms", 
                      "cumcases_imported", "cumcases_community", "cumcases_dorms",
                      "logcumcases_imported", "logcumcases_community", "logcumcases_dorms","cuminfns_dorms")
    write.csv(output,c(paste0('output/data/',dat$doy[length(dat$doy)],'/projection1_lower.csv'),
                                     paste0('output/data/',dat$doy[length(dat$doy)],'/projection1_central.csv'),
                                     paste0('output/data/',dat$doy[length(dat$doy)],'/projection1_upper.csv'))[KKK],row.names = FALSE)
    
  }

rm(CI,output,PROJECTIONS,projections,z,hat,j,KKK,type,typeid,smoother)
