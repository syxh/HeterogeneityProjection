NFWs=323000
MCMCITS=10000

# Data preparation --------------------------------------------------------

fulldata = read.csv('data/daily_data.csv',as.is=TRUE)
tail(fulldata)
fulldata$date=as.Date(fulldata$date,format = '%m/%d/%Y')

dailydata = fulldata[which(fulldata$date<=as.Date("2020-12-31","%Y-%m-%d")),]
rm(fulldata)
mobility = read.csv("data/Region_Mobility_Report_CSVs/2020_SG_Region_Mobility_Report.csv")
mobility = mobility[,-c(1:8)]
tail(mobility)
mobility$date <- as.Date(as.character(mobility$date),"%m/%d/%Y")
# From Jan 19 till Feb 14, fill the mobility information as 0
id_fill <- which(dailydata$date < as.Date("2020-02-15","%Y-%m-%d") & !dailydata$date%in%mobility$date)
x = as.data.frame(do.call('cbind', lapply(2:length(colnames(mobility)),function(i){rep(0,length(id_fill))})))
names(x) <- colnames(mobility)[-1]
mobility_extr <- cbind(date=dailydata$date[id_fill],x)
mobility_data <- rbind(mobility_extr,mobility)
rm("id_fill","mobility_extr",'x','mobility')

if(nrow(dailydata)>nrow(mobility_data)){
  dailydata <- dailydata[which(dailydata$date%in%mobility_data$date),]
}else{

  dailydata <- dailydata[which(mobility_data$date%in%dailydata$date),]
  mobility_data <- mobility_data[which(mobility_data$date%in%dailydata$date),]
}
tail(dailydata)
tail(mobility_data)
##################################################

# Folder organization (processed data and output) ----------
# As for the processed data, they are all save into working folder, sub-folders were named by the doy
for(doy_index in 14:348# seq(211,300,21)#74:length(dailydata$doy)
    ){
  sub_folder_name = file.path('working',dailydata$doy[doy_index])
  if(!file.exists(sub_folder_name)){
    dir.create(sub_folder_name)}
}
# As for the output, separate as the output data, which including the projection results; 
# and output plot, which includes projections of mobility and infections by types

for(doy_index in 14:348#seq(211,300,21)#74:length(dailydata$doy)
    ){
  sub_folder_name = file.path('output','data',dailydata$doy[doy_index])
  if(!file.exists(sub_folder_name)){
    dir.create(sub_folder_name)}
}

rm(doy_index,sub_folder_name)


##################################################

projectionwindow = 90

for(daily_index in 14:348
 )
{
  dat = dailydata[c(1:daily_index),]
  mobility = mobility_data
  source('code/functions.r')
  source('code/mcmc1_total_icu.r')
  source('code/mcmc2_new_cases_imported.r')
  source('code/mobility_prediction_transit.R')
  source('code/mobility_prediction_resident.R')
  source('code/mobility_prediction_grocery.R')
  source('code/mobility_prediction_parks.R')
  source('code/mobility_prediction_works.R')
  source('code/mobility_prediction_retail.R')
  source('code/mcmc3_new_cases_local.R')
  source('code/mcmc4_new_cases_fws.r')
  source('code/mcmc5_deaths.r')
  source('code/pool_mcmc.r')
  source('code/projector.r')
  source('code/process_mcmc_output.r')
}


