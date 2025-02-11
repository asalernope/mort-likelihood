#############################################################
#  Mortality - disturbance project Balkans
#  Calculate census interval mean climate for each plot
#  calculate climate anomalies (interval - antecedent)
#  Dec 2024
#############################################################


library(tidyverse)
rm(list=ls())

# User computer
computer <- "~"

# Directores
if(computer == "~"){
  Dir.Base <- "~/Desktop/Prague/Code"
}else{
  Dir.Base <- "insert path"
}

Dir.Data <- file.path(Dir.Base, "Data")
Dir.Code <- file.path(Dir.Base, "Code")
Dir.Clim <- file.path(Dir.Data,"Climate")
dirs <- sapply(c(Dir.Base, Dir.Data, Dir.Code, Dir.Clim), function(x) if(!dir.exists(x)) dir.create(x))


# read tree data
load(file=file.path(Dir.Data,"full_trees.Rdata"))

targets <- full_trees
head(targets)
summary(targets)


##### combine census dates
survey_dates <- subset(targets,select=c(plotid, date, date_re)) %>%
  rename(date_census_1 = date, date_census_2 = date_re)
survey_dates <- unique(survey_dates)
which(duplicated(survey_dates))

write.csv(survey_dates,file=file.path(Dir.Data,"survey_dates.csv"),row.names=F)

# census interval summary
summary(survey_dates$date_census_2 - survey_dates$date_census_1) # not consistent 5 year intervals

# survey_dates$diff<-  survey_dates$date_census_2 - survey_dates$date_census_1

################
# load annual-resolution climate data (plot scale)
clim_df <- load(file=file.path(Dir.Clim,"5_plot_monthly_water_budget_variables.Rdata"))
clim_df <- get(clim_df)
head(clim_df)


clim_array <- array(data=list(NULL),dim=nrow(survey_dates))

# set length of antecedent climate interval
period_length <- 30

# loop through plot and calculate quantiles
pb <- txtProgressBar(min=0, max=length(survey_dates$plotid), style=3)  # progress bar
tind=0

# loop through plots and compute mean climate for census and prior intervals
i <- 1
for(i in 1:length(survey_dates$plotid) )   
{
	tind=tind+1
	setTxtProgressBar(pb, tind)
	
	p <- survey_dates$plotid[i]
	
	# get census interval
	start_yr <- survey_dates$date_census_1[which(survey_dates$plotid %in% p)]
	end_yr <- survey_dates$date_census_2[which(survey_dates$plotid %in% p)]
	
	# subset climate data to census interval
	dat.plot.int <- subset(clim_df, year>=start_yr &
								year<=end_yr &
								plotid %in% p )
	
	### calculate mean climate for census interval
	mean_ann_temps_int <- mean(dat.plot.int$temp_ann_K,na.rm=T)   
	mean_ann_precips_int <- mean(dat.plot.int$precip_ann_mm,na.rm=T)
	mean_ann_WD_ann_int <- mean(dat.plot.int$WD_ann_mm,na.rm=T) # water deficit (PET-AET)
	mean_ann_WB_int <- mean(dat.plot.int$water_balance_seasonal_mm,na.rm=T) # effective growing season water supply
	
	# max annual census climate  (See Canham & Murphy 2017)
	max_ann_temps_int <- max(dat.plot.int$temp_ann_K,na.rm=T)   
	max_ann_precips_int <- max(dat.plot.int$precip_ann_mm,na.rm=T)
	max_ann_WD_ann_int <- max(dat.plot.int$WD_ann_mm,na.rm=T) # water deficit (PET-AET)
	max_ann_WB_int <- max(dat.plot.int$water_balance_seasonal_mm,na.rm=T) # effective growing season water supply
	
	
	### calculate mean climate for prior interval
	end_yr <- start_yr - period_length

	dat.plot.ant <- subset(clim_df, year<start_yr &
							year>=end_yr &
							plotid %in% p )
	
# 	mean_ann_temps_ant <- quantile(dat.plot.ant$temp_ann_K,probs=c(0.5),na.rm=T)
#   mean_ann_precips_ant <- quantile(dat.plot.ant$precip_ann_mm,probs=c(0.5),na.rm=T)
# 	mean_ann_WD_ann_ant <- quantile(dat.plot.ant$WD_ann_mm,probs=c(0.5),na.rm=T)
# 	mean_ann_WB_ant <- quantile(dat.plot.ant$water_balance_seasonal_mm,probs=c(0.5),na.rm=T)
	mean_ann_temps_ant <- mean(dat.plot.ant$temp_ann_K,na.rm=T)
    mean_ann_precips_ant <- mean(dat.plot.ant$precip_ann_mm,na.rm=T)
	mean_ann_WD_ann_ant <- mean(dat.plot.ant$WD_ann_mm,na.rm=T)
	mean_ann_WB_ant <- mean(dat.plot.ant$water_balance_seasonal_mm,na.rm=T)

	
	### calculate climate anomalies
	temp_k_ann_diff <- mean_ann_temps_int - mean_ann_temps_ant 
	precip_ann_diff <- mean_ann_precips_int - mean_ann_precips_ant 
	WD_ann_diff <- mean_ann_WD_ann_int - mean_ann_WD_ann_ant 
	WB_ann_diff <- mean_ann_WB_int - mean_ann_WB_ant 

	
	clim_array[[i]] <- data.frame("plotid"=p, mean_ann_temps_int,mean_ann_temps_ant, temp_k_ann_diff,
								  mean_ann_precips_int,mean_ann_precips_ant, precip_ann_diff,
								  mean_ann_WD_ann_int,mean_ann_WD_ann_ant, WD_ann_diff,
								  mean_ann_WB_int,mean_ann_WB_ant, WB_ann_diff,
								  max_ann_temps_int,max_ann_precips_int,max_ann_WD_ann_int,max_ann_WB_int)
}

# convert to dataframe
plot_clim <- do.call(rbind,clim_array)

str(plot_clim)
summary(plot_clim)

# save census interval climate
save(plot_clim,file=file.path(Dir.Clim,"6_plot_mean_census_climate.Rdata"))
	






	

	
	















