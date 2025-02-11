##############################################################
# Extract TerraClimate values to plots
# netcdf file format
# monthly temporal resolution
# 0.04° (~4 km) spatial resolution
# Dec 2024
##############################################################

rm(list=ls())

# increase maximum virtual memory allocation before extracting data

install.packages("usethis")
library(usethis) 
usethis::edit_r_environ()
# R_MAX_VSIZE <- 100Gb

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
dirs <- sapply(c(Dir.Base, Dir.Data, Dir.Code, Dir.Clim), function(x) if(!dir.exists(x)) dir.create(x) )


# packages
require("stringr")
require("raster")

# load Remote data
dataset_name <- "full_trees"
load(file=file.path(Dir.Data, paste0(dataset_name,".Rdata")) )
str(full_trees)        

# check for missing data
check <- full_trees[is.na(full_trees$lng),]
head(check)


### subset coordinates and remove NAs
plots <- unique(full_trees[,names(full_trees) %in% c("plotid","lng","lat")] )

# remove plot with missing coordinates
plots <- plots[!is.na(plots$lng),]
which(is.na(plots))
which(duplicated(plots$plotid))
str(plots)

plots_copy <- plots    

# climate variable list
Vars <- c("pet","ppt","tmax","tmin","soil")

# define dates
dates <- seq(1958,2023,1)
start_yr <- dates[1]
end_yr <-dates[length(dates)]

# netcdf layer names
mo_year_names <- paste( str_pad(string = 1:12, width = 2, "left", 0),rep(start_yr:end_yr, each = 12), sep="_")


############################
# loop through variables
plot_clim_array <- array(list(NULL),dim=length(Vars))

#index <- 1
for(index in 1:length(Vars))
{
	clim_var <- Vars[index]
		
	# load monthly Terra files (names of layers not retained - need to re-define)
	Terra_region <- stack(file.path(Dir.Clim, paste("1_TerraClimate",clim_var,"all_years_Region.nc",sep="_")))
	
	# define names = dates by month
	names(Terra_region) <- mo_year_names

	# Define plot dataframe as spatial object
	if(index == 1){
	   coordinates(plots) <- ~ lng + lat
	   proj4string(plots) <- crs(Terra_region)
	}

	# Extract bil values corresponding to plots (bilinear interpolation of 4 nearest cells)
	rasValue = extract(Terra_region, plots, method="bilinear")
	
	# add extracted values to plot dataset
	plot_clim <- cbind(plots_copy[,names(plots_copy) %in% c("plotid","lng","lat")], rasValue)
	head(plot_clim)
	
	# convert to long format
	plot_clim_long <- reshape(plot_clim, direction = "long", varying = list(names(plot_clim)[4:length(plot_clim)]), v.names = clim_var, 
		              idvar = c("plotid"), timevar = "period", times = mo_year_names)

	# Add year and month columns
	plot_clim_long$year <- as.numeric(substr(plot_clim_long$period, 4, 8))
	plot_clim_long$month <- as.numeric(substr(plot_clim_long$period, 1, 2))
	
	
	# Define water year  (oct to sept)
	plot_clim_long$Water_yr <- as.numeric((plot_clim_long$year)) 
	is.nxt <-  as.numeric((plot_clim_long$month)) %in% 10:12
	plot_clim_long$Water_yr[is.nxt] <- plot_clim_long$Water_yr[is.nxt]+1 

	# Define seasonal periods (3 month periods within a water year)
	plot_clim_long$Season = ifelse ((plot_clim_long$month == 12 | plot_clim_long$month == 1 | plot_clim_long$month == 2 ), "winter",      
					          ifelse ((plot_clim_long$month == 3 | plot_clim_long$month == 4 | plot_clim_long$month == 5), "spring",                 
					           ifelse ((plot_clim_long$month == 6 | plot_clim_long$month == 7 | plot_clim_long$month == 8 ), "summer",
							    "fall")))
	# combine variables
	if(index == 1) 
	{
	    plot_clim_array <- plot_clim_long
	}else{
	    plot_clim_array <- merge(plot_clim_array, plot_clim_long, by=c("plotid","lng","lat","period","year","month","Season","Water_yr"))
	}
}

colnames(plot_clim_array)[names(plot_clim_array) %in% "soil"] <- "soil_mm"
plot_clim_array <- droplevels(plot_clim_array)
summary(plot_clim_array)
str(plot_clim_array)

check <- unique(plot_clim_array[, names(plot_clim_array) %in% c("plotid","year")])
which(duplicated(check))


######
# Save monthly data long format
setwd(Dir.Clim)
save(plot_clim_array, file=paste("2_Plot_terra_climate_compiled_by_month_", start_yr, "_to_",end_yr,".Rdata",sep="") )
	











