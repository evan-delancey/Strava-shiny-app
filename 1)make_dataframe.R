# 1) make_dataframe.R
	# takes origional gpx files from strava and combines all rides into a data frame

library(raster)
library(dplyr)
library(plotKML)

# location of gpx files
setwd(wrk <- "C:/Users/edelance/Desktop/strava/rides")
lst <- list.files(wrk)

i=1

# make data frame
df <- data.frame(c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1))
colnames(df) <- c("lon", "lat", "ele", "time", "place", "commute", "type", "ride_num", "date")

# for every file in folder give certation attributes and cobine into data frame
for (files in lst){
	d <- readGPX(files)
	d <- d$tracks[[1]]
	d <- d[[1]]
	
	# assign location based on name of file
	if (grepl("Saturna", files)){
		d$place <- "Saturna"
	} else {
		if (grepl("Banff", files)){
			d$place <- "Banff"
		} else {
			d$place <- "Edmonton"
		}
	}
	
	# assign if commute base on name of file
	if (grepl("work", files)){
		d$commute <- 1 
	} else {
		d$commute <- 0
	}
	
	# assign type of exersis base on name of file
	if (grepl("walk", files)){
		d$type <- "run/walk"
	} else {
		d$type <- "bike"
	}
	
	# assign ride number
	d$ride_num <- i
	i = i + 1
	
	# give a date
	d$date <- substr(d$time,1,10)
	
	# combine ride into data frame
	df <- rbind(df,d)
}

# delete dummy first row of data frame
df <- df[-1,]

#save
write.csv(df,"C:/Users/edelance/Desktop/strava/data/rides.csv",row.names=F)

