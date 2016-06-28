# 2)calc_stats
	# takes data frame of all my strava rides and calculates speed, slope, and assigns colors to 
	# represent these later on a map

library(raster)
library(dplyr)
library(plotKML)

#location of strava rides data frame
setwd(wrk <- "C:/Users/edelance/Desktop/strava/data")

dd <- read.csv("rides_LCC.csv")
rides <- distinct(select(dd,ride_num))

dd$speed <- 0
dd$slope <- 0

df <- data.frame(c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1))
colnames(df) <- colnames(dd)

# for each ride calculate speed and slope between points
for (j in 1:length(rides[,1])){

d <- filter(dd,ride_num == j)

for (i in 1:(length(d[,1]) - 1)){
	# speed
	dx <- abs(d[(i+1),11] - d[i,11])
	dy <- abs(d[(i+1),12] - d[i,12])
	dist <- sqrt((dx^2) + (dy^2))
	d[(i+1),13] <- dist * 3.6
	
	#slope
	dele <- d[(i+1),4] - d[i,4]
	d[(i+1),14] <- (tan(dele / dist)) * 100
}

# recombine rides
df <- rbind(df,d)
print(j)
}

# delete dummy first row
df <- df[-1,]
df <- df[,-1]

# filter out spurious data
df$speed[df$speed>60] <- 20
df$slope[df$slope>15] <- 0
df$slope[df$slope<(-15)] <- 0


# assign color palette
speed.pal <- colorRampPalette(c("#ffffcc","#fed976","#fd8d3c","#e31a1c","#800026","#000000"))
df$speed_col <- speed.pal(30)[as.numeric(cut(df$speed,breaks=30))]
df$speed_size <- ifelse(df$speed<10,0.1,ifelse(df$speed<15,4,8)) 

# filter out locations
ed <- filter(df,place=="Edmonton")
ed$ele[ed$ele<600] <- 620
sat <- filter(df,place=="Saturna")
Banff <- filter(df,place=="Banff")

#assign elevation palette
ele.pal <- colorRampPalette(c("#4d9221","#f5f5f5","#8c510a"))
ed$ele_col <- ele.pal(30)[as.numeric(cut(ed$ele,breaks=30))]
sat$ele_col <- ele.pal(30)[as.numeric(cut(sat$ele,breaks=30))]
Banff$ele_col <- ele.pal(30)[as.numeric(cut(Banff$ele,breaks=30))]

# assign slope palette
slope.pal <- colorRampPalette(c("#d73027","#f46d43","#fdae61","#fee08b","#ffffbf","#d9ef8b","#a6d96a","#66bd63","#1a9850"))
ed$slope_col <- slope.pal(30)[as.numeric(cut(ed$slope,breaks=30))]
sat$slope_col <- slope.pal(30)[as.numeric(cut(sat$slope,breaks=30))]
Banff$slope_col <- slope.pal(30)[as.numeric(cut(Banff$slope,breaks=30))]

# remove every xth row is shiny app cannot easily display all pos points
ed <- ed[seq(1,length(ed[,1]),6),]
sat <- sat[seq(1,length(sat[,1]),4),]
Banff <- Banff[seq(1,length(Banff[,1]),4),]

# save csvs to be used in shiny app
setwd("C:/Users/edelance/Desktop/Shiny/strava/data")
write.csv(ed,"ed.csv",row.names=F)
write.csv(sat,"sat.csv",row.names=F)
write.csv(Banff,"ban.csv",row.names=F)




