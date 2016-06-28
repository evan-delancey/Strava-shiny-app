# Shiny app for gps strava data in 3 locations

library(shiny)
library(leaflet)
library(dplyr)
library(SDMTools)
library(ggplot2)

# set color pallettes
speed.pal <- colorRampPalette(c("#ffffcc","#fed976","#fd8d3c","#e31a1c","#800026","#000000"))
ele.pal <- colorRampPalette(c("#4d9221","#f5f5f5","#8c510a"))
slope.pal <- colorRampPalette(c("#d73027","#f46d43","#fdae61","#fee08b","#ffffbf","#d9ef8b","#a6d96a","#66bd63","#1a9850"))

#location of legend
y=c(0.8,0.8,0.2,.2)
x=c(0,0.60,0.60,0)


# heart of shiny app
shinyApp(
	# user interface
	ui = fluidPage(theme = "bootstrap.css",
	
	# set colors such as background color, text color, font
	tags$head(tags$style(
    HTML('
         #sidebar {
			font-family: "Arial"
            background-color: #303030;
			color.btn-default: #7CFC00;
        }
		
		selectInput{
			color: #303030;
		}
		

        body, label, input, button, select, others { 
          font-family: "Arial";
		  background-color: #303030;
		  color: #7CFC00;
        }')
	)),

	
	#set selection color in select input drop down menus
	tags$head(
      tags$style(HTML(".selectize-input.input-active, .selectize-input.input-active:hover, .selectize-control.multi .selectize-input.focus {border-color: #7CFC00 !important;}
                      .selectize-dropdown .active {background: #7CFC00  !important;} { color: blue }"))),

		mainPanel(leafletOutput('myMap',width="100%",height="800px")),
		sidebarPanel(id="sidebar",width=3,
			selectInput("select", "Location:", choices = c("Edmonton", "Banff", "Saturna Island"), selected = "Edmonton"), #l ocation
			selectInput("variable", "Vizualize:", choices = c("speed (km/h)", "elevation (m)", "slope"), selected = "speed(km/h"), # what the circles will vizualize
			selectInput("background", "Background:", choices = c("satellite", "roads", "simple"), selected = "satellite"), # background tiles
			selectInput("type", "Type of exersise:", choices = c("all", "bike", "run/walk"), selected = "all"), # type of exersise
			selectInput("plot", "Vizualize stats", choices = c("slope vs speed", "speed histogram (chosen location)", "ride length histogram (all locations)"), selected = "slope vs speed"), # plot statistics
			plotOutput("legend",width=100,height=200),
			plotOutput("stats",width=400,height=200)
			
		)
	),




	
# server section of shiny app
	server = function (input, output){
	observe({
		#read in data
		ed <- read.csv("data/ed.csv")
		sat <- read.csv("data/sat.csv")
		ban <- read.csv("data/ban.csv")
		
		#combine all data for summary plots
		df <- rbind(ed,sat,ban)
		
		# switch data base on location
		d <- switch(input$select,
			"Edmonton" = ed,
			"Saturna Island" = sat,
			"Banff" = ban
		)
		
		#switch type of exersise
		d <- switch(input$type,
			"all" = d,
			"bike" = filter(d,type=="bike"),
			"run/walk" = filter(d,type=="walk")
		)
		
		#location of where to display map based on location
		lat.choice <- switch(input$select,
			"Edmonton" = 53.544,
			"Saturna Island" = 48.7822,
			"Banff" = 51.1784
		)
		
		#location of where to display map based on location
		lon.choice <- switch(input$select,
			"Edmonton" = -113.4909,
			"Saturna Island" = -123.1642,
			"Banff" = -115.5708
		)
		
		# what variable to display on the circles
		viz.choice <- switch(input$variable,
			"speed (km/h)" = d$speed_col,
			"elevation (m)" = d$ele_col,
			"slope" = d$slope_col
		)
		
		# satellite, roads, or simple background tiles
		background.choice <- switch(input$background,
			"satellite" = "Esri.WorldImagery",
			"roads" = "MapQuestOpen.OSM",
			"simple" = "Stamen.Toner"
		)
		
		# color palette for color ramp
		pal.choice <- switch(input$variable,
			"speed (km/h)" = speed.pal(1000),
			"elevation (m)" = ele.pal(1000),
			"slope" = slope.pal(1000)
		)
		
		# what variable to display
		viz.choice1 <- switch(input$variable,
			"speed (km/h)" = d$speed,
			"elevation (m)" = d$ele,
			"slope" = d$slope
		)
		
		#title of the color ramp
		tit <- switch(input$variable,
			"speed (km/h)" = "Speed (km/h)",
			"elevation (m)" = "Elevation (m)",
			"slope" = "Slope"
		)
		
		#calculates ride length for all locations
		unique.rides <- distinct(select(df,ride_num))
		ride.len <- vector()
		type.vect <- vector()
		place.vect <- vector()
		for (i in 1:length(unique.rides[,1])) {
			r1 <- filter(df, ride_num==unique.rides[i,1])
			typ <- paste0(r1$type[1])
			plc <- paste0(r1$place[1])
			dist <- (sum(r1$speed)/length(r1$speed)) * (length(r1$speed)/60/60*4)
			ride.len <- c(ride.len,dist)
			type.vect <- c(type.vect,typ)
			place.vect <- c (place.vect,plc)
		}
		df.ride.len <- cbind(ride.len,type.vect,place.vect)
		df.ride.len <- as.data.frame(df.ride.len)
		df.ride.len[,1] <- as.numeric(as.character(df.ride.len[,1]))
		
		# save the ggplot to plt object for graphing variables
		plt <- switch(input$plot,
			"slope vs speed" = ggplot(d, aes(slope,speed)) + geom_point(color="#7CFC00") + labs(x = "slope", y = "speed (km/h)") +
				theme(axis.text = element_text(size = 18, color = "#B3B3B3"), panel.background = element_rect(fill='#303030',color='#303030'), plot.background = element_rect(fill='#303030',color='#303030'), 
				axis.title.x = element_text(size=20,color = "#7CFC00"), axis.title.y = element_text(size=20,color = "#7CFC00"), panel.grid.major = element_line(colour = "#303030"),
				panel.grid.minor = element_line(colour = "#303030")),
				
			"speed histogram (chosen location)" = ggplot(d, aes(speed)) + geom_histogram(aes(fill=type)) + labs(x = "speed (km/h)", y = "count") + scale_fill_manual("Type of exersise",values=c("#7CFC00","#B3B3B3")) +
				theme(axis.text = element_text(size = 18, color = "#B3B3B3"), plot.background = element_rect(fill='#303030', color='#303030'), 
				panel.background = element_rect(fill='#303030',color='#303030'), axis.title.x = element_text(size=20,color = "#7CFC00"), axis.title.y = element_text(size=20,color = "#7CFC00"),
				panel.grid.major = element_line(colour = "#303030"), panel.grid.minor = element_line(colour = "#303030"), legend.justification = c(1, 1), legend.position = c(1, 1),
				legend.background = element_rect(colour = "#303030", fill = "#303030"), legend.title = element_text(color = "#B3B3B3"), legend.text = element_text(color = "#B3B3B3"), 
				legend.key = element_rect(colour = "#303030")),
				
			"ride length histogram (all locations)" = ggplot(df.ride.len, aes(ride.len)) + geom_histogram(aes(fill=place.vect),bins=12) + labs(x = "distance (km)", y = "count") + 
				scale_fill_manual("Location",values=c("#7CFC00","#B3B3B3", "#FFA500")) +
				theme(axis.text = element_text(size = 18, color = "#B3B3B3"), plot.background = element_rect(fill='#303030', color='#303030'), panel.background = element_rect(fill='#303030',color='#303030'),
				axis.title.x = element_text(size=20,color = "#7CFC00"), axis.title.y = element_text(size=20,color = "#7CFC00"),
				panel.grid.major = element_line(colour = "#303030"), panel.grid.minor = element_line(colour = "#303030"), legend.title = element_text(color = "#B3B3B3"), legend.text = element_text(color = "#B3B3B3"),
				legend.justification = c(1, 1), legend.position = c(1, 1), legend.background = element_rect(colour = "#303030", fill = "#303030"), legend.key = element_rect(colour = "#303030"))
		)
		
		
		#renders map
		map = leaflet(d) %>% addProviderTiles(background.choice) %>% setView(lon.choice, lat.choice, 12) %>% addCircles(lat=~lat,lng=~lon,radius=0.5,col=~viz.choice,fill=T,fillOpacity=0,opacity=1)
		output$myMap = renderLeaflet(map)
		
		#render color ramp
		output$legend <- renderPlot({
			par(mar=c(0,0,0,0))
			par(bg = '#303030')
			plot(1,1,pch="",axes=F,xlab="",ylab="",xlim=c(0,1),ylim=c(0,1))
			legend.gradient(cbind(x = x, y = y), cols = pal.choice, title = tit, limits = c(round(min(viz.choice1),digits=0),round(max(viz.choice1),digits=0)), col = "#B3B3B3", cex = 1.2, font = 2)
		}) 
		
		# renders satistics plots
		output$stats <- renderPlot({
			plt
		})
	})
	}
)
	
