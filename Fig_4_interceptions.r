# --- figure 4 : introduction's pathways ---
# More info here: https://github.com/JeromeMathieuEcology/GlobalWorming.

# librairies
	library(ggplot2)
	library(Cairo) # we need Cairo in order to export the figure with embede policies in the file
	library(patchwork)
	library(ggExtra)
	library(rgeos)
	library(rworldmap)
	library(rgdal)
	library(maps)
	library(viridis)
	library(png)
	library(grid)
	library(cowplot)

# read data
	interceptions <- read.csv2(".\\EWINA\\EWINA_IPATHS\\EWINAPATH.csv", sep = ",")

	# make sure variables are numeric
		interceptions$lon_source <- as.numeric(interceptions$lon_source)
		interceptions$lat_source <- as.numeric(interceptions$lat_source)
		interceptions$lon_arrival <- as.numeric(interceptions$lon_arrival)
		interceptions$lat_arrival <- as.numeric(interceptions$lat_arrival)


# read icons
	plane <- readPNG(".\\images\\plane.png")
	gp <- rasterGrob(plane, interpolate=TRUE)

	boat <- readPNG(".\\images\\boat.png")
	gb <- rasterGrob(boat, interpolate=TRUE)

	mail <- readPNG(".\\images\\mail.png")
	gm <- rasterGrob(mail, interpolate=TRUE)

	car <- readPNG(".\\images\\car.png")
	gc <- rasterGrob(car, interpolate=TRUE)

	unknown <- readPNG(".\\images\\unknown.png")
	gu <- rasterGrob(unknown, interpolate=TRUE)



# TRANSPORTS MODES --------------------------------------------------------------------

	# nb of individuals by transportation mode

		trspt_table <- data.frame(sort(table(interceptions$transport),decreasing=T))
		# rename
			names(trspt_table) <- c("mode","freq")
		# capitalize 1st letter
			trspt_table$mode <- stringr::str_to_title(trspt_table$mode)

		# ordering transport modes by frequency
			trspt_table$mode <- factor(trspt_table$mode, levels = trspt_table$mode[order(trspt_table$freq)])
	 		


	# nb of species by tranportation mode

	 	interceptions_sp <- interceptions[-grep("sp$|sp.$",interceptions$species_name),]
		table_mode_sp <- table(interceptions_sp$transport,interceptions_sp$species_name)
		trspt_table2 <- sort(vegan::specnumber(table_mode_sp),decreasing=T)

		names(trspt_table2) <- c("mode","freq")
		trspt_table2 <- data.frame(trspt_table2)
		trspt_table2 <- data.frame(mode = row.names(trspt_table2), nsp = trspt_table2)
		names(trspt_table2) <- c("mode","nsp")

		trspt_table2$mode <- factor(trspt_table2$mode, levels = trspt_table2$mode[order(trspt_table2$nsp)])
		trspt_table2$mode <- stringr::str_to_title(trspt_table2$mode)
		trspt_table2$nsp <- rev(trspt_table2$nsp)


# plots 

	font_size <- 8

	# 1° FIG 3B / plot of mean of transportation summary

		gtrspt <- ggplot(trspt_table2, aes(mode, nsp)) +
						geom_point(aes(x = mode, y = 0), size = 2, color = "#00441b", alpha = .8) +		# departure
						geom_point(size = 2, color = "#800026", alpha = .8) + 							# arrival
						geom_segment(data = subset(trspt_table2, mode == 1), aes(x = mode, y = 3, xend = mode, yend = nsp-3), color = "#bf781b", size = .8, lineend = "round", linejoin = "bevel", arrow = arrow(length = unit(0.1, "inches"))) +
						geom_segment(data = subset(trspt_table2, mode == 2), aes(x = mode, y = 3, xend = mode, yend = nsp-3), color = "#544608", size = .8, lineend = "round", linejoin = "bevel", arrow = arrow(length = unit(0.1, "inches"))) +
						geom_segment(data = subset(trspt_table2, mode == 3), aes(x = mode, y = 3, xend = mode, yend = nsp-3), color = "#7570b3", size = .8, lineend = "round", linejoin = "bevel", arrow = arrow(length = unit(0.1, "inches"))) +
						geom_segment(data = subset(trspt_table2, mode == 4), aes(x = mode, y = 3, xend = mode, yend = nsp-3), color = "#1b9e77", size = .8, lineend = "round", linejoin = "bevel", arrow = arrow(length = unit(0.1, "inches"))) +
				        coord_flip() +
				        ylim(0, 40) +
				        theme_minimal() +
				        xlab("Introduction\nPathways") +	
				        ylab("Number of species") +	

				        # icons
					        annotation_custom(gp, xmin = 4.1,  xmax = 4.6,  ymin = 0, ymax = 11) +
					        annotation_custom(gb, xmin = 2.98, xmax = 3.62, ymin = 2, ymax = 12) +
					        annotation_custom(gu, xmin = 2.07, xmax = 2.4,  ymin = 0, ymax = 10) +
					        annotation_custom(gc, xmin = 1.07, xmax = 1.33, ymin = 0, ymax = 10) +
                 			removeGridX() +
				
				        theme(	axis.title.y = element_text( colour="grey10", size = font_size, margin =  margin(t = 0, r = 10, b = 0, l = 0)),
				        	  	axis.title.x = element_text( colour="grey10", size = font_size, margin = margin( t = 5)), #family= "Georgia",
				        	  	axis.text = element_text(colour="grey10", size = font_size),
				        	  	axis.text.y = element_blank(),
				        	  	panel.grid.major.x = element_line(size = 0.1, linetype = 5 , colour = "grey60"),
				        	  	panel.grid.major.y = element_blank(),
				        	  	  	#text = element_text(family = "Georgia"),
				        	  	plot.margin = margin(t = 0, r = 40, b = 0, l = 40, unit = "pt"),
				        	  	legend.position = "none")


	# 2° Fig 3A / map ------------------

		# get world map
		  	wmap <- getMap(resolution="high")

		# map transports -----------

			worldmap <- borders("world", colour =  "transparent",
											fill = "grey60" ,
											size = 0.05) 
			
			map_intercep <-	ggplot(interceptions) + worldmap +
					scale_alpha(name = "", range = c(0.2, 0), guide = F) +
					geom_point(data = interceptions, aes(x = lon_source, y = lat_source), col = "#00441b", size = .8, alpha = .5) + 
				    geom_point(data = interceptions, aes(x = lon_arrival, y = lat_arrival), col = "#800026", size = .8,  alpha = .5) +
				    geom_curve(data = subset(interceptions, transport == "plane"), aes(x = lon_source, y = lat_source, xend = lon_arrival, yend = lat_arrival), col = "#1b9e77",
							curvature = -.3, size = .2 , alpha=.2) + 
				    geom_curve(data = subset(interceptions, transport == "boat"), aes(x = lon_source, y = lat_source, xend = lon_arrival, yend = lat_arrival), color = "#7570b3",
							curvature = .1, size = .2 , alpha=.2) + 
				    geom_curve(data = subset(interceptions, transport == "unknown"), aes(x = lon_source, y = lat_source, xend = lon_arrival, yend = lat_arrival), color = "#544608",
							curvature = .5, size = .2 , alpha=.2) + 
				    geom_curve(data = subset(interceptions, transport == "car"), aes(x = lon_source, y = lat_source, xend = lon_arrival, yend = lat_arrival), color = "#bf781b",
							curvature = -.2, size = .2 , alpha=.5) + 			    
				  	ylim(-60, 90) +
				  	theme(axis.title = element_blank(),
				  			axis.text = element_blank(),
				  			axis.ticks = element_blank(),
				  			panel.background = element_blank(),
				  			plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
				  			legend.position = "none")



	# 3° composite figure ------------

		ggdraw() +
				draw_plot(map_intercep , 0, 0.45, 1, .5) +
				draw_plot(gtrspt , 0.2, 0.06, 0.6, .35) +
		  		draw_plot_label("A", .05, 0.95, size = 10) +
		 		draw_plot_label("B", .31, 0.41, size = 10)

# I/O 
 	ggsave("Fig_4_interceptions_map.pdf", device = cairo_pdf, width = 4.75, height = 4.5)

