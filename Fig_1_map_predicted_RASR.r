# --- script to produce Fig. 1 and Extended Fig. 10: maps of predicted relative alien species richness and its uncertainty ---
# More info here: https://github.com/JeromeMathieuEcology/GlobalWorming.


# You need to run the script 0_random_forest.r before running this one

# libraries
	library(tidyverse)
	library(sf)
	library(ggplot2)
	library(viridis)
	library(cowplot)
	library(ggmap) 
	library(maps)
	library(rgdal)
	library(Cairo)
	library(grid)
	library(gridExtra)


# custom functions	
	source(".\\functions\\map_theme.r")
	source(".\\functions\\ggplot_density.r")


# Load data
	# rasr_predicted_sf : layer with predicted RASR - from script 0

	# state boundaries
		states_sf <- read_sf(".\\geodata\\states_nad83\\states_NAD83.shp")

	# lakes
		lakes_sf <- read_sf(".\\geodata\\ne_10m_lakes\\ne_10m_lakes.shp")	

 	# reprojections
  		lakes_sf <- st_transform(lakes_sf, st_crs(states_sf))		
		rasr_predicted_sf <- st_transform(rasr_predicted_sf, st_crs(states_sf))


# FIGURES ---

	# 1° sub graph of the distribution of RASr per GEOID

		# subplot B Density plot of predicted RASR
				g_distrib <- ggplot_density(na.omit(rasr_predicted_sf$predict), xlabel ="Predicted RASR")
				g_distrib

		# categorize predicted RASR
			quantiles <- c(0, 0.1, 0.2, 0.3, .4, 0.5, 0.6, 0.7, .8, 0.9, 1)
			labels <- c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100")
			invasion_level_cat <- cut(rasr_predicted_sf$predict, breaks = quantiles, labels = labels, include.lowest = T)


		# subplot A : map itself

			gmap_pred_RASR <- ggplot(data = rasr_predicted_sf) +
				geom_sf( data = states_sf, fill = "gray80", color = "white", size = 0.1	) +	
				geom_sf( mapping = aes(fill = invasion_level_cat), color = "transparent", size = 0.05) +
				scale_alpha(name = "", range = c(0.75, 0), guide = "none") + # suppress legend
				scale_fill_viridis(
						option = "cividis",
						name = "RASR (%):\nRelative Alien\nSpecies Richness",
						alpha = 0.8, # make fill a bit brighter
						begin = 0.05, # truncate the color scale, so that extreme colors are not used
						end = .9, #.8 or .7  remove low color
						discrete = T, # discrete classes, thus guide_legend instead of _colorbar
						direction = -1, # dark is lowest, yellow is highest
						guide = guide_legend(
								keyheight = unit(3, units = "mm"),
								keywidth = unit(3, units = "mm"),
								title.position = "top",
								reverse = T) # display highest RASR on top
					) +
				xlim(-5000000,2500000) +
				geom_sf( data = lakes_sf, fill = "#D6F1FF", color = "transparent") +
				theme_map()




		# Figure 1 : map of predicted RASR

			ggdraw() +
					draw_plot(gmap_pred_RASR , 0, 0, 1, 1) +
					draw_plot(g_distrib , 0.07, 0.15, 0.22, .2) +
			  		draw_plot_label("A", .05, 0.85, size = 10) +
			 		draw_plot_label("B", .05, 0.35, size = 10)

			# I/O
	 			ggsave("Fig_1_map_RASR.pdf", device = cairo_pdf, width = 4.75, height = 4)





# uncertainity map ---

	predict_se_cat <- cut(rasr_predicted_sf$predict_se, breaks = quantiles, labels = labels, include.lowest = T)

	ggplot(data = rasr_predicted_sf) +
				geom_sf( data = states_sf, fill = "gray80", color = "white", size = 0.1	) +	
				#geom_sf( data = counties_full_sf, fill = "gray80", color = "gray80", size = 0.5	) +  # with Greenland
				geom_sf( mapping = aes(fill = predict_se_cat), color = "transparent", size = 0.05) +

				# use the "alpha hack" (as the "fill" aesthetse is already taken)
					scale_alpha(name = "", range = c(0.6, 0), guide = "none") + # suppress legend

				# use the Viridis color scale
					scale_fill_viridis(
						option = "cividis",
						name = "RASR  (%)\nPrediction Uncertainty",
						alpha = 0.9, # make fill a bit brighter
						begin = 0, # truncate the color scale, so that extreme colors are not used
						end = .8, #.8 or .7 
						discrete = T, # discrete classes, thus guide_legend instead of _colorbar
						direction = -1, # dark is lowest, yellow is highest
						guide = guide_legend(
								keyheight = unit(3, units = "mm"),
								keywidth = unit(3, units = "mm"),
								title.position = "top",
								reverse = T) # display highest RASR on top
					) +
				geom_sf( data = lakes_sf, fill = "#D6F1FF", color = "transparent") +
				xlim(-5000000,2500000) +
				#geom_sf(data = states_sf, fill = "transparent",	color = "white", size = 0.01	) +
				theme_map()

	ggsave("Fig_S10_map_RASR_uncertainty.pdf", device = cairo_pdf, width = 4.75, height = 3.8)


