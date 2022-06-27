# --- script to produce Fig. 5 : spatio temporal dynamics of alien earthworm species accross North America ---
# More info here: https://github.com/JeromeMathieuEcology/GlobalWorming.

# libraries
	library(ggplot2)
	library(grid)
	library(gridExtra)
	library(viridis)
	library(sf)
	library(cowplot)

# own functions
	source(".\\functions\\map_theme.r")
	source(".\\functions\\ggplot_density.r")
	source(".\\functions\\plot_ggmap.r")

# load data

	# spatial data

		# layer with TDWG4 units
			states_sf <- read_sf(".\\geodata\\states_nad83\\states_NAD83.shp")

		# whole continent, with Greenland	
			NA_continent_sf <- read_sf(".\\geodata\\land\\NA_land_NAD83.shp")
			NA_continent <- as_Spatial(NA_continent_sf)

		# lakes
			lakes_sf <- read_sf(".\\geodata\\ne_10m_lakes\\ne_10m_lakes.shp")
			# reprojection
  				lakes_sf <- st_transform(lakes_sf, st_crs(states_sf))	

	# earthworm data
		states_1920 <- read.csv2(".\\EWINA\\EWINA_RICH\\EWINA_TDWG4_earthworms_1920.csv")
		states_1940 <- read.csv2(".\\EWINA\\EWINA_RICH\\EWINA_TDWG4_earthworms_1940.csv")
		states_1960 <- read.csv2(".\\EWINA\\EWINA_RICH\\EWINA_TDWG4_earthworms_1960.csv")
		states_1980 <- read.csv2(".\\EWINA\\EWINA_RICH\\EWINA_TDWG4_earthworms_1980.csv")
		states_2000 <- read.csv2(".\\EWINA\\EWINA_RICH\\EWINA_TDWG4_earthworms_2000.csv")
		states_2021 <- read.csv2(".\\EWINA\\EWINA_RICH\\EWINA_TDWG4_earthworms_2021.csv")


# merge worm data to spatial layer
			states_1920_sf <- merge(states_sf, states_1920, by = "GEOID_st")
			states_1940_sf <- merge(states_sf, states_1940, by = "GEOID_st")
			states_1960_sf <- merge(states_sf, states_1960, by = "GEOID_st")
			states_1980_sf <- merge(states_sf, states_1980, by = "GEOID_st")	
			states_2000_sf <- merge(states_sf, states_2000, by = "GEOID_st")	
			states_2021_sf <- merge(states_sf, states_2021, by = "GEOID_st")


# categorize pr_exo RASR

		quantiles <- c(0, 1, 5, 10, 15, 20, 25, 30, 35)
		labels <- c("0","1-5","5-10","10-15","15-20","20-25","25-30","30-35")

		invasion_level_cat_1920 <- cut(states_1920_sf$exotic, breaks = quantiles, labels = labels, include.lowest = T)
		invasion_level_cat_1940 <- cut(states_1940_sf$exotic, breaks = quantiles, labels = labels, include.lowest = T)
		invasion_level_cat_1960 <- cut(states_1960_sf$exotic, breaks = quantiles, labels = labels, include.lowest = T)
		invasion_level_cat_1980 <- cut(states_1980_sf$exotic, breaks = quantiles, labels = labels, include.lowest = T)
		invasion_level_cat_2000 <- cut(states_2000_sf$exotic, breaks = quantiles, labels = labels, include.lowest = T)
		invasion_level_cat_2021 <- cut(states_2021_sf$exotic, breaks = quantiles, labels = labels, include.lowest = T)



# individual maps by date

		map_1920 <- plot_ggmap_history(layer = states_1920_sf, variable = invasion_level_cat_1920, labels = labels, title = "< 1920", position = "none")
		map_1940 <- plot_ggmap_history(layer = states_1940_sf, variable = invasion_level_cat_1940, labels = labels, title = "< 1940", position = "none")
		map_1960 <- plot_ggmap_history(layer = states_1960_sf, variable = invasion_level_cat_1960, labels = labels, title = "< 1960", position = "none")
		map_1980 <- plot_ggmap_history(layer = states_1980_sf, variable = invasion_level_cat_1980, labels = labels, title = "< 1980", position = "none")
		map_2000 <- plot_ggmap_history(layer = states_2000_sf, variable = invasion_level_cat_2000, labels = labels, title = "< 2000", position = "none")
		map_2021 <- plot_ggmap_history(layer = states_2021_sf, variable = invasion_level_cat_2021, labels = labels, title = "< 2021", position = "none")


# build a separate legend

		# dummy plot to get the legend at bottom
			map_legend <- plot_ggmap_history(layer = states_2000_sf, variable = invasion_level_cat_2000, 
				labels = labels, title = "Number of Alien Species", position = "bottom")

		# extract the legend
			legend <- cowplot::get_legend(map_legend)
		
		# adjust the position of the legend
			legend$vp$x <- unit(0.5, 'npc')	
			legend$vp$y <- unit(0.05, 'npc')


# make the figure

		cowplot::plot_grid(map_1920, map_1940, map_1960, map_1980, map_2000, map_2021, NULL, NULL, NULL, rel_heights = c(6,6,1),
	    	ncol = 3, labels = c('1920','1940',"1960", "1980", "2000", "2021"), hjust = 0.5, label_x = .58) +
			draw_plot(legend , 0.35, 0.05, 0.3, .25) 


# I/O
	ggsave("Fig_5_states_history_n_aliens.pdf", device = cairo_pdf, width = 10, height = 7)





