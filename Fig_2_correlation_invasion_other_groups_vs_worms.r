# --- script to produce fig. 2 : comparison of worm and aboveground alien species richness in TDWG units ---
# More info here: https://github.com/JeromeMathieuEcology/GlobalWorming.

# libraries
	library(sf)
	library(geojsonio)

	library(cowplot) # stack ggplots
	library(Cairo)
	library(viridis)
	library(scales)

	library(png)
	library(ggplot2)
	library(ggExtra)

# specific function for the plot
	source(".\\functions\\plot_correl.r")


# load and prep data data

	# load layer with Dawson invasive data and worm RASR (from script 13)
		states_inv_sf <- geojsonio::geojson_sf(".\\database\\EWINA_RICH\\EWINA_TDWG4_aboveground.geojson")
		
	# load worm data 
		obs_rasr <- read.csv2(".\\EWINA\\EWINA_RICH\\EWINA_TDWG4_earthworms_2021.csv")
	
	# merge worm data to aboveground invasive taxa
		states_inv_rasr_sf <- merge (states_inv_sf, obs_rasr, by = "GEOID_st")

	# make sure variables are numeric
			states_inv_rasr_sf$Plants <- as.numeric(states_inv_rasr_sf$Plants)
			states_inv_rasr_sf$Spiders <- as.numeric(states_inv_rasr_sf$Spiders)
			states_inv_rasr_sf$Mammals <- as.numeric(states_inv_rasr_sf$Mammals)
			states_inv_rasr_sf$Birds <- as.numeric(states_inv_rasr_sf$Birds)


# correlation test for each group 
		cor.test(states_inv_rasr_sf$nsp_alien, states_inv_rasr_sf$Plants, use ="complete.obs", method = "spearman") 	# 0.77
		cor.test(states_inv_rasr_sf$nsp_alien, states_inv_rasr_sf$Spiders, use ="complete.obs", method = "spearman")	# .73
		cor.test(states_inv_rasr_sf$nsp_alien, states_inv_rasr_sf$Mammals, use ="complete.obs", method = "spearman")	# 0.42
		cor.test(states_inv_rasr_sf$nsp_alien, states_inv_rasr_sf$Birds, use ="complete.obs", method = "spearman")		# 0.62


# correlation plot for each group  --------------------------------------

	plants_fig <- readPNG(".\\images\\plants.png")
	spiders_fig <- readPNG(".\\images\\spider.png")
	birds_fig <- readPNG(".\\images\\bird.png")
	mammals_fig <- readPNG(".\\images\\mammal.png")
	worm_fig <- readPNG(".\\images\\worm.png")


	# PLANTS ---------------

		points_plants <- get_reg_points(x = states_inv_rasr_sf$Plants, y = states_inv_rasr_sf$nsp_alien)

		g_correl_plants <- plot_correl(states_inv_rasr_sf, states_inv_rasr_sf$Plants, "Plants", points_plants) #, plant_breaks, plant_breaks_labels
		g_correl_plants_fig <- ggdraw() +  
						draw_plot(g_correl_plants, 0, 0, 1, 1) +
						draw_image(plants_fig,  0.77, .3, .23, .20) +
						#draw_image(worm_fig,  0.18, .8, .15, .15) +
						annotate(geom = "text", x = .28, y = .88, label = paste("italic(r)==","italic('.77')~',' * ~italic('  p<2.2e-16')"), parse = TRUE, hjust = 0, size = 3, col = "grey40")


	# SPIDERS ---------------

		points_spiders <- get_reg_points(x = states_inv_rasr_sf$Spiders, y = states_inv_rasr_sf$nsp_alien)

		g_correl_spiders <- plot_correl(states_inv_rasr_sf, states_inv_rasr_sf$Spiders, "Spiders", points_spiders) #, states_inv_rasr_sf$fill_birds,birds_breaks,, birds_breaks_labels
		g_correl_spiders_fig <- ggdraw() +  
							draw_plot(g_correl_spiders, 0, 0, 1, 1) +
							draw_image(spiders_fig,  0.75, .23, .22, .22) +
							#draw_image(worm_fig,  0.18, .8, .15, .15) +
							annotate(geom = "text", x = .28, y = .88, label = paste("italic(r)==","italic('.73')~',' * ~italic(' p= 2.2e-16')"), parse=TRUE, hjust = 0, size = 3, col = "grey40")


	# MAMMALS ---------------

		points_mammals <- get_reg_points(x = states_inv_rasr_sf$Mammals, y = states_inv_rasr_sf$nsp_alien)

		g_correl_mammals <- plot_correl(states_inv_rasr_sf, states_inv_rasr_sf$Mammals, "Mammals", points_mammals) #, states_inv_rasr_sf$fill_birds,birds_breaks,, birds_breaks_labels
		#g_correl_mammals <- plot_correl(states_inv_rasr_sf, states_inv_rasr_sf$Mammals, states_inv_rasr_sf$fill_mammals, "Mammals", mammals_breaks,mammals_breaks_labels)
		g_correl_mammals_fig <- ggdraw() +  
								draw_plot(g_correl_mammals, 0, 0, 1, 1) +
								draw_image(mammals_fig,  0.7, .22, .3, .3) +
								#draw_image(worm_fig,  0.18, .8, .15, .15) +
								annotate('text', x = .28, y = .88, label = paste("italic(r)==","italic('.42')~',' * ~italic(' p= 1.4e-07')"), parse=TRUE, hjust = 0, size = 3, col = "grey40")

	# BIRDS ---------------


		points_birds <- get_reg_points(x = states_inv_rasr_sf$Birds, y = states_inv_rasr_sf$nsp_alien)

		g_correl_birds <- plot_correl(states_inv_rasr_sf, states_inv_rasr_sf$Birds, "Birds", points_birds) #, states_inv_rasr_sf$fill_birds,birds_breaks,, birds_breaks_labels
		g_correl_birds_fig <- ggdraw() +  
						draw_plot(g_correl_birds, 0, 0, 1, 1) +
						draw_image(birds_fig,  0.79, .28, .16, .16) +
						#draw_image(worm_fig,  0.18, .8, .15, .15) +
						annotate(geom = "text", x = .28, y = .88, label = paste("italic(r)==","italic('.62')~',' * ~italic(' p=5.7e-13')"), parse=TRUE, hjust = 0, size = 3, col = "grey40")
	
# complete figure

	cowplot::plot_grid(g_correl_plants_fig, g_correl_spiders_fig, g_correl_mammals_fig, g_correl_birds_fig,  ncol = 2, labels = c('A','B',"C",'D'))

	ggsave("Fig_2_correl_nsp_exo_vt_other_groups.pdf", device = cairo_pdf, width = 4.75, height = 4.75)









