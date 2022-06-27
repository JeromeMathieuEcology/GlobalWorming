

plot_ggmap <- function (layer, variable, title){

	ggplot(data = layer) +
				geom_sf( data = NA_continent_sf, fill = "gray80", color = "white", size = 0.5) +
				geom_sf( data = states_sf, fill = "gray80", color = "white", size = 0.5	) +
				geom_sf( mapping = aes(fill = variable), color = "transparent", size = 0.05) +

				# use the "alpha hack" (as the "fill" aesthetic is already taken)
					scale_alpha(name = "", range = c(0.6, 0), guide = F) + # suppress legend

				# use the Viridis color scale
					scale_fill_viridis(
						option = "cividis",
						name = title,
						alpha = 0.9, # make fill a bit brighter
						begin = 0, # with this we can truncate the color scale, so that extreme colors  are not used
						end = .8,
						discrete = T, # discrete classes, thus guide_legend instead of _colorbar
						direction = -1, # dark is lowest, yellow is highest
						guide = guide_legend(
									keyheight = unit(5, units = "mm"),
									title.position = "top",
									reverse = F) # T : display highest RASR on top/beginning
					) +
				geom_sf( data = lakes_sf, fill = "#D6F1FF", color = "transparent") +
				#geom_sf(data = states_sf, fill = "transparent",	color = "white", size = 0.01	) +
				theme_map() +
				theme(legend.position = "bottom")



}




plot_ggmap_history <- function (layer, variable, labels, title, position = "bottom"){

	ggplot(data = layer) +
				geom_sf( data = NA_continent_sf, fill = "gray80", color = "white", size = 0.5) +
				geom_sf( data = states_sf, fill = "gray80", color = "white", size = 0.1	) +
				geom_sf( mapping = aes(fill = variable), color = "transparent", size = 0.05) +

				# use the "alpha hack" (as the "fill" aesthetic is already taken)
					scale_alpha(name = "", range = c(0.6, 0), guide = "none") + # suppress legend

				# use the Viridis color scale
					scale_fill_viridis(
						option = "cividis",
						name = title,
						alpha = 0.9, # make fill a bit brighter
						begin = 0, # with this we can truncate the color scale, so that extreme colors  are not used
						end = .85,
						discrete = T, # discrete classes, thus guide_legend instead of _colorbar
						direction = -1, # dark is lowest, yellow is highest
						limits = labels,
						guide = guide_legend(
									keyheight = unit(5, units = "mm"),
									title.position = "top",
									title.hjust = .5, # center horizontaly the title of the legende
									nrow = 1,	# legend in 1 row only
									reverse = F) # T : display highest RASR on top/beginning
					) +
				xlim(-5000000,2500000) +
				geom_sf( data = lakes_sf, fill = "#D6F1FF", color = "transparent") +
				#geom_sf(data = states_sf, fill = "transparent",	color = "white", size = 0.01	) +
				theme_map() +
				theme(legend.position = position)

}
