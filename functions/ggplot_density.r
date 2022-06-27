# make a density histogram with vertical areas along the X values

library(tidyverse)

	ggplot_density = function(x, adj = 1, drop_levs = F, xlabel = "Label") {

						# Calculate density values for input data
								dens = data.frame(density(x, n=2^10)[c("x","y")]) %>% 
							    mutate(section = cut(x, breaks=c(0, .3, .4, .5,.6 ,.7 ,.8, .9, 1.1))) %>% 						    
							    group_by(section) %>% 
							    mutate(prob = paste0(round(sum(y) * mean(diff(x)) * 100),"%"))


						# Get probability mass for each level of section
						# We'll use these as the label values in scale_fill_manual
							sp = dens %>% 
							    	group_by(section, prob) %>% 
							    	summarise %>% 
							    	ungroup

							if(!drop_levs) {
							   sp = sp %>% complete(section, fill = list(prob="0%"))
							  }


						# Assign colors to each level of section
						  	col = setNames(rev(viridis_pal(option = "cividis", begin = 0.05, end = .9, alpha = 0.9,)(8)), levels(dens$section))


						ggplot(dens, aes(x, y, fill = section)) +
						    geom_area() +
						    scale_fill_manual(labels = sp$prob, values = col, drop = drop_levs) +
						    labs(fill="") +
						    xlab(xlabel) +
						    scale_x_continuous( breaks = c(0, .25, .5, .75, 1), labels = c("0", "0.25", "0.5", "0.75","1"), limits = c(0,1) ) +
						    scale_alpha(name = "", range = c(0.5, 0), guide = "none") + # suppress legend

						    #xlim(0, 1) +						    
						    theme(legend.position = "none",
						    	panel.grid = element_blank(),
						    	panel.background = element_rect(fill = "transparent"), 				# bg of the panel
						    	plot.background = element_rect(fill = "transparent", color = NA), 	# bg of the plot
						    	axis.text.y = element_blank(),
								axis.ticks = element_blank(),
								axis.title.y = element_blank(),
								axis.text = element_text(size = 6, color = "black"),
								axis.title.x = element_text( size = 7), # color = "#939184",family= "Georgia",
								axis.text.x.bottom = element_text(margin = margin(t = 0,b = 1, unit = "mm"))
				    			)
					    	
	}


    
