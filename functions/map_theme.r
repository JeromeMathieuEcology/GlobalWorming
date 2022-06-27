# custom theme

	 	# based on https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/
		
		theme_map <- function(...) {
		  theme_minimal() +
		  theme(
		    # remove all axes
			    axis.line = element_blank(),
			    axis.text = element_blank(),
			    #axis.text.y = element_blank(),
			    axis.ticks = element_blank(),
			    axis.title = element_blank(),
		    # grids
		   		panel.grid = element_blank(),			    
		    # borders and margins
		    	plot.margin = unit(c(0, 0.2, 0, 0), "cm"),
		    	panel.border = element_blank(),
		    	#panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
		    # text
		   	 	legend.title = element_text(size = 7),
		    	legend.text = element_text(size = 6, hjust = 0), #,color = default_font_color)	   
		  )
		}
