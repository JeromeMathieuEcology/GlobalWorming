	# function to perform orthogonal regression and get min and max point to draw the regression line on a plot

		get_reg_points <- function(x,y) {

			# prep df
				data_reg <- na.omit(data.frame(x = x, y = y))

			# orthogonal regression
				dem_reg <- SimplyAgree::dem_reg( "x", "y", data = data_reg)

				points_reg <- data.frame( x = c(min(data_reg$x) , max(data_reg$x)),
												y = c( dem_reg$model$coef[1] + dem_reg$model$coef[2] * min(data_reg$x),
					   									dem_reg$model$coef[1] + dem_reg$model$coef[2] * max(data_reg$x) ) )
		return(points_reg)

		}


	# function to do subplot correlation  between worm species richness and other taxas

		plot_correl <- function(df, x, label_taxa, points_reg){
		#plot_correl <- function(df, x, col_taxa, label_taxa, group_breaks, group_breaks_labels){
			# df : dataframe with data
			# x: group species richness
			# col_taxa : color of each data
			# label_taxa : label for x axis legend
					


		 			ggplot(df, aes(x = x, y = nsp_alien)) + 
		                    
		                    geom_point(color = "grey20", size = .8) +
		                    geom_line(data = points_reg, aes(x = x , y = y), col= "grey40", linetype = 'dashed') +

		                    xlab(paste0("Species Richness\nof Alien ", label_taxa)) +
		                    ylab("Species Richness\nof Alien Earthworms") +

		                    removeGrid() +
		                    coord_cartesian(xlim = c(max(min(x, na.rm=T)-1,1), max(x, na.rm=T)+1), ylim = c(0, 35)) +

		                    theme(plot.background = element_rect(fill = "white"),
		                        	panel.background = element_rect(fill ="white"),
		                        	panel.border = element_rect(fill = NA, color = "white", size = 0.5, linetype = "solid"),
		                        	panel.grid.major = element_line(size = 0.5, linetype = 5 , colour = "grey60"),
		                        	panel.grid.minor = element_blank(),
		                        	axis.title = element_text( colour = "grey30", size = 10, margin = margin( t = 1)),
		                        	axis.line = element_line(colour = "grey30", size = .8, linetype = "solid"),
		                        	axis.ticks = element_blank(), 
		                        	axis.text = element_text(color = "grey30", size = 10),
		                        	axis.text.y  = element_text(hjust = 1)
		          				)
		}
