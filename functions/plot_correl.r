	# function to do subplot correlation between worm species richness and other taxas

		plot_correl <- function(df, x, label_taxa){
		#plot_correl <- function(df, x, col_taxa, label_taxa, group_breaks, group_breaks_labels){
			# df : dataframe with data
			# x: group species richness
			# col_taxa : color of each data
			# label_taxa : label for x axis legend
					
		 			ggplot(df, aes(x = x, y = nsp_alien)) + 
		                    #geom_vline(xintercept = group_breaks[-1], size = 0.5, linetype = 5 , colour = "grey60") +
		                    #geom_hline(yintercept = predict_breaks[-1], size = 0.5, linetype = 5 , colour = "grey60") + 	
		                    #geom_point(color = col_taxa) +
		                    geom_point(color = "grey20", size = .8) +
		                   # geom_smooth(method="lm", color = "grey40", se = F, linetype = "dashed", size = 1) +
		                    xlab(paste0("Species Richness\nof Alien ", label_taxa)) +
		                    ylab("Species Richness\nof Alien Earthworms") +
		                    removeGrid() +
		                    scale_x_continuous(trans = log10_trans(), 
		                    					#breaks = group_breaks_labels,
		                    					 limits = c(max(min(x, na.rm=T)-1,1), max(x, na.rm=T)+1)
		                    					) + #)
		                    					 #
  							scale_y_continuous( #breaks = nsp_exo_breaks_labels, 
  												limits = c(0, 35)) + #trans = 'log10'
  							annotation_logticks(sides = "b")  +
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
