# --- script to produce Fig. 6 and Extended Fig. S2 ---
# More info here: https://github.com/JeromeMathieuEcology/GlobalWorming.

library(ggplot2)
library(Cairo)
library(colorspace)
library(cowplot)
library(grid)
library(ggpmisc)
library(sf)
library(viridis)

# read data

		# tdwg4 units with worm functional data
				tdwg4_rasr <- read.csv2( ".\\EWINA\\EWINA_RICH\\EWINA_TDWG4_fun.csv")

		# states
				states_sf <- read_sf(".\\geodata\\states_nad83\\states_NAD83.shp")
		# lakes
				lakes_sf <- read_sf(".\\geodata\\ne_10m_lakes\\ne_10m_lakes.shp")
				lakes_sf <- st_transform(lakes_sf, st_crs(states_sf))

		# species profile
				sp_profile <- read.csv2(".\\EWINA\\EWINA_SPECIES\\EWINA_SP.csv",sep=",",dec=".")


# prep data

		# species profile

		    # capitalize first letter of species' names
		        sp_profile$species_name <- as.character(sp_profile$species_name)
		        sp_profile$species_name <- paste(toupper(substr(sp_profile$species_name, 1, 1)), substr(sp_profile$species_name, 2, nchar(sp_profile$species_name)), sep="")
				
			# order of species
				sp_profile <- sp_profile[order(sp_profile$freq, decreasing = F),]
				sp_profile$species_name <- factor(sp_profile$species_name, levels = sp_profile$species_name[order(sp_profile$freq, decreasing = F)])

			# color of species					
				colors_sp <- c("#6c2323","#476c23") #exotic native

				sp_profile$sp_col <- ifelse(sp_profile$exotic=="yes", colors_sp[1],colors_sp[2])		
					

			# side of bar according to origin
				   sp_profile$freq2 <- ifelse(sp_profile$exotic=="yes", sp_profile$freq +.01, -1*sp_profile$freq-.01)

		# merge worm data to stats
			RASR_lv4_sf <- merge(states_sf, tdwg4_rasr, by = "GEOID_st")


	# subset
	 	nsp_tot <- nrow(sp_profile)
	  	nb_sp <- 15
		sp_profile_120 <- sp_profile[nsp_tot:(nsp_tot-nb_sp+1),]

		sp_profile_120$species_name <- as.character(sp_profile_120$species_name)
		sp_profile_120$species_name <- gsub("Bimastos rubidus","Bimastos rubidus *  \u036B", sp_profile_120$species_name)
		sp_profile_120$species_name <- gsub("Diplocardia caroliniana","Diplocardia caroliniana \u0368  \u036B", sp_profile_120$species_name)
		sp_profile_120$species_name <- gsub("Sparganophilus tamesis","Sparganophilus tamesis  \u036B" , sp_profile_120$species_name)
		sp_profile_120$species_name <- gsub("Bimastos tumidus","Bimastos tumidus \u0368", sp_profile_120$species_name)

		sp_profile_120$species_name <- as.factor(sp_profile_120$species_name )



# --- PLOTS --------------------


	# Fig. S2 : species geographical range

		# zoom polygon coordinates
			zzoom <- data.frame( y = c(-.005, -.005, .005, .55, .55, .05,  .05, .005, -.005),
		                     	 x = c(  nsp_tot- nb_sp-0.5,   nsp_tot, nsp_tot,  236,   16, 16, 236 , nsp_tot-nb_sp-0.5,   nsp_tot-nb_sp-0.5)
		                     )
			backgrd <- data.frame(	y=c
				(0.53, 0.532, 0.07, 0.07),
									x=c(225,  23,   23,  225))

			g_big <- ggplot(sp_profile, aes(x = species_name, y = freq2)) +
							geom_segment(aes(x = species_name, y = 0, xend = species_name, yend = freq2), color = sp_profile$sp_col, size = .1) +
							geom_polygon(data = zzoom,aes(x = x, y = y), fill = "#B0A473FF", alpha = .6) + #
							geom_polygon(data = backgrd, aes(x=x, y= y), fill = "white") +
							scale_y_continuous(breaks = c(-0.25,0 ,0.25 ,0.5,0.75), labels = c(25, 0, 25, 50 ,"75%"), limits = c(-0.45, 0.8)) +			
							coord_flip() +
					    theme_minimal() +
					    labs(title = "", y = "Geographical range") +
		 
							annotate(geom = "text", y = rep(.3, 15),   x = seq(32, 214,13), label = rev(sp_profile_120$species_name), fontface = "italic", colour = rev(sp_profile_120$sp_col), size = 3) +

					       	theme(	plot.title = element_text(size = 16, color = "grey30", hjust=.15, margin = margin(b = 3, t = 10, l = 50)),
						        	axis.title.y = element_blank(),
						        	axis.title.x = element_text(size = 18, ,color = "grey30", hjust = .18), #family = "Georgia"						        	
					        		axis.text.y = element_blank(),	
				        	  		axis.text.x = element_text(size = 16, margin = margin(b = 3, t = 2)),				        					        	
					        		panel.grid.minor.y = element_blank(),
					        		panel.grid.major.y = element_blank(),
					        		panel.grid.minor.x = element_blank(),
					        		plot.margin = margin(t = 20, r = 0, b = 15, l = 0, unit = "pt"),
					        		text = element_text(family="Helvetica")
					        	  )
			g1 <- ggdraw() +
			  		draw_plot(g_big , 0, 0, .95, 1) +
			  	  	draw_plot_label("Top 15 Species", .3, 0.725, size = 12, color = "grey25") +
			  	  	draw_plot_label(c("Aliens", "Natives"), c(.35, 0.05 ), c(.95, 0.95), size = 18, color = colors_sp )	 +
			  	    theme(plot.margin = margin(0,0,0,0) )  	  

		 	g1

		ggsave("Fig_S2_species_frequency.pdf", device = cairo_pdf, width = 4.75, height = 5)



	# --- FIG.6 ---

		#	prep species'category ~ exotic vs native -------------------------------------------------------------

			# clean categ
					sp_profile$categ <- as.character(sp_profile$categ)
					sp_profile$categ <- gsub("Epigeic-limicolous","Limicolous",sp_profile$categ)
					sp_profile$categ <- gsub("epigeic","Epigeic",sp_profile$categ)
					sp_profile$categ <- gsub("Endo-anecic","Anecic",sp_profile$categ)
					sp_profile$categ <- gsub("Corticole-Epigeic","Corticole",sp_profile$categ)
					sp_profile$categ <- gsub("Epi-endogeic","Epi-Endogeic",sp_profile$categ)

					sp_profile$categ2 <- sp_profile$categ
					sp_profile$categ2 <- gsub("Epigeic","Litter feeders",sp_profile$categ2)
					sp_profile$categ2 <- gsub("Epi-Endogeic","Litter feeders",sp_profile$categ2)
					sp_profile$categ2 <- gsub("Anecic","Litter feeders",sp_profile$categ2)
					sp_profile$categ2 <- gsub("Endogeic","Soil feeders",sp_profile$categ2)
					sp_profile$categ2 <- gsub("Limicolous","Semi aquatic",sp_profile$categ2)
					sp_profile$categ2 <- gsub("Corticole","Tree inhabitants",sp_profile$categ2)		

			 # contingency table
					table_co2 <- as.data.frame.matrix( table(sp_profile$categ2,sp_profile$exotic) )
					table_co2$categ2 <- row.names(table_co2)

			# tranform in long format
					table_co_long2 <- reshape2::melt(table_co2,id.vars=c("categ2"),variable="exotic",value="Nbsp")

			# remove species with unknown categ2
					table_co_long2 <- table_co_long2[table_co_long2$categ2 !="",]

			# reorder categ2s
				table_co_long2$categ2 <- factor(table_co_long2$categ2, 
					levels=rev(c("Soil feeders","Litter feeders","Semi aquatic","Tree inhabitants" )))

			# side of bars according to origin
				table_co_long2$nbsp <- ifelse(	table_co_long2$exotic=="no",-table_co_long2$value,table_co_long2$value)
			
			# color according to origin
				table_co_long2$categ2_col <- ifelse(table_co_long2$exotic=="yes", colors_sp[1], colors_sp[2])

			table_co_long2 <- na.omit(table_co_long2)



		# Fig.6A : plot of diet ~ origin 

			g_diet <- ggplot(table_co_long2,aes(x=categ2,y=nbsp)) +

					geom_segment(aes(x = categ2, y = 0, xend = categ2, yend = nbsp), color = table_co_long2$categ2_col, size = 1.5, lineend = "round") +

					geom_segment(aes(x =  .5, y = 0, xend =  .8, yend = 0), color ="grey80", size = 1) +
					geom_segment(aes(x = 1.5, y = 0, xend = 1.75, yend = 0), color ="grey80", size = 1) +
					geom_segment(aes(x = 2.5, y = 0, xend = 2.75, yend = 0), color ="grey80", size = 1) +
					geom_segment(aes(x = 3.5, y = 0, xend = 3.75, yend = 0), color ="grey80", size = 1) +
					geom_segment(aes(x = 4.5, y = 0, xend = 5.1, yend = 0), color ="grey80", size = 1) +

					coord_flip() +			
					theme_minimal() +
					labs(y = "Number of Species") +
					labs(x = "") +	 

					# alien vs exotic				               								
					    annotation_custom(textGrob('Natives', gp = gpar(col = colors_sp[2], fontface = 'bold', fontsize = 8)), 
							                   									xmin = 4.9, xmax = 5, ymin = -55, ymax = -25) +		
							                   											
						annotation_custom(textGrob('Aliens', gp = gpar(col = colors_sp[1], fontface = 'bold', fontsize = 8)), 
							        	 										xmin = 4.9, xmax = 5, ymin = 28, ymax = 40) +
					#  diets
					    annotation_custom(textGrob('Soil feeders', gp = gpar(col = 'grey30', fontface = 'bold', fontsize = 7)), 
							                   									xmin = 3.7, xmax = 5, ymin =-20, ymax = 20) +

					    annotation_custom(textGrob("Litter feeders", gp = gpar(col = 'grey30', fontface = 'bold', fontsize = 7)), 
							                   									xmin = 2.65, xmax = 3.9, ymin = -20, ymax = 20) +

					    annotation_custom(textGrob("Tree inhabitants", gp = gpar(col = 'grey30', fontface = 'bold', fontsize = 7)), 
							                   									xmin = 1.7, xmax = 3, ymin = -20, ymax = 20) +

					    annotation_custom(textGrob("Semi aquatic", gp = gpar(col = 'grey30', fontface = 'bold', fontsize = 7)), 
							                   									xmin = 0.7, xmax = 2, ymin = -20, ymax = 20) +

					scale_x_discrete(expand = c(mult = c(0, 0), add = c(0,0)) ) +
					scale_y_continuous(breaks = c(-150,-100,-50,0,50), labels = c(150,100,50,0,50), limits = c (-180, 50) ) +

					theme(#axis.title = element_blank(),
							    axis.text.y = element_blank(),
							    axis.text.x = element_text(size = 6),			        	  
							    plot.title = element_text(size = 7, color = "grey30"), #hjust=.15, margin = margin(b=3,t = 10)),
							    axis.title.x = element_text(size = 7.5, color = "grey30", hjust = .82, margin = margin(b=3,t = 5)),
							    panel.grid.minor = element_blank(),
							    panel.grid.major.y = element_blank(),
							    plot.margin = margin(t = 20, r = 5, b = 5, l = 0, unit = "pt") )



		# Fig. 6B : map of functionnal enrichement
		
			# categorize new_feed

			    quantiles_new_feed <- c(0,.5 ,1.5,2.5,3.5)
			    labels_new_feed <- c("No enrichment","Litter Feeders","Soil Feeders","Litter & Soil Feeders")
			    new_feed_level_cat <- cut(RASR_lv4_sf$new_feed, breaks = quantiles_new_feed, labels = labels_new_feed, include.lowest = T)

			    my_pal <- c("#ffcb69","#A5A58D","#CB997E","#997b66")

			# map
			    map_new_feed <-	ggplot( data = RASR_lv4_sf ) +     
			        					geom_sf( data = states_sf, fill = "gray80", color = "transparent", size = 0) +
			                        	geom_sf( aes(fill = new_feed_level_cat), color = "white", size = .005) +
			                        	scale_alpha(name = "", range = c(0.6, 0), guide = "none") + # suppress legend
			                        	scale_fill_manual(values = my_pal,
			                                      name = "Functionnal\nenrichment",
			                                      #alpha = 0.9, # make fill a bit brighter
			                                      na.translate = F ,
																						guide = guide_legend(
																								keyheight = unit(3, units = "mm"),
																								keywidth = unit(3, units = "mm"),
																								title.position = "top",
																								reverse = F) # display highest RASR on top
			                                    ) + 
			                        	xlim(-5000000,2500000) +                               
			                        	geom_sf( data = lakes_sf, fill = "#D6F1FF", color = "transparent") +	
			                        	theme_map() +
			                        	theme(legend.text = element_text(size = 6), legend.title = element_text(size = 8))




	# Fig.6 complete : COMPOSITE FIGURE with map

			g_frame <- ggplot() +
			            theme(
			            		plot.background = element_rect(fill="white"),
			                	panel.background = element_rect(fill="white")
			                )

			ggdraw() +
			        draw_plot(g_frame) +   
			        draw_plot(map_new_feed , 0.3, 0, .7, 1) +        
			        draw_plot(g_diet , 0, 0, .38, 1) +
			        annotate( geom = "text", x = .02, y=.92, label ="A", colour="grey20", size = 4 )  +
			        annotate( geom = "text", x = .4, y=.92, label ="B", colour="grey20", size = 4 )         
			                         

			ggsave("Fig_6_functional_shift.pdf", device = cairo_pdf, width = 4.75, height = 2) 