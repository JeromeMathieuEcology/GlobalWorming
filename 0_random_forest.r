# --- script to fit the random model, make predictions and compute diagonistics | Extended figures  4,5,8,9 ---
# More info here: https://github.com/JeromeMathieuEcology/GlobalWorming.

# libraries
	library(tidyverse)

	library(sf)
	library(geojsonsf)

	library(rfUtilities)
	library(ranger)	# random forest
	library(vip)
	library(pdp) # partial dependence  and ICE plots
	library(sp)
	library(spdep)

	library(ggplot2)


#source(".\\functions\\rdm_forest_functions.r")
source(".\\functions\\ggplot_pdp.r")


# load & prep data ------

	# background layer of counties with env data
		
		# read file and clean geometry
			counties_env_sf <- geojson_sf(".//EWINA//EWINA_RICH//EWINA_counties.geojson") %>% st_make_valid(counties_env_sf)

		# remove empty geometries
			counties_env_sf_ne <- counties_env_sf %>% filter(!st_is_empty(.))

			# transfo into spatial dataframe
				counties_env_sp <- as(counties_env_sf_ne, "Spatial")

			summary(counties_env_sp)
			dim(counties_env_sp)
			# 3459 15


	# worm and env. data (Observed RASR)

		obs_rasr <- read.csv2(".//EWINA//EWINA_RICH//EWINA_2000_counties_obs.csv")

		# clean
			obs_rasr$rasr <- as.numeric(obs_rasr$rasr)
			obs_rasr$area <- as.numeric(obs_rasr$area)
			obs_rasr$area_log <- as.numeric(obs_rasr$area_log)

			# remove greenland
				obs_rasr <- obs_rasr[!(obs_rasr$GEOID%in%c("G991","G992","G993","G994","G995")),]

			# remove counties with less than 5 species
				obs_rasr <- obs_rasr[obs_rasr$nsp>5,]

	# prep data for modeling --

			# select relevant variabless
				obs_rasr <- obs_rasr[,c("GEOID","rasr",
												"area_log","roads_log",
												"bio1", "bio12",
												"hii", 
												"grassland", "cropland",
												"ph_10","C0","C30",
												"alti","alti_sd")]



			# remove counties with NA in env or rasr data
				idx_not_na <- apply(apply(obs_rasr,2,is.na),1,sum)<1
				obs_rasr_nona <- obs_rasr[idx_not_na,]
				obs_rasr_nona <- obs_rasr[idx_not_na,]	



			# define X and Y
				Y <- obs_rasr_nona$rasr
				X <- obs_rasr_nona[, -grep("rasr", names(obs_rasr_nona) )]
				X <- X[, -grep("GEOID", names(X) )]


			# construct df	
				XY <- cbind(Y,X)
				XY_df <- as.data.frame(XY)

				XY_df <- map_dfr(XY_df,as.numeric)
				XY_df <- as.data.frame(XY_df)



		# custom weights to resample more often low values of RASR
				my_strata <- ifelse (XY_df$Y < .45,2,1) 
				table(my_strata)
				my_weights <- my_strata/sum(my_strata)


		# multicolinearity test (using qr-matrix decomposition )
			rfUtilities::multi.collinear(as.data.frame(X), perm = TRUE, p=0.05)


# RANDOM FOREST -------------------------------------------------

		# fit
				set.seed(2020)
				rdm_forest <- ranger(Y~., data = XY_df, case.weights = my_weights , importance = "permutation", keep.inbag = TRUE, quantreg = TRUE , mtry = dim(XY_df)[2]-1) #impurity_corrected
		
				rdm_forest

		# variable importance
						vips <- as.data.frame(sort(importance(rdm_forest), decreasing = T))#[1:10]
						names(vips) <- "vip"
						vips$var <- as.factor(row.names(vips))
	
				# clean names for the plot
						vips$var <- gsub("dist2ice","Dist. to GLM Ice Sheet", vips$var)
						vips$var <- gsub("alti_sd","Elevation heterogeneity", vips$var)
						vips$var <- gsub("d2coast","Dist. to Oceanic Coast", vips$var)
						vips$var <- gsub("C30","Carbon at 30 cm depth", vips$var)
						vips$var <- gsub("C0","Carbon at Soil Surface", vips$var)	
						vips$var <- gsub("ph_10","pH at 10 cm depth", vips$var)					
						vips$var <- gsub("alti_sd","Elevation sd", vips$var)
						vips$var <- gsub("prec_temp","bio1 x bio12", vips$var)
						vips$var <- gsub("alti","Elevation", vips$var)
						vips$var <- gsub("hii","Human Impact Index", vips$var)
						vips$var <- gsub("roads_log","Roads Density", vips$var)
						vips$var <- gsub("area_log","Area of Geo. Unit", vips$var)
						vips$var <- gsub("grassland","% of Area in Grassland", vips$var)
						vips$var <- gsub("cropland","% of Area in Cropland", vips$var)
						vips$var <- gsub("farming","% of Area Farmed", vips$var)						
						vips$var <- gsub("riv_dens","River Density", vips$var)
						vips$var <- gsub("bio12","Annual Precipitation", vips$var)
						vips$var <- gsub("bio1","Mean Annual temperature", vips$var)						
						

						vips$var <- forcats::fct_reorder(vips$var, vips$vip)

		# --- Extended data Figure 4 / Variable importance ---

						ggplot(data = vips, aes(x = var, y = vip)) + 
									geom_hline(yintercept = 0, col= "grey90", size = .5) +							
									geom_segment( aes(xend = var, yend = 0), col= "grey65") +				
									geom_point(size = 1.5) +
									coord_flip() +
									theme_minimal() +
									ylim(0,.021) +
									theme(axis.text = element_text(size = 5),
											panel.grid.minor = element_blank(),	
											panel.grid.major.x = element_line(size = 0.2, linetype = 2),												
											axis.title = element_blank())



		# predictions on training data

				# predict
					
						rdm_forest_fit <- predict(rdm_forest, X)$predictions

						# df with predicted rasr in observed locations
							fit <- data.frame(GEOID = obs_rasr$GEOID, fit = rdm_forest_fit)

						# dataframe with obs and predicted rasr in observed locations
							obs_and_pred_rasr_df <- data.frame(obs_rasr, pred_rasr = rdm_forest_fit)


		# --- Extend data Figure 8 / predicted vs observed RASR ---
				dev.off()

				pdf("Fig_S8_predicted_vs_observed_RASR.pdf", width = 4.75, height = 4.75 , colormodel = "cmyk")

						par(pty="s")
						plot(pred_rasr ~ rasr, data = obs_and_pred_rasr_df, pch = 19, cex = .1, col = "grey30", xlab = "Observed RASR", ylab = "Predicted RASR",
											 las = 1, xlim = c(0,1), ylim = c(0,1))
						abline(0,1,col="gray60", lwd=2)
						points(pred_rasr ~ rasr, data = obs_and_pred_rasr_df, pch = 19, cex = .15, col = "grey30")

				dev.off()




		# --- Extended Data Figure 5 / c-ICE partial dependance plots ---

					g_ice_bio1 <- ggplot_ice (rdm_forest, "bio1", xlabel = paste("bio1\nMean Annual  Temperature"), ylims = c (-1, 1)) 
					g_ice_bio12 <- ggplot_ice (rdm_forest, "bio12", xlabel = paste("bio12\nAnnual Precipitation (mm)"))
																
					g_ice_alti <- ggplot_ice (rdm_forest, "alti", xlabel = paste("Elevation (m)\n"))
					g_ice_alti_sd <- ggplot_ice (rdm_forest, "alti_sd", xlabel = paste("Elevation\n heterogeneity"))
						
					g_ice_grassland <- ggplot_ice (rdm_forest,  "grassland", xlabel = paste("% of Area\nin grassland"))
					g_ice_cropland <- ggplot_ice (rdm_forest,  "cropland", xlabel = paste("% of Area cropped\n"))

					g_ice_roads <- ggplot_ice (rdm_forest, "roads_log", xlabel = paste("(log) Road Density\n"))	
					g_ice_hii <- ggplot_ice (rdm_forest, "hii", xlabel = paste("Human\nImpact Index"))	
										
					g_ice_C30 <- ggplot_ice (rdm_forest, "C30", xlabel = paste("Carbon at\n30cm depth"))			
					g_ice_C0 <- ggplot_ice (rdm_forest, "C0", xlabel = paste("Carbon at\nSoil Surface"))				
					g_ice_ph_10 <- ggplot_ice (rdm_forest, "ph_10", xlabel = paste("pH at\n10cm depth"))				
								
					g_ice_area <- ggplot_ice (rdm_forest, "area_log", xlabel = paste("(log) Area\n")) #, angle = 45				



					# plot with all ice plots ordered according to var importance ranking		
						x11()
						gg_ice <- grid.arrange(g_ice_bio1, g_ice_C0, g_ice_bio12, g_ice_alti_sd, g_ice_grassland, g_ice_C30, g_ice_hii, g_ice_alti, g_ice_ph_10, g_ice_cropland, g_ice_area, g_ice_roads,  ncol = 4)

					# I/O
						ggsave("Fig_S5_partial_dependance_plots_ice.pdf", gg_ice, device = cairo_pdf, width = 4.75, height = 5) 





		# find interactions between covariates with VIMP

			library("randomForestSRC")

			rdF_SRC <- rfsrc(Y ~ ., data = XY_df)
			find.interaction(rdF_SRC, method = "vimp")



# SPATIAL ANALYSIS OF RESIDUALS

# reprendre ici (refaire couche spatiale)





		# merge worm data (<ith rasr) to spatial layer of counties		
				obs_rasr_sf <- merge(counties_env_sf_ne, obs_rasr, by = "GEOID")

		# add predicted rasr
				obs_and_predict_rasr_sf <- merge(obs_rasr_sf, fit)
				dim(obs_and_predict_rasr_sf)

		# housekeeping
				obs_and_predict_rasr_sf$rasr <- as.numeric(obs_and_predict_rasr_sf$rasr)
				obs_and_predict_rasr_sf$res <- obs_and_predict_rasr_sf$rasr - obs_and_predict_rasr_sf$fit
						

		# Remove isolated GEOIDs - to test autocorrelation
			
					obs_and_predict_rasr_sf2 <- obs_and_predict_rasr_sf %>% filter(!GEOID %in% c("01015","01057","02220","05059","05137","05149","06019","06041","06061","06109",
												"13059","13233","15001","15003","15009","17161","18097","19101","207","21033","21135","22","22019","22109",
												"22123","236","254","26097","26145","27021","27029","28007","28065","28111","28161","29001","29019","29095",
												"29109","29179","293","3","30007","30111","31109","31111","31153","35035","36113",
												"37159","39073","39091","40153","45071","46003","46047","46071","48385","48455","49","49043","49049",
												"51131","51159","51810","54091","54093","54109","55009","56019","56029","88","M18","M19","M22","M31"))

				dim(obs_and_predict_rasr_sf2)
				
				obs_and_predict_rasr_sp2 <- as_Spatial(obs_and_predict_rasr_sf2)
				row.names(obs_and_predict_rasr_sf2) <- obs_and_predict_rasr_sf2$GEOID

	# neighbourhood matrix based on contiguity (see vignette on neighbours by R. Bivand)

				xy <- coordinates(obs_and_predict_rasr_sp2)
				
				# neighbourhood
					# slow
						( w <- poly2nb(obs_and_predict_rasr_sp2, row.names = obs_and_predict_rasr_sp2$GEOID, snap = 5000 , queen = F) )
								# snap = forces contiguity between object close by but not touching themselves
								# queen : should objects in diagonals be considered as neighbours, (TRUE), which can lead to cross links

					# function converts nb object to a data.frame
					# https://www.r-bloggers.com/2015/06/plotting-spatial-neighbors-in-ggmap/

						nb_to_df <- function(nb, coords){
						  x <- coords[, 1]
						  y <- coords[, 2]
						  n <- length(nb)
						  cardnb <- card(nb)
						  i <- rep(1:n, cardnb)
						  j <- unlist(nb)
						  return(data.frame(x=x[i], xend=x[j],
						                    y=y[i], yend=y[j]))
						  }


					coord_df <- nb_to_df(w, xy)


				# weights
					 	wok <- w
						ww <- nb2listw(wok, style='W') #,zero.policy =T						


				# test for spatial patterns in residuals


					# moran test based on contiguitiy neighbourhood + weights
						MC <- moran.mc(obs_and_predict_rasr_sf2$res, ww, nsim = 500)

					# local Moran based on neighbourhoud

						local_moran <- sp.correlogram(neighbours = w, var = obs_and_predict_rasr_sf2$res, order = 10, method = "I", style = "W", zero.policy = T)
						plot(local_moran, main = "Local Moran's I\n on Contiguitiy Neighbourhood", ylim = c(-1,1))
						# savePlot("supp_fig_local_moran.pdf", type="pdf")


				#  --- Extended Data Figure 9: Global and local Moran ---

						dev.off()
						pdf("Fig_S9_moran&correlogram.pdf", width = 4.75, height = 2.5, pointsize = 8)
							par(mfrow=c(1,2))
							plot(MC, main="Global Moran", xlab="Residuals", las = 1) #,  cex.axis = .5, cex.main = 1, cex.lab = .5, cex.sub = .5)
							plot(local_moran, main = "Local Moran", ylim = c(-1,1), las = 1, cex.axis = .5, cex.main = .4, cex.lab = .4, cex.title = .3)
						dev.off()

			

# PREDICTIONS ON COMPLETE CONTINENT -------------------------


		# environmental data for all counties, on which to predict RASR

				# remove counties on which we don't want predictions
					counties_env_sf_small <- counties_env_sf_ne[!(counties_env_sf_ne$GEOID%in%c("117","","G991","G992","G993","G994","G995")),] 

				# convert to sp data
						counties_env_sf_small_ne <- counties_env_sf_small %>% filter(!st_is_empty(.))
						counties_env <- as_Spatial(counties_env_sf_small_ne)

				# get data table
						counties_env_df <- st_drop_geometry(counties_env_sf_small_ne)
						dim(counties_env_df) #  3453   18


		#  prep data on which to make predictions

			# select Env predictors

				counties_env_df <- counties_env_df[,c("GEOID","area_log",
														"roads_log",
														"bio1", "bio12", 
														"hii","grassland","cropland",
														"ph_10","C0","C30",
														"alti","alti_sd")]

				counties_env_df_nona <- na.omit(counties_env_df)


				dim(counties_env_df_nona)
				# 3453 13

			# rescale temperatures /10

				#div10 <- function(x) x/10
				#counties_env_df_nona[,c("bio1","bio2","bio3","bio5","bio8","bio9")] <- map_dfr(counties_env_df_nona[,c("bio1","bio2","bio3","bio5","bio8","bio9")],div10)

				# counties_env_df_nona$bio1 <- counties_env_df_nona$bio1/10


		# predictions

			# predicted RASR
				rdm_forest_predict_mean <- predict(rdm_forest, counties_env_df_nona)$predictions

			# uncertainity
				rdm_forest_predict_se <- predict(rdm_forest, counties_env_df_nona, type = "se")$se
						
			# dataframe with predictions and uncertainty			
				rdm_forest_predict <- data.frame(GEOID = counties_env_df_nona$GEOID, predict = rdm_forest_predict_mean, predict_se = rdm_forest_predict_se)
				head(rdm_forest_predict)
			

			# add prediction and uncertainty to spatial layer
				rasr_predicted <- merge(counties_env, rdm_forest_predict, by = "GEOID")

			# transfo in sf object	
				rasr_predicted_sf <- sf::st_as_sf(rasr_predicted)


