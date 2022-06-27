# requires a model built with ranger


# function to plot variable importance

	plot_var_imp <- function(Xvip){
						# Xvip : dataframe with twho columns : variables and their importance
							vip_g <- ggplot(data = Xvip, aes(x = var, y = vip)) + 
									geom_segment( aes(xend = var, yend=0), col= "grey75") +
									geom_point(,size =4) +
									coord_flip() +
									theme_minimal() +
									theme( axis.text = element_text(size=16),
		        							axis.title= element_blank())
									return(vip_g)
					}
					

# function to compute partial dependence data for plots plots
						

						partial_dependence <- function( predictor) {
							# needs to have in the environement :
								# model, newdata and size
								# predictor = variable along which to compute predictions

								var <- ensym(predictor)
								x_s <- select(newdata, !!var)
								x_c <- select(newdata, -!!var)
								grid <- crossing(x_s, x_c)

								idx_sb_sple <- sample(1:dim(grid)[1],size)
								sub_grid <- grid[idx_sb_sple,]

								my_predicts <- predict(model, sub_grid)$predictions
								my_predicts <- cbind(sub_grid,my_predicts)

								pd_tmp <- my_predicts %>%
									gather(class, prob, c("A", "B","C")) %>% 
									group_by(class, !!var) %>%
									summarize(marginal_prob = mean(prob))
								return(pd_tmp)
						}