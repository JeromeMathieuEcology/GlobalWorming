# plot ICE curves
	ggplot_ice <- function(model, x , ylims = c(-1, 1), xlabel = "My Label", angle = 0) {

		ranger_ice <- function(object, newdata) {
									  predict(object, newdata)$predictions
									}

		my_ice <- partial(model, pred.var = x, pred.fun = ranger_ice, recursive = F) %>%
			  				group_by(yhat.id) %>%  # perform next operation within each yhat.id
			  				mutate(yhat.centered = yhat - first(yhat))  # so each curve starts at yhat = 0	

		ice_g <- ggplot(my_ice, aes(x = get(x), y = yhat.centered)) +
								ylim(ylims) +
								labs(x = xlabel, y = "Effect on Predicted RASR") +
								geom_line(aes(group = yhat.id), alpha = 0.2, col = "#233E6CFF", size = .3) + #"#656870FF"
								stat_summary(fun = mean, geom = "line", col = "#BCAF6FFF", size = .7) + #"#E0CB5EFF"
								theme(	legend.position = "none",
										#panel.grid = element_blank(),
										panel.background = element_rect(fill = "transparent"), # bg of the panel
										plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
										panel.grid.minor = element_blank(),
										panel.grid.major.x = element_blank() ,
										panel.grid.major.y = element_line(size = .1, linetype = 'dotted', colour = "grey60"),
										axis.text.x = element_text(size = 5, angle = 0,  ), # hjust = 2
										#axis.text.y = element_blank(),
										#axis.ticks = element_blank(),
										#axis.title.y = element_blank(),
										axis.text = element_text(size = 5),
										axis.title = element_text( colour="grey35", size = 7 , margin = margin( t = 1)), #
										)
		return(ice_g)
	}
