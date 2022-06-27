# --- script to produce fig. 3 : acccumulated number of earthworm species through time and temporal dynamics of interceptions at the US borders ---
# More info here: https://github.com/JeromeMathieuEcology/GlobalWorming.

# libraries
  library(ggplot2)
  library(Hmisc)
  library(ggExtra)
  library(viridis)
  library(gridExtra)
  library(cowplot)
  library(dplyr)


# read data

  # first record of species
      first_records <- read.csv2(".\\EWINA\\EWINA_1st_RECORDS\\EWINA_1st_records.csv")

  # interceptions data
        interceptions <- read.csv2(".\\EWINA\\EWINA_IPATHS\\EWINAPATH.csv", sep = ",")


# compute cummulated total number of exotic species

        nsp_exotic_cumm <- NULL

        i <- 0
        for (y in min(first_records$first_year):2021){
          i <- i+1
          nsp_exotic_cumm[i] <- length(unique(first_records$species_name[first_records$first_year<=y & first_records$exotic== "yes"]))
           } 


        nsp_exotic_cumm <- data.frame(year = I(min(first_records$first_year):2021), nSpCum = nsp_exotic_cumm)


# compute cummulated number of species_name recorded by origin

        nsp_cumm_origin <- data.frame(0,0)

        i <- 0
        j <- 0

        for (t in 1:length(unique(first_records$origin))) {
          for (y in min(first_records$first_year):2021){
            i <- i+1
            nsp_cumm_origin[i,t] <- length(unique(first_records$species_name[first_records$first_year <= y & first_records$origin == unique(first_records$origin)[t] ]))
          } 
          i <- 0    
        }

        names(nsp_cumm_origin) <- unique(first_records$origin)
        nsp_cumm_origin <- data.frame(year=I(min(first_records$first_year):2021), nsp_cumm_origin)
        nsp_cumm_origin$exotic <- nsp_exotic_cumm$nSpCum

    # remove old observations
      nsp_cumm_origin <- nsp_cumm_origin[nsp_cumm_origin$year>1860,]




# plot 3A / accumulated number of species through time
  text_size <- 4

  g_cumm <- ggplot(data = nsp_cumm_origin, aes(x = year, y = exotic)) + 
     #geom_hline(yintercept = c(0, 10, 20, 30, 40, 40, 50), size = 0.5, linetype = 5 , colour = "grey60") +
     annotate(geom = "segment", x = rep(1860, 7), xend = rep(2022, 7), y = c(0, 10, 20, 30, 40, 40, 54), yend = c(0, 10, 20, 30, 40, 40, 54), size = 0.5, , colour = "grey65") + #linetype = 5 
     geom_vline(xintercept = c(1900, 1950, 2000), linetype = "dotted", color = "grey60", size = 1) +

     labs(y = " Alien Earthworm Species") +

     geom_ribbon(data = nsp_cumm_origin, aes(x = year, ymax = exotic + 54), ymin = 54, fill = viridis_pal( option = "cividis", begin = 0.35, end = .9, alpha = 0.9)(25)[2]) +
     geom_line(data = nsp_cumm_origin, aes(x = year, y = exotic + 54), size = 1.1, lineend = "round", col = viridis_pal(option = "cividis", begin = 0.35, end = .9)(25)[2]) +

     geom_ribbon(data = nsp_cumm_origin, aes(x = year, ymax = Europe + 40), ymin = 40, fill = viridis_pal( option = "cividis", begin = 0.35, end = .9)(25)[8], alpha = 0.8) +
     geom_line(data = nsp_cumm_origin, aes(x = year, y = Europe + 40), size = 1.1, lineend = "round", col = viridis_pal(option = "cividis", begin = 0.35, end = .9)(25)[5]) +
     geom_line(data = nsp_cumm_origin, aes(x = year, y = Europe + 40.5), size = .1, lineend = "round", col = "white") +


     geom_ribbon(data = nsp_cumm_origin, aes(x = year, ymax = Asia + 30), ymin = 30, fill = viridis_pal( option = "cividis", begin = 0.35, end = .9)(25)[13], alpha = 0.8) +
     geom_line(data = nsp_cumm_origin, aes(x = year, y = Asia + 30), size = 1.1, lineend = "round", col = viridis_pal(option = "cividis", begin = 0.35, end = .9)(25)[10]) +
     geom_line(data = nsp_cumm_origin, aes(x = year, y = Asia + 30.5), size = .1, lineend = "round", col = "white" ) +
         
     geom_ribbon(data = nsp_cumm_origin, aes(x = year, ymax = South.America + 20), ymin = 20, fill = viridis_pal( option = "cividis", begin = 0.35, end = .9)(25)[16], alpha = 0.8) +
     geom_line(data = nsp_cumm_origin, aes(x = year, y = South.America + 20), size = 1.1, lineend = "round", col = viridis_pal(option = "cividis", begin = 0.35, end = .9)(25)[16]) +
     geom_line(data = nsp_cumm_origin, aes(x = year, y = South.America + 20.5), size = .1, lineend = "round", col = "white" ) +
         
     geom_ribbon(data = nsp_cumm_origin, aes(x = year, ymax = Pacific + 10), ymin = 10, fill = viridis_pal( option = "cividis", begin = 0.35, end = .9)(25)[20], alpha = 0.8) +
     geom_line(data = nsp_cumm_origin, aes(x = year, y = Pacific + 10), size = 1.1, lineend = "round", col = viridis_pal(option = "cividis", begin = 0.35, end = .9)(25)[20]) +
     geom_line(data = nsp_cumm_origin, aes(x = year, y = Pacific + 10.5), size = .1, lineend = "round", col = "white")  +
       
     geom_ribbon(data = nsp_cumm_origin, aes(x = year, ymax = Africa), ymin = 0, fill = viridis_pal( option = "cividis", begin = 0.35, end = .9)(25)[18], alpha = 0.8) +
     geom_line(data = nsp_cumm_origin, aes(x = year, y = Africa), size = 1.1, lineend = "round", col = viridis_pal(option = "cividis", begin = 0.35, end = .9)(25)[25]) +
     geom_line(data = nsp_cumm_origin, aes(x = year, y = Africa + .5), size = .1, lineend = "round", col = "white") +
   
     # country labels
      annotate(geom = "text", x = 1920, y = 76, label = "Total", fontface = "bold.italic", colour = "grey10", size = text_size, hjust = 0) +
      annotate(geom = "text", x = 1920, y = 49, label = "Europe", fontface = "italic", colour = "grey10", size = text_size, hjust = 0) +
      annotate(geom = "text", x = 1920, y = 33, label = "Asia", fontface = "italic", colour = "grey10", size = text_size, hjust = 0) +
      annotate(geom = "text", x = 1920, y = 23, label = "South America", fontface = "italic", colour = "grey10", size = text_size, hjust = 0) +    
      annotate(geom = "text", x = 1920, y = 13, label = "Pacific", fontface = "italic", colour = "grey10", size = text_size, hjust = 0) +    
      annotate(geom = "text", x = 1920, y = 3,  label = "Africa", fontface = "italic", colour = "grey10", size = text_size , hjust = 0) +

     # number of sp
      annotate(geom = "text", 2028, y = 134, label = "70", colour = "grey20", size = text_size) +
      annotate(geom = "text", 2028, y = 73, label = "32", colour = "grey20", size = text_size) +
      annotate(geom = "text", 2028, y = 51, label = "20", colour = "grey20", size = text_size) +
      annotate(geom = "text", 2028, y = 31, label = "11", colour = "grey20", size = text_size) +  
      annotate(geom = "text", 2028, y = 14, label = "3", colour = "grey20", size = text_size) +
      annotate(geom = "text", 2028, y = 5, label = "4", colour = "grey20", size = text_size) +

     removeGridX() +

     theme( 
             axis.title = element_blank(),
             axis.text.x = element_blank(), 
             axis.text.y = element_blank(), 
             axis.ticks = element_blank(), 
             panel.grid.major = element_blank(), 
             panel.grid.minor = element_blank(), 
             panel.background = element_rect(fill = "transparent"), 
             plot.background = element_rect(fill = "transparent", color = NA),
             plot.margin = unit(c(1, 1, 1, 0.5), "cm")
             )


# PLOT 3B / CALENDAR INTERCEPTIONS --------------------------------------------------------------

  # number of interceptions per year
      table_year <- as.data.frame( table(interceptions$year) )
      names(table_year) <- c("year","freq")      
      table_year$year <- as.character(table_year$year)
      table_year$year <- as.numeric(table_year$year)      
      table_year <- table_year[table_year$year > 1945 & table_year$year <1975 ,]


  # number of interceptions per year and month
    table_month_year <- as.data.frame( table(interceptions$year,interceptions$month) )

    names(table_month_year) <- c("year","month","freq")
    table_month_year$month <- as.character(table_month_year$month)
    table_month_year$month <- as.numeric(table_month_year$month)
    table_month_year$year <- as.character(table_month_year$year)
    table_month_year$year <- as.numeric(table_month_year$year)


    table_month_year <- table_month_year[table_month_year$year > 1945 & table_month_year$year <1975 ,]

    table_month_year <- na.omit(table_month_year)
    table_month_year <- droplevels(table_month_year)



    quantiles <- c(0, 1, 15, 30)
    labels <- c("0","1-15","15-30")
    intercept_level_cat <- cut(table_month_year$freq, breaks = quantiles, labels = labels, include.lowest = T)

    month_av <- aggregate(table_month_year,list(table_month_year$month),mean)


    # subplots
      g_month <-  ggplot(month_av, aes(x = month, y = freq)) +
                  geom_line() +
                  scale_y_continuous(limits = c(0,4.5),breaks = 0:4) +                  
                  scale_x_continuous( breaks = 1:12, , expand = c(0.03,0.03)) +                   
                  coord_flip() +
                  removeGridX() +
                  theme(
                        #plot.title = element_text(color= "white",hjust=0,vjust=1, size=rel(3)),
                        plot.background = element_rect(fill= "white"),
                        panel.background = element_rect(fill= "white"),
                        panel.border = element_rect(fill = NA, color = "white", size = 0.5, linetype = "solid"),
                        panel.grid.major.x = element_line(size = 0.5, linetype = 5 , colour = "grey60"),
                        panel.grid.minor = element_blank(),
                        #axis.title = element_text( colour = "grey10", size = 20, margin = margin( t = 1)),
                        axis.line = element_blank(),
                        axis.ticks = element_blank(), 
                        axis.text = element_text(color= "grey20", size = rel(1.2)),
                        #axis.text.y  = element_text(hjust=1),
                        axis.text.y = element_blank(),
                        legend.text = element_text(color= "grey20", size = rel(1.6)),
                        legend.background = element_rect(fill= "white"),
                        legend.position = "bottom",
                        #plot.margin = margin(t = 40, r = 0, b = 30, l = 0, unit = "pt"),
                        legend.title.align = 0.5,
                        legend.title  = element_text(size=16,colour= "grey10"),#,
                        axis.title = element_blank(),
                        plot.margin = unit(c(.5,0,1,0), "cm")
                        #legend.title = element_blank()
                        )



      g_temp <- ggplot(table_year, aes(x = year, y = freq)) +
                  geom_line() +
                  scale_y_continuous(breaks = c(0,50,100,150)) +
                  scale_x_continuous(limits = c(1947,1973), expand = c(0,0)) +       
                  removeGridX() +

                  theme(
                        #plot.title = element_text(color= "white",hjust=0,vjust=1, size=rel(3)),
                        plot.background = element_rect(fill= "white"),
                        panel.background = element_rect(fill= "white"),
                        panel.border = element_rect(fill=  NA, color = "white", size = 0.5, linetype = "solid"),
                        panel.grid.major = element_line(size = 0.5, linetype = 5 , colour = "grey60"),
                        panel.grid.minor = element_blank(),
                        axis.line = element_blank(),
                        axis.ticks = element_blank(), 
                        axis.text = element_text(color= "grey20", size=rel(1.2)),
                        axis.text.y  = element_text(hjust = 1, margin = margin(r = 15)),
                        axis.text.x = element_blank(),
                        legend.text = element_text(color= "grey20", size=rel(1.6)),
                        legend.background = element_rect(fill= "white"),
                        legend.position = "bottom",
                        legend.title.align = 0.5,
                        legend.title  = element_text(size=16,colour= "grey10"),#,
                        axis.title = element_blank(),
                        plot.margin = unit(c(0,0,0,0), "cm")
                  )

      # colors        
        my_pal <- c("#DADADA" , "#A77F61" , "#6D2120")

      # calendar plot 
        g_cal <- ggplot(table_month_year, aes(year, month, fill = intercept_level_cat)) + 
       
                    geom_tile(colour= "white", size = 1.5, stat = "identity") + 
                    scale_fill_manual(values = my_pal, name = NULL) +
                    scale_y_continuous(breaks = 1:12, labels = month.abb[1:12]) +
                    ylab("") +
                    xlab("") +
                    theme(
                      plot.background = element_rect(fill= "white"),
                      panel.background = element_rect(fill= "white"),
                      panel.border = element_rect(fill= NA , color = "white", size = 0.5, linetype = "solid"),
                      panel.grid = element_blank(),
                      axis.line = element_blank(),
                      axis.ticks = element_blank(), 
                      axis.text = element_text(color = "grey20", size = rel(1.2)),
                      axis.text.y = element_text(hjust = 1),
                      axis.text.x = element_text(vjust = 5), #margin = margin(t = -5)
                      axis.title = element_text(colour = "grey10", size = 13),                
                      axis.title.x = element_text(margin = margin( t = 10)),
                      legend.text = element_text(color= "grey20", size = rel(1.2)),
                      legend.title = element_text(size = 10, colour = "grey10"),  
                      legend.title.align = 0.5,                              
                      legend.background = element_rect(fill= "white"),
                      legend.key = element_blank(),
                      legend.position = "bottom", # c(0.5, -.2), #
                      legend.margin = margin(0,0,0,0),
                      legend.box.margin = margin(-45, 0 ,15, 0),
                      plot.margin = unit( c(0,0,0,0), "cm")              

                )






# complete figure

  g_frame <- ggplot() +
              theme(
                  plot.background = element_rect(fill= "white"),
                  panel.background = element_rect(fill= "white")
                  )

  # zoom
    zzoom <- data.frame( x = c(0.37,.55,.55,.69,.69,.87), y = c(.46, .52, .92,.92, .52, .46))


    ggdraw() +
          draw_plot(g_frame) + 
          geom_polygon(data = zzoom, aes(x = x, y = y), fill = "#CFCFCD") + ##73D055FF "#20A387FF"           , alpha = .8
          draw_plot(g_cumm , 0.08, 0.5, .97, .5) +   
          draw_plot(g_month, 0.85, 0.015, .125, .366) + 
          draw_plot(g_cal, 0.22, 0, .66, .38) + 
          draw_plot(g_temp , 0.268, 0.375, .58, .08) + 
          annotate( geom = "text", .20, y = .26, label = "Number of Interpections events in the US", colour=  "grey15", size = 5, angle = 90)  +
          annotate( geom = "text", .11, y = .73, label = "Number of Alien Species", colour =  "grey20", size = 5, angle = 90)  +  
          annotate( geom = "text", .34, y = .535, label = "1900", colour = "grey20", size = 5) +
          annotate( geom = "text", .575, y = .535, label = "1950", colour = "grey20", size = 5) +
          annotate( geom = "text", .8, y = .535, label = "2000", colour = "grey20", size = 5) +
          annotate( geom = "text", .05, y = .9, label = "A", colour = "black", size = 6) +  
          annotate( geom = "text", .13, y = .5, label = "B", colour = "black", size = 6) +            
          annotate( geom = "text", x =.62, y = .477, label = "Interceptions", fontface = "italic", colour = "grey20", size = 5) 

    
    ggsave("Fig_3_temporal_dynamics.pdf", device = cairo_pdf, width = 4.75, height = 8.5)