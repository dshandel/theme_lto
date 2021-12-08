#' theme_lto
#'
#' Adds LTO minimal theme layer to ggplots adding + theme_lto to ggplot object. 
#' To add Journal of Clinical Oncology themed colours add: + scale_fill_lto_clinic() for barplots or scale_colour_lto_clinic() for dotplots, lineplots etc.
#' To add Nature Publishing Group themed colours add: + scale_fill_lto_trans() for barlots or scale_colour_lto_trans() for dotplots, lineplots etc. 
#' Any margin warnings can be ignored. 
#' @param base_family Font of the texts in the plots. Default at 'sans'.
#' @param text_size Size of text. Default at 18.
#' @param grid Boolean value. If True adds grid to plot. Default at False.
#' @param facet_box Boolean value. Draws a box around facets. Only handy when facets are used with facet_wrap. Default at True.
#' @param legend_position Defines the position of the legend within the plot. Either choose: 'topright', 'topleft', 'bottomright', 'bottomleft' (all within the figure)or 'top', 'left', 'right', 'bottom' (out of the figure). Default at 'top'. 
#' @param legend_title Boolean value. If True adds the title of the legend. Set at False. 
#' @return Returns the lto theme. 
#' @examples 
#' # Creating an arbitrary dataframe
#' df <- data.frame(x = c(3,5,7,8,5), y = c(3,7,9,0,3), cline = c('HUB181I', 'HUB181I', 'HUB181I','HUB197', 'HUB197'))
#' # Creating a ggplot object
#' plot <- ggplot(data = df, aes(x = x, y = y, col = cline)) +  geom_point(size = 5) 
#' # Plotting a geom_point with theme_lto
#' plot + theme_lto()
#' # Legend to top left.
#' plot + theme_lto(legend_position = 'topleft')
#' # With grids.
#' plot + theme_lto(grid = T)
#' # With facets but no box around the facets
#' plot + facet_wrap(~cline) + theme_lto(facet_box = F)
#' # Adding Nature Publishing group themes colours.
#' plot + theme_lto() + scale_colour_lto_trans()
#' @export
theme_lto<- function(base_size=16, 
                      base_family="sans", 
                      text_size = 16, 
                      grid = FALSE, 
                      facet_box = TRUE,
                      legend_position = 'top',
                      legend_title = FALSE) {
  # base_size = general size of the plot
  # text_sizes = general sie of the texts in the plot. 
  # grid: if TRUE shows grid in the plot. (default FALSE)
  # facet_box: if TRUE shows boxes in facet or grid plot (default TRUE)
  # legend_position (default top): either 'topright', 'topleft', 'bottomright', 'bottomleft' (all within the figure)
  # or 'top', 'left', 'right', 'bottom' (out of the figure),
  # or 'none' (no legend).
  # ! It is sometimes necessary to adjust the position. 
  # legend_title (default FALSE) If TRUE shows legend title otherwise hides it.
  # legend_key_size lets you adjust the thicknes of the colour marks in the legend. 
  
  # Installing packages the user will need: 
  packages_theme_lto <- c('ggthemes', 'scales', 'cowplot', 'lemon', 'ggsci', 'tidyverse')
  
  install.packages(setdiff(packages_theme_lto, rownames(installed.packages()))) 
  
  # loading package
  library(ggthemes)
  library(scales)
  library(cowplot)
  library(lemon)
  library(ggsci)
  library(tidyverse)
  
  (theme_foundation(base_size=base_size, base_family=base_family) 
    + theme_cowplot() # use theme cowplot as base theme. 
    + theme(
      # specifiying text characteristics. 
      plot.title = element_text(face = "bold",  
                                size = rel(1.2), hjust = 0.5), # adjust size and posiition
      text = element_text(size = text_size, family = base_family), 
      axis.title = element_text(face = "bold",size = rel(1)),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),
                                  angle=90, vjust = 3.7), # angle 90 degrees, adjust vertical (away from plot)
      axis.title.x = element_text(vjust = -2, 
                                  margin = margin(t = 15, r = 0, b =0, l = 0)), # adjust position (away from plot) 
      axis.text = element_text(), 
      
      # define characteristics of the x and y axis lines and ticks. 
      axis.ticks.length=unit(.1, "cm"),
      axis.line = element_line(colour="black"), 
      axis.ticks = element_line(colour="black"), 
      axis.text.y = element_text(margin = margin(r = 6), size = 14),
      axis.text.x = element_text(margin = margin(t = 6), size = 14),
      
      
      # defining some legend characteristics. # you might need to change these according to 
      # your plot. 
      legend.key = element_rect(colour = NA),
      legend.position = (if (legend_position == 'topleft') {
        c(0.05, 0.85)
      } else if (legend_position == 'topright') {
        c(0.70, 0.85)
      } else if (legend_position == 'bottomleft') {
        c(0.02, 0.25)
      } else if (legend_position == 'bottomright') {
        c(0.70, 0.25)
      } else 
        legend_position),
      legend.direction = ifelse(legend_position %in% c('bottom', 'top'), 'horizontal', 'vertical'), 
      legend.margin = unit(0, "cm"),
      legend.title = (if(legend_title == TRUE) {
        element_text(face="italic")
      } else
        element_blank()),
      
      # ifelse(legend_position %in% c('bottom','top'), "horizontal", "vertical")
      
      # defining panel and plot characteristics
      # if the user defined grid = TRUE, show a grid in the plot with grey colour. 
      panel.grid.major = element_line(colour= ifelse(grid == TRUE, "#f0f0f0", NA), size = 1.2), 
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(10,5,5,5),"mm"),
      panel.background = element_rect(colour = NA), # set panel background colour to white
      plot.background = element_rect(colour = NA), # set plot background colour to white
      panel.border = element_rect(colour = NA), # no panel border
      
      # define some facet_wrap/facet_grid statistics
      strip.background=element_rect(colour= ifelse(facet_box == TRUE, 'black', NA), 
                                    fill= NA,
                                    size = 2,
                                    linetype = 'solid'),
      strip.text = element_text(face="bold")
    ))
  
}


#' scale_colour_lto_clinic
#' 
#' Adds Journal of Clinical Oncology style colours to plot. 
#' @examples
#' plot + theme_lto() + scale_colour_lto_clinic()
scale_colour_lto_clinic <- function() {
  scale_color_jco()
}

#' scale_fill_lto_clinic
#' 
#' Same as scale_colour_lto_clinic, but to fill a plot like a barplot.
scale_fill_lto_clinic <- function() {
  scale_fill_jco()
  }

#' scale_colour_lto_trans
#' 
#' Adds Nature Publishing group style colours to plot. 
#' @examples
#' plot + theme_lto() + scale_colour_lto_trans()
scale_colour_lto_trans <- function() {
  scale_color_npg()
}

#' scale_fill_lto_trans
#' 
#' Same as scale_colour_lto_trans, but to fill a plot like a barplot.
scale_fill_lto_trans <- function() {
  scale_fill_npg()
}


#' scale_colour_rel, scale_fill_rel, scale_colour_org and scale_fill_org
#' 
#' These are personal functions and can be ignored. 
scale_colour_rel <- function() {
  
  # data is the dataframe to consider. The column with patients needs to be
  # called 'rel_code' 
  
  packages_scale_colour_rel <- c('ggsci', 'tidyverse')
  install.packages(setdiff(packages_scale_colour_rel, rownames(installed.packages()))) 
  library(ggsci)
  library(tidyverse)
  color_rel <- c('1' = "#0073C2FF", 
                 '2' = "#EFC000FF", 
                 '3' = "#868686FF" , 
                 '10' = "#CD534CFF", 
                 '11' = "#7AA6DCFF",
                 '12'=  "003C67FF",
                 '14' = "8F7700FF")
  
  rels <-  color_rel[names(color_rel) %in% data$rel_code]
  
  return(scale_colour_manual(name = "rel_code",values = rels))
}

scale_fill_rel <- function(data) {
  # data is the dataframe to consider. The column with patients needs to be
  # called 'rel_code' 
  

  packages_scale_fill_rel <- c('ggsci', 'tidyverse')
  install.packages(setdiff(packages_scale_fill_rel, rownames(installed.packages()))) 

  library(ggsci)
  library(tidyverse)
  color_rel <- c('1' = "#0073C2FF", 
                 '2' = "#EFC000FF", 
                 '3' = "#868686FF" , 
                 '10' = "#CD534CFF", 
                 '11' = "#7AA6DCFF",
                 '12'=  "003C67FF",
                 '14' = "8F7700FF")
  
  rels <-  color_rel[names(color_rel) %in% data$rel_code]
  
  
  return(scale_fill_manual(name = "rel_code",values = rels))
}


# Personal colours for organoids. Based on Nature Publishing Group. 
scale_colour_org <- function(data) {
  # data is the dataframe to consider. The column with organoids needs to be
  # called 'cline' 
  
  packages_scale_colour_org <- c('ggsci', 'tidyverse')
  install.packages(setdiff(packages_scale_colour_org, rownames(installed.packages()))) 
  

  library(ggsci)
  library(tidyverse)
  color_org <- c(HUB181I = "#8491B4FF", 
                 HUB005 = "#E64B35FF", 
                 HUB062 = "#91D1C2FF", 
                 HUB015 = "#4DBBD5FF",  
                 HUB106 = "#3C5488FF", 
                 HUB183 = "#F39B7FFF", 
                 HUB175 = "#00A087FF",
                 HUB197 = 'grey')
  
  orgs <- color_org[names(color_org) %in% data$cline]
  
  
  
  return(scale_colour_manual(name = "cline",values = orgs))
}

scale_fill_org <- function(data) {
  # data is the dataframe to consider. The column with organoids needs to be
  # called 'cline' 
  
  packages_scale_fill_org <- c('ggsci', 'tidyverse')
  install.packages(setdiff(packages_scale_colour_org, rownames(installed.packages()))) 

  library(ggsci)
  library(tidyverse)
  color_org <- c(HUB181I = "#8491B4FF", 
                 HUB005 = "#E64B35FF", 
                 HUB062 = "#91D1C2FF", 
                 HUB015 = "#4DBBD5FF",  
                 HUB106 = "#3C5488FF", 
                 HUB183 = "#F39B7FFF", 
                 HUB175 = "#00A087FF",
                 HUB197 = 'grey')
  
  orgs <- color_org[names(color_org) %in% data$cline]
  
  return(scale_fill_manual(name = "cline", values = orgs))
}


