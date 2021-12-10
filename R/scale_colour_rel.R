#' Adds custom Journal of Clinical Oncology style colours to plot. 
#' @export
#' @examples
#' plot + theme_lto() + scale_colour_rel
scale_colour_rel <- function() {
  
  # data is the dataframe to consider. The column with patients needs to be
  # called 'rel_code' 
  
  packages_scale_colour_rel <- c('ggsci', 'tidyverse')
  install.packages(setdiff(packages_scale_colour_rel, rownames(installed.packages()))) 
  library(ggsci)
  library(tidyverse)
  color_rel <- c('A' = "#0073C2FF", 
                 'B' = "#EFC000FF", 
                 'C' = "#868686FF" , 
                 'D' = "#CD534CFF", 
                 'E' = "#7AA6DCFF",
                 'F'=  "003C67FF",
                 'G' = "8F7700FF")
  
  rels <-  color_rel[names(color_rel) %in% data$rel_code]
  
  return(scale_colour_manual(name = "rel_code",values = rels))
}
