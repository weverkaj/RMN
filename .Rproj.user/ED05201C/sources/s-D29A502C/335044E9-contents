#' @title Make a soil compaction plot
#'
#' @description From output of  creates a soil compaction plot displaying BD and Infiltration data, and compares to other ranches in the region
#'
#' @param data dataframe from output of soil.final.cleanup()
#' @param transect Character string of selected ranch code
#' @param background = TRUE whether to display "background" data on plot not from selected ranch
#' @param labels = TRUE determines whether points from the chosen ranch are labeled on the plot
#' @param pointcolors = c("black", "gray") a vector that specifies colors of points on the plot
#' @param legend specifies whether to display a legend
#' @param legendnames specifies how points are named on the legend
#' @param legendtitle title for the legend
#' @param box.padding numeric adjusts spacing of labels on the plot
#' @param xlab,ylab character strings for axis lables
#' @param linetype specifies type of line shown for BD and Infiltration targets
#' @param Inftarget target infiltration time
#' @param xlims,ylims numeric vectors of length 2 that specify the respective lower and upper limits of the axes. NA values cause default axis limits
#'
#' @return Shows soil compaction plot
#'
#' @examples compaction.plot(soil, "ranchname")
#'
#'
#' @export compaction.plot
#'
#'


compaction.plot<-function(data,
                          transect,
                          year,
                          background = TRUE,
                          labels = TRUE,
                          pointcolors = c(rep("black", length(transect)),"gray"),
                          legend = TRUE,
                          legendnames = c(paste(c(transect)), "Others"),
                          legendtitle = "Ranch",
                          xlab = "Bulk density, distance from target",
                          ylab = "Infiltration, distance from target",
                          box.padding = 0.5,
                          linetype = "dashed",
                          Inftarget = 3.81,
                          xlims = c(NA, NA),
                          ylims = c(NA, NA)){

  library(reshape2)
  library(dplyr)
  library(ggplot2)
  library(soiltexture)
  library(ggrepel)

  data = subset(data, YEAR %in% year)
  if(!background){data = subset(data, subset = Transect %in% transect)}
  masked = data %>% prepare.soil.triangle(Inftarget = Inftarget)
  masked$Transect = as.character(replace(as.character(masked$Transect), !(masked$Transect %in% transect), values = "zzzz"))


  masked_soil = arrange(masked, desc(Transect))




  if(isTRUE(labels)){
    labs = as.character(masked_soil$Point)
  } else {
    labs = NA
  }



  p = ggplot(masked_soil, aes(x = BD_dist, y = Infilt_dist, color = Transect)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = linetype) +
    geom_vline(xintercept = 0, linetype = linetype) +
    scale_color_manual(values = pointcolors, labels = legendnames) +
    guides(color = ifelse(legend, guide_legend(title = legendtitle), FALSE), label = FALSE) +
    xlab(xlab) +
    ylab(ylab) +
    theme_bw() +
    geom_label_repel(aes(label = ifelse(Transect == transect, labs, NA)), box.padding = box.padding, show.legend = FALSE) +
    xlim(as.numeric(xlims[1]), as.numeric(xlims[2])) +
    ylim(as.numeric(ylims[1]), as.numeric(ylims[2]))


  return(p)
}







