#' @title Make a soil compaction plot
#'
#' @description From output of  creates a soil compaction plot displaying BD and Infiltration data, and compares to other ranches in the region
#'
#' @param data dataframe from output of soil.final.cleanup()
#' @param transect Character string of selected ranch code
#' @param labels = TRUE determines whether points from the chosen ranch are labeled on the plot
#' @param pointcolors = c("black", "gray") a vector that specifies colors of points on the plot
#' @param legend specifies whether to display a legend
#' @param legendnames specifies how points are named on the legend
#' @param box.padding numeric adjusts spacing of labels on the plot
#' @param xlab,ylab character strings for axis lables
#' @param linetype specifies type of line shown for BD and Infiltration targets
#' @param Inftarget target infiltration time
#'
#' @return Shows soil compaction plot
#'
#' @examples compaction.plot(soil, "ranchname")
#'
#'
#' @export compaction.plot
#'
#'


compaction.plot<-function(data, transect, labels = TRUE, pointcolors = c("black","gray"), legend = FALSE, legendnames = c(transect, "Others"),
                          xlab = "Bulk density, distance from target", ylab = "Infiltration, distance from target",
                          box.padding = 0.5, linetype = "dashed", Inftarget = 3.81){

  masked = data %>% prepare.soil.triangle(Inftarget = Inftarget)
  masked$Transect = as.character(replace(as.character(masked$Transect), masked$Transect != transect, values = "zzzz"))

  masked1 = masked[is.na(masked$Carbon.0.10.survey) & is.na(masked$Carbon.10.40.survey),]
  masked2 = masked[!is.na(masked$Carbon.0.10.survey) & !is.na(masked$Carbon.10.40.survey),]

  masked1[10:16] = masked[,3:9]

  masked_soil = rbind(masked1, masked2)
  masked_soil = arrange(masked_soil, desc(Transect))

  masked_soil$TextCategory = ifelse(masked_soil$CLAY > 25, "Fine", ifelse(masked_soil$CLAY < 15 & masked_soil$SAND > 80, "Coarse","Coarse"))

  masked_soil$BD_target = replace(masked_soil$BD_target, masked_soil$TextCategory == "Fine", 1.1)
  masked_soil$BD_target = replace(masked_soil$BD_target, masked_soil$TextCategory == "Coarse", 1.4)

  masked_soil$BD_dist = masked_soil$BD_target - masked_soil$Bulk.Density.survey

  if(isTRUE(labels)){
    labs = as.character(masked_soil$Point)
  } else {
    labs = NA
  }


  p = ggplot(masked_soil, aes(x = BD_dist, y = Infilt_dist, color = Transect)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = linetype) +
    geom_vline(xintercept = 0, linetype = linetype) +
    scale_color_manual(values = pointcolors, labels = legendnames, guide = legend) +
    xlab(xlab) +
    ylab(ylab) +
    theme_bw() +
    geom_label_repel(aes(label = ifelse(Transect == transect, labs, NA)), box.padding = box.padding)


  return(p)
}







