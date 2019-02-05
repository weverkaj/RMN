#' @title Make a soil compaction plot
#'
#' @description From output of prepare.soil.triangle() creates a soil compaction plot displaying BD and Infiltration data, and compares to other ranches in the region
#'
#' @param data
#' @param transect
#' @param labels
#' @param guide
#'
#' @return Shows soil compaction plot
#'
#' @examples compaction.plot(soil, "ranchname")
#'
#'
#' @export compaction.plot
#'
#'


compaction.plot<-function(data, transect, labels = TRUE, guide = FALSE){

  masked = data %>% prepare.soil.triangle()
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
    l = geom_label_repel(aes(label = ifelse(Transect == transect, as.character(masked_soil$Point), NA)), box.padding = 0.5)
  } else {
    l = geom_label_repel(aes(label = ifelse(Transect == transect, NA, NA)), box.padding = 0.5)
  }


  p = ggplot(masked_soil, aes(x = BD_dist, y = Infilt_dist, color = Transect)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c("black", "gray"), labels = c(transect, "Others"), guide = guide) +
    xlab("Bulk density, distance from target") +
    ylab("Infiltration, distance from target") +
    theme_bw() +
    l

  return(p)
}







