#' @title Make a soil compaction plot
#'
#' @description From output of prepare.soil.triangle() creates a soil compaction plot displaying BD and Infiltration data, and compares to other ranches in the region
#'
#' @param data
#' @param transect
#' @param ysd
#' @param xsd
#'
#' @return Shows soil compaction plot
#'
#' @examples compaction.plot(soil, "ranchname")
#'
#'
#' @export compaction.plot
#'
#'


compaction.plot<-function(data, transect){

  masked = data %>% prepare.soil.triangle()
  masked$Transect = as.character(replace(as.character(masked$Transect), masked$Transect != transect, values = "zzzz"))
  masked = masked %>% arrange(desc(Transect))

  ggplot(masked, aes(x = BD_dist, y = Infilt_dist), color = Transect) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c("black", "gray"), labels = c(transect, "Others")) +
    xlab("Bulk density, distance from target") +
    ylab("Infiltration, distance from target") +
    theme_bw()
}







