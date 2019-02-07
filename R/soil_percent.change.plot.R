#' @title Make a percent change boxplot of soil carbon and bulk Density
#'
#' @description Creates a plot that shows average change in SOC at 2 depths and BD compared to regional average
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
#' @export percent.change.plot
#'
#'
#'



percent.change.plot = function(data, transect){

masked = data
masked$Transect = as.character(replace(as.character(masked$Transect), masked$Transect != transect, values = "zzzz"))
masked = masked %>% arrange(desc(Transect))

masked = subset(masked, select = c("Point", "Transect" ,"Carbon010change", "Carbon1040change","Bulk.density.change"))

masked = melt(masked, id.vars = c("Point", "Transect"))

ggplot(masked, aes(x = variable, y = value, color = Transect)) +
  geom_boxplot() +
  scale_color_manual(name = "Ranch", values = c("black", "gray"), labels = c(transect, "Others")) +
  theme_bw() +
  scale_x_discrete(labels = c("Carbon 0-10 cm", "Carbon 10-40 cm", "Bulk Density")) +
  ylab("% Change") +
  xlab(NULL)


}




