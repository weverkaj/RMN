#' @title Make a soil texture triangle
#'
#' @description From output of prepare.soil.triangle() creates a soil texture plot and compares a ranch to other ranches in the dataset
#'
#' @param data
#' @param transect
#'
#' @return Soil classification triangle with chosen ranch points emboldened
#'
#' @examples texture.triangle.plot(soil, "ABER")
#'
#'
#' @export texture_triangle_plot
#'
#' @return soil texture triangle
#'
#'
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

texture.triangle.plot = function(data, transect){

  data1 = subset(data, data$Transect == transect)
  data2 = subset(data, data$Transect != transect)


  ##texture plot##

  test = TT.plot(
    class.sys = "USDA.TT",    ## with the UDSA texture classes
    tri.data = data2,
    pch = 16,
    main = paste("Soil Texture - ",transect),
    cex.axis = 0.5,
    cex.lab = 0.75,
    cex.main = 0.75,
    text.tol = 0.2,
    tri.sum.tst = FALSE,
    col = "gray",
    cex = 0.75,
    frame.bg.col = "white",
    grid.show = FALSE
  )

  TT.points(geo=test,
            tri.data = data1,
            pch = 16,
            tri.sum.tst=FALSE,
            col="black",
            cex=0.75,
  )

  TT.text(
    tri.data = data1,
    geo = test,
    tri.sum.tst=FALSE,
    labels = data1$Location,
    pos=c(1,3,2),
    offset=0.2,
    font = 0.1,
    cex = 0.75,
    col= "black")
}
