#' View a Woodson Pallette color scheme. 
#'
#' @description This function shows a woodson pallette color scheme when the name is entered. 
#'  When no name of a wpal() color scheme is entered, all woodson pallette shemes are shown.
#'  
#' @param color the name of the color scheme
#
#' @return plots of one or all Woodson pallette schemes
#' @export
#'
#' @examples view_wpal()
#' @examples view_wpal("easter_to_earth")


view_wpal<-function(color=NULL){
  
  if (is.null(color)){
    print("No color specified; plotting all colors")
    index<-1
    for (color in names(wpal())){
      if (index==1){grid.newpage(); pushViewport(viewport(layout = grid.layout(5, 15)))}
      if (index<=5){print((plot_colors(wpal(color),color_list_name=color)), vp = vplayout(index, 1:15))}
      index <- index+1
      if (index>5){index<-1}
    }}else{
      print(paste0("Plotting wpal color scheme ",color))
      print((plot_colors(wpal(color),color_list_name=color)))}
} # Closing function