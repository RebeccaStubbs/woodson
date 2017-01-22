#' Plot colors of a color pallette
#'
#' @description This function creates colored swatches of a list
#' of colors for easy visualization. 
#'
#' @param color_list list or vector of valid R colors
#' @param color_list_name name of color scheme, optional, default is NULL. 
#' 
#' @return ggplot of color swatches and optional name of color pallette
#' @export
#'
#' @examples plot_colors(c("red,"yellow","blue"),"primary colors")

plot_colors<-function(color_list,color_list_name=NULL){
  # color_list: list of colors
  # color_list_name: The title of the plot
  # requires: data.table,ggplot2,ggthemes
  
  data_table<-data.table(data.frame(x=letters[1:length(unique(color_list))]))
  data_table[, colors:=unique(color_list)]
  data_table[, bar_height:=1]
  p<-ggplot(data_table,aes(x=colors))+geom_bar(aes(fill=colors, stat="bin"))+theme_tufte()+labs(x=paste(unique(color_list), collapse=', '))+
    theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank())
  p<-p+ scale_fill_manual(values=color_list) + ggtitle(color_list_name)
  return(p)
}