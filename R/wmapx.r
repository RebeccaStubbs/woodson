wmapx<-function (chloropleth_map, geog_id, variable, outline_map = NULL, 
          data = NULL, chlor_lcol = NA, chlor_lsize = 0, histogram = FALSE, 
          hist_color = NULL, dist_stats = NULL, mean_color = "red", 
          quantile_color = "black", return_map_object_only = FALSE, 
          destination_folder = NULL, color_ramp = wpal("easter_to_earth"), 
          outline_size = 0.1, outline_color = "white", override_scale = NULL, 
          color_value_breaks = NULL, diverging_centerpoint = NULL, 
          map_title = " ", map_subtitle = NULL, title_justification = "center", 
          additional_variable_name_string = NULL, title_font_size = NULL, 
          title_font_face = "plain", fontfamily = "serif", fontsize = 12, 
          series_dimension = NULL, series_sequence = NULL, legend_name = NULL, 
          legend_position = "bottom", legend_font_size = NULL, legend_font_face = "plain", 
          legend_bar_width = 0.4, legend_bar_length = 20, legend_breaks = NULL, 
          legend_labels = NULL, scramble_colors = FALSE, patch_width = 0.25, 
          patch_height = 0.25, label_position = "right", verbose = F, var.lab = paste0(variable)) 
{
  chloropleth_map <- copy(chloropleth_map)
  outline_map <- copy(outline_map)
  data <- copy(data)
  if (!(geog_id %in% names(chloropleth_map@data))) 
    stop("That geographic ID does not appear to exist within your chloropleth map object.")
  setnames(chloropleth_map@data, geog_id, "geog_id")
  if (!is.null(data)) {
    if (!(geog_id %in% names(data))) 
      stop("That geographic ID does not appear to exist within your data set.")
    if (!(variable %in% names(data) & !(variable %in% names(chloropleth_map@data)))) 
      stop("That variable does not appear to exist either your geometry or your data set.")
    if ((variable %in% names(data)) & (variable %in% names(chloropleth_map@data))) 
      stop("The variable you defined for color exists in both your geometry and the additional data set.")
    setnames(data, geog_id, "geog_id")
  }
  else {
    data <- copy(chloropleth_map@data)
    if (!(variable %in% names(data))) {
      stop("That variable does not appear to exist in the data attributes of your spatial object.")
    }
  }
  setnames(data, variable, "variable")
  if (is.factor(data[["variable"]]) | is.ordered(data[["variable"]])) {
    discrete_scale <- TRUE
  }
  else {
    discrete_scale <- FALSE
  }
  if (is.character(data[["variable"]])) {
    print("Character data being converted to a factor. If you'd like a specific order to your levels, please transform your variable into a factor or ordered data type with appropriate ordering. ")
    data[, `:=`(variable, as.factor(variable))]
    discrete_scale <- TRUE
  }
  if (discrete_scale & histogram) {
    histogram <- F
    print("Sorry, histogram/bar graph functionality is not yet supported in this version of the mapping suite for categorical data. No histogram will be printed.")
  }
  pallette <- colorRampPalette(color_ramp)
  color_list <- pallette(nlevels(data[["variable"]]))
  if (scramble_colors == T) {
    color_list <- color_list[sample(1:length(color_list))]
  }
  if (!is.null(series_dimension)) {
    if (!(series_dimension %in% names(data))) 
      stop("That series dimension (what you want to iterate through) does not appear to exist within your data set.")
    setnames(data, series_dimension, "series_dimension")
  }
  else {
    data[, `:=`(series_dimension, "*&^! no dimensions")]
  }
  if (is.null(series_sequence)) {
    map_dims <- unique(data$series_dimension)
  }
  else {
    for (select_dimension in series_sequence) {
      if (!(select_dimension %in% unique(data$series_dimension))) 
        stop(paste0("The dimension ", select_dimension, 
                    " does not appear to exist in the column you have specified to hold your dimensions."))
    }
    map_dims <- series_sequence
  }
  if (title_justification == "left") {
    title_justification <- 0
  }
  if (title_justification == "center") {
    title_justification <- 0.5
  }
  if (title_justification == "right") {
    title_justification <- 1
  }
  chloropleth_map <- data.table(suppressWarnings(fortify(chloropleth_map, 
                                                         region = "geog_id")))
  setnames(chloropleth_map, "id", "geog_id")
  if (!is.null(outline_map)) {
    outline_map <- data.table(suppressWarnings(fortify(outline_map)))
  }
  data <- data[, list(geog_id = as.character(geog_id), variable, 
                      series_dimension)]
  orig_rows <- nrow(chloropleth_map)
  chloropleth_map <- merge(data, chloropleth_map, by = "geog_id", 
                           allow.cartesian = T)
  after_rows <- nrow(chloropleth_map)
  if (orig_rows < after_rows & is.null(series_dimension)) 
    stop("You are trying to map more than one data observation per geometry, and you have not specified a series dimension to map over. Did you intend to subset your data further before passing it to this function?")
  if (!is.null(destination_folder)) {
    pdf(paste0(destination_folder, variable, additional_variable_name_string, 
               ".pdf"))
  }
  map_and_histogram_objects <- list()
  for (select_dimension in map_dims) {
    main_map_title <- map_title
    if (verbose) 
      print(main_map_title)
    if (select_dimension == "*&^! no dimensions") {
      main_map_subtitle <- map_subtitle
    }
    else {
      main_map_subtitle <- paste0(map_subtitle, " ", select_dimension)
    }
    if (verbose) 
      print(main_map_subtitle)
    subset <- copy(chloropleth_map[series_dimension == select_dimension])
    if (discrete_scale == F) {
      if (!is.null(override_scale)) {
        if (is.numeric(override_scale)) {
          minimum <- override_scale[1]
          maximum <- override_scale[2]
        }
        else {
          if (override_scale == "each_dimension") {
            maximum <- max(subset[["variable"]])
            minimum <- min(subset[["variable"]])
          }
          else {
            stop("Any character input other than 'each_dimension', which will produce a color ramp from the min/max of each dimension, is not recognized.")
          }
        }
      }
      else {
        maximum <- max(chloropleth_map[["variable"]])
        minimum <- min(chloropleth_map[["variable"]])
      }
      if (!is.null(diverging_centerpoint)) {
        if (diverging_centerpoint > maximum) {
          stop("The diverging centerpoint provided is greater than the maximum value in the data set.")
        }
        if (diverging_centerpoint < minimum) {
          stop("The diverging centerpoint provided is less than the minimum value in the data set.")
        }
        value_range <- maximum - minimum
        difference_from_minimum <- diverging_centerpoint - 
          minimum
        break_value <- difference_from_minimum/value_range
        color_value_breaks <- c(0, break_value, 1)
        if (verbose) 
          print(paste0("Centering color ramp at ", diverging_centerpoint, 
                       ". Any other color breaks provided have been overridden."))
      }
      map_plot <- ggplot(subset, aes(text = paste0("Country: ",geog_id, " , <br>", var.lab, ": ", format(variable, digits=2) ) )) + geom_polygon(aes(x = long, 
                                                    y = lat, group = group, fill = variable), color = chlor_lcol, 
                                                size = chlor_lsize) + scale_x_continuous("", 
                                                                                         breaks = NULL) + scale_y_continuous("", breaks = NULL) + 
        coord_fixed(ratio = 1) + labs(title = main_map_title, 
                                      subtitle = main_map_subtitle) + theme_tufte(base_size = fontsize, 
                                                                                  base_family = fontfamily) + theme(plot.title = element_text(hjust = title_justification), 
                                                                                                                    plot.subtitle = element_text(hjust = title_justification))
      if (!is.null(legend_breaks) & !is.null(legend_labels)) {
        map_plot <- map_plot + scale_fill_gradientn(colours = rev(color_ramp), 
                                                    limits = c(minimum, maximum), values = color_value_breaks, 
                                                    breaks = legend_breaks, labels = legend_labels)
      }
      else {
        map_plot <- map_plot + scale_fill_gradientn(colours = rev(color_ramp), 
                                                    limits = c(minimum, maximum), values = color_value_breaks)
      }
      if (legend_position %in% c("bottom", "top")) {
        map_plot <- map_plot + guides(fill = guide_colourbar(title = legend_name, 
                                                             title.position = "top", barheight = legend_bar_width, 
                                                             barwidth = legend_bar_length, label = TRUE, 
                                                             ticks = FALSE)) + theme(legend.position = legend_position, 
                                                                                     legend.title = element_text(size = legend_font_size))
      }
      if (legend_position %in% c("right", "left")) {
        map_plot <- map_plot + guides(fill = guide_colourbar(title = legend_name, 
                                                             title.position = "top", barheight = legend_bar_length, 
                                                             barwidth = legend_bar_width, label = TRUE, 
                                                             ticks = FALSE)) + theme(legend.position = legend_position, 
                                                                                     legend.title = element_text(size = legend_font_size))
      }
      if (legend_position %in% c("none")) {
        map_plot <- map_plot + theme(legend.position = "none")
      }
      if (histogram == TRUE) {
        if (!is.null(hist_color)) {
          hist_color_ramp <- hist_color
        }
        else {
          hist_color_ramp <- color_ramp
        }
        histo <- histogram_colorstats(datavector = subset$variable, 
                                      color_ramp = hist_color_ramp, minimum = minimum, 
                                      maximum = maximum, color_value_breaks = color_value_breaks, 
                                      dist_stats = dist_stats, mean_color = mean_color, 
                                      quantile_color = quantile_color)
      }
    }
    if (discrete_scale == T) {
      map_plot <- ggplot(na.omit(subset)) + geom_polygon(aes(x = long, 
                                                             y = lat, group = group, fill = variable), color = chlor_lcol, 
                                                         size = chlor_lsize) + scale_x_continuous("", 
                                                                                                  breaks = NULL) + scale_y_continuous("", breaks = NULL) + 
        coord_fixed(ratio = 1) + labs(title = main_map_title, 
                                      subtitle = main_map_subtitle) + theme_tufte(base_size = fontsize, 
                                                                                  base_family = fontfamily) + theme(plot.title = element_text(hjust = title_justification), 
                                                                                                                    plot.subtitle = element_text(hjust = title_justification))
      map_plot <- map_plot + scale_fill_manual(values = rev(color_list), 
                                               drop = FALSE)
      map_plot <- map_plot + guides(fill = guide_legend(title = legend_name, 
                                                        keywidth = patch_width, keyheight = patch_height, 
                                                        label.position = label_position)) + theme(legend.position = legend_position)
      if (legend_position %in% c("none")) {
        map_plot <- map_plot + theme(legend.position = "none")
      }
      if (histogram == TRUE) {
        histo <- ggplot(na.omit(subset), aes(x = variable,  
                                             fill = variable)) + geom_bar() + labs(x = NULL, 
                                                                                   y = NULL) + scale_fill_manual(values = rev(color_list)) + 
          theme_tufte(base_size = fontsize, base_family = fontfamily) + 
          theme(legend.position = "none", axis.ticks.x = element_blank(), 
                axis.ticks.y = element_blank()) + theme(plot.title = element_text(hjust = 0.5))
      }
    }
    map_plot <- map_plot + theme(plot.title = element_text(size = title_font_size, 
                                                           face = title_font_face)) + theme(legend.text = element_text(size = legend_font_size, 
                                                                                                                       face = legend_font_face))
    if (!is.null(outline_map)) {
      map_plot <- map_plot + geom_path(data = outline_map, text = geog_id,
                                       aes(x = long, y = lat, group = group), color = outline_color, 
                                       size = outline_size)
    }
    if (return_map_object_only == TRUE) {
      return(map_plot)
    }
    else {
      if (histogram == TRUE) {
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(5, 
                                                   1)))
        vplayout <- function(x, y) viewport(layout.pos.row = x, 
                                            layout.pos.col = y)
        print(map_plot, vp = vplayout(1:4, 1))
        print(histo, vp = vplayout(5, 1))
      }
      else {
        print(map_plot)
      }
    }
  }
  if (length(destination_folder) > 0) {
    dev.off()
    print("PDF ready to view.")
  }
}
