#' Map Plotting Utility for Gridded Data
#'
#' @description Simplified plotting utilitity for evenly spaced points (raster data)
#'
#' @param geometry A data table with an x and a y column, as well as a geographic id.
#' @param xcol String; the name of the column that is the x values for the grid
#' @param ycol String; the name of the column that is the y values for the grid
#' @param geog_id string; the column name for the unique geogrpahic ID
#' @param variable string; the column with values you wich to plot
#' @param outline_map Another SpatialPolygons object that you want to use the
#'     outlines from. Make sure your outline map and main map have the same projection.
#' @param data A data.table that contains the data you want to map
#'    (must contain geog_id, and the variable of interest, if specified.
#'    If a series dimension and/or series sequence is defined,
#'    those must also exist in this data set)
#' @param histogram logical; the plot will contain a histogram of the values
#' @param hist_color If a character string for a color (or colors) are entered
#'    (ex:"grey"), the histogram will be that color rather than the color ramp
#'    used for the main map.
#' @param dist_stats Vertical lines on the histogram plot showing summary
#'    statistics. To show this, provide a vector of numeric values (between 0
#'    and 1) to serve as quantiles, and the options "mean" and "sd" can also be
#'    included. example: c("mean","sd",.1,.5,.9). Default=NULL.
#' @param mean_color The color of lines you want to represent mean and standard
#'    deviation statistics, only relevant if dist_stats!=NULL. Default="red".
#' @param quantile_color The color of lines you want to represent the median and
#'    quantile lines on the histogram, only relevant if dist_stats!=NULL.
#' @param return_map_object_only you can assign the function to a variable, and
#'    store the map plot portion of this ggplot object so that you can combine
#'    it with other graphics at will. This will never return the histogram.
#' @param destination_folder A string file path to a folder you want a PDF
#'    created in that will hold your map(s)
#' @param color_ramp A list of colors that will serve as the colors you
#'    "stretch" through based on your data values. This will default to a color
#'    scheme described in woodson pallettes called "Easter to Earth" that
#'    displays variation well when there are many geographic units. See woodson
#'    palletes for more options, or create your own.
#' @param outline_size A numeric value that specifies how large you want your
#'    white outlines to be if you have specified an outline you want shown on
#'    your map. Default value is .1.
#' @param outline_color What color you want the outline of the additional
#'    geography to be (if provided). This can be any color r recognizes
#'    suggestions might be "black","yellow", or "white". Default is white.
#' @param override_scale Values that will be used to stretch the color ramp
#'    instead of the min/max values present in the entire data set. Should
#'    either be structured "c(min,max)", with numeric values, or be
#'    "each_dimension", which will create a map series where each individual map
#'    in a series will based on the min/max from that subset of data.
#' @param color_value_breaks How you want the colors "stretched" across the
#'    range of minimum/maximum values. Default is NULL/ uniform distribution
#'    stretched across the color ramp from the minimum and maximum data values
#'    provided. Vector must begin with 0 and end with 1.
#' @param diverging_centerpoint Accepts any numeric value between the minimum
#'    and maximum of your data set. Sets the center of your color scheme to the
#'    value defined. This is meant to be used with diverging color schemes. It
#'    will override any previously defined color_value_breaks. Default=NULL.
#' @param map_title A string that serves as the basis for your map title (if no
#'    dimensions are specified, the title will be as it is specified. If a
#'    dimension is specified, a phrase constructed using the series dimension
#'    and what you are mapping will be added to the plot title [ex="year:1990"].
#' @param additional_variable_name_string This is an additonal string that you
#'    want to add to the name of the PDF to describe any other breakdowns you
#'    might be using. For example, if you had to map something by year, age,
#'    sex, you would first need to subset your data to be one age/sex group
#'    before plotting it out by year. If you subset your data in a loop, you
#'    could use this string to specify something along the lines of
#'    paste0("age_ ",a," _ sex _",s).
#' @param fontfamily The name of the font family you want to use for the text
#'     on the plot. Default is 'serif'.
#' @param fontsize The base/minimum size of the text on your graphic. 
#'       Default is NULL. 
#' @param title_justification Where ("left","center",or "right) you want the title 
#'        and subtitle. Default is "center".
#' @param title_font_size How large you want the title font to be. No default;
#'    default values based on ggthemes tufte()'s default.
#' @param title_font_face Special properties of the title font.
#'    Options include "plain", "bold", "italic". Default is plain.
#' @param series_dimension A string-- the name of the column that will serve as
#'    the variable you loop through to create a series map. For example, year.
#' @param series_sequence A vector c(x,y,z...) that specifies a subset of the
#'    series dimensions you want to map. For example, if you have a data set
#'    that contains all years between 1980-2014, you can specify that you only
#'    want to plot out every other year by setting series sequence to be
#'    seq(1980,2014,2). This function will make sure all of the items you
#'    speficy actually exist within your series_dimension.
#' @param legend_name Title above the legend. Default is NULL. 
#' @param legend_position Where you want the legend to go. Options are
#'    "top","bottom","right","left", and "none". Default is "bottom".
#' @param legend_font_size How large you want the legend font to be.
#'    No default; default values based on ggthemes tufte()'s default.
#' @param legend_font_face Special properties of the legend font. Options
#'    include "plain", "bold", "italic". Default is plain.
#' @param legend_bar_width How fat you want the color bar that serves as the
#'    legend to be. Default value is 0.4.
#' @param legend_bar_length How long you want the color bar that serves as the
#'    legend to be. Default value is 20.
#' @param legend_breaks An optional vector of the values you want to label in
#'    your legend's color scale.
#' @param legend_labels An optional vector of the character strings you want to
#'    use to label your legend's color scale (must be same length as
#'    legend_breaks)
#' @param scramble_colors Logical; Scrambles the input color ramp's values. 
#'      useful for categorical data.
#' @param patch_width width of color swatches in legend when categorical data
#'      is used. Default is .25. 
#' @param patch_height height of color swatches in legend when categorical data
#'      is used. Default is .25.        
#' @param label_position Position of category labels in legend when categorical
#'      data is used. Default= "right". 
#' @param verbose logical; Whether you want print statements from the function
#'               scramble_colors=FALSE,
#'
#' @return ggplot object or None (plots written to pdf)
#'
#' @examples see https://rpubs.com/BeccaStubbs/introduction_to_woodson_mapping_suite
#' 
#' @export

wmaprast<-function(geometry,
               xcol,
               ycol,
               geog_id,
               variable,
               
               # Optional data/geometry
               outline_map=NULL, # 
               data=NULL,
               
               # What elements of the map do you want the function to return?
               histogram=FALSE,
               hist_color=NULL,
               dist_stats=NULL,
               mean_color="red",
               quantile_color="black",
               return_map_object_only=FALSE,
               destination_folder=NULL,
               
               # Inputs for the color scheme of the maps
               color_ramp=wpal("easter_to_earth"),
               outline_size=.1,
               outline_color="white",
               override_scale=NULL,
               color_value_breaks=NULL,
               diverging_centerpoint=NULL,
               
               # Inputs for text
               map_title=" ",
               map_subtitle=NULL,
               title_justification="center",
               additional_variable_name_string=NULL,
               title_font_size=NULL,
               title_font_face="plain",
               
               # Fonts
               fontfamily="serif",
               fontsize=12,
               # Inputs for generating series-maps
               series_dimension=NULL,
               series_sequence=NULL,
               
               # Inputs for map Legend
               legend_name=NULL,
               legend_position="bottom", 
               legend_font_size=NULL,
               legend_font_face="plain",
               
               # Inputs for numeric data only
               legend_bar_width=.4,
               legend_bar_length=20,
               legend_breaks=NULL,
               legend_labels=NULL,
               
               # Inputs for categorical data only
               scramble_colors=FALSE,
               patch_width=.25,
               patch_height=.25,
               label_position="right",
               
               # Do you want print statements?
               verbose=F){      
  
  
  # Copying objects such that the original data sets are unaltered
  data_is_null<-is.null(data)
  geometry<-copy(geometry)
  setnames(geometry,xcol,"lon"); setnames(geometry,ycol,"lat")
  outline_map<-copy(outline_map)
  data<-copy(data)
  
  # Getting the data object ready to join to the fortified spatial object (either from the chloropleth map's data.table, or from an external data.table)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check to make sure the geog_id specified is within the chloropleth map's data table
  if(!(geog_id %in% names(geometry))) stop("That geographic ID does not appear to exist within your geometry data object.")
  setnames(geometry,geog_id,"geog_id")
  
  # Rename variables within data set
  if (!is.null(data)){ # if external data *IS* specified
    if(!(geog_id %in% names(data))) stop("That geographic ID does not appear to exist within your data set.")
    if(!(variable %in% names(data) & !(variable %in% names(geometry)))) stop("That variable does not appear to exist either your geometry or your data set.")
    if( (variable %in% names(data)) & (variable %in% names(geometry))) stop("The variable you defined for color exists in both your geometry and the additional data set.")

    setnames(data,geog_id,"geog_id")
  }else{ # If external data is NOT specified
    data<-copy(geometry)
    if(!(variable %in% names(data))){
      stop("That variable does not appear to exist in the data attributes of your spatial object.")
    }
  }
  setnames(data,variable,"variable")
  
  # checking whether the data is a factor/ordered: this will make it be mapped with a discrete scale.
  if (is.factor(data[["variable"]])|is.ordered(data[["variable"]])){discrete_scale<-TRUE}else{discrete_scale<-FALSE}
  if (is.character(data[["variable"]])){
    print("Character data being converted to a factor. If you'd like a specific order to your levels, please transform your variable into a factor or ordered data type with appropriate ordering. ")
    data[,variable:=as.factor(variable)]
    discrete_scale<-TRUE}
  if(discrete_scale&histogram){histogram<-F; print("Sorry, histogram/bar graph functionality is not yet supported in this version of the mapping suite for categorical data. No histogram will be printed.")}
  
  # Making sure that there are the right number of colors in the color ramp, by sampling and then scrambling (optional)
  # the color pallette chosen. 
  pallette<-colorRampPalette(color_ramp) 
  color_list<-pallette(nlevels(data[["variable"]]))
  if(scramble_colors==T){
    color_list<-color_list[sample(1:length(color_list))]
  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Defining "map dims"; the levels of the series dimension you want to map
  
  # First, discovering or defining the "series dimension"
  if (!is.null(series_dimension)){ # If you plan to loop through miltiple dimensions...
    if(!(series_dimension %in% names(data))) stop("That series dimension (what you want to iterate through) does not appear to exist within your data set.")
    setnames(data,series_dimension,"series_dimension")
  }else{
    data[,series_dimension:="*&^! no dimensions"]
  }
  
  # Restricting the mapping to only *some* levels of that dimension, if desired
  if (is.null(series_sequence)){
    map_dims<-unique(data$series_dimension)
  }else{ 
    for (select_dimension in series_sequence){ 
      if(!(select_dimension %in% unique(data$series_dimension))) stop(paste0("The dimension ",select_dimension," does not appear to exist in the column you have specified to hold your dimensions."))
    }
    map_dims<-series_sequence
  }
  
  # determining the "hjust" of the title
  if(title_justification=="left"){title_justification<-0}
  if(title_justification=="center"){title_justification<-.5}
  if(title_justification=="right"){title_justification<-1}
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fortifying the Map(s) and Joining on the Data
  
  # "fortifying" the Rdata shapefiles
    if (!is.null(outline_map)){outline_map<-data.table(suppressWarnings(fortify(outline_map)))} # If an outline map is specified, fortify the outline map as well.
  
  # creating one long, huge object that you can subset by merging together the data and the forfified geometry
   if(data_is_null==F){ # if there is extra data provided, merge it on, then sub-select down.
     data<-merge(data, geometry, by="geog_id", allow.cartesian=T)
     data<-data[, list(geog_id=as.character(geog_id), variable, lon, lat, series_dimension)]
   }else{ #if no additonal data is provided, subset down the data (in the geometry file) provided.
     data<-data[, list(geog_id=as.character(geog_id), variable, lon, lat, series_dimension)]
    }
  
  ###########################################
  ## LOOPING ACROSS DIMENSIONS
  ########################################### 
  # Starting a PDF, if desired
  if (!is.null(destination_folder)){pdf(paste0(destination_folder,variable,additional_variable_name_string,".pdf"))}
  # Creating a list in which to store the map and histogram
  map_and_histogram_objects<-list()
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Starting the Loop
  for (select_dimension in map_dims){ #for each dimension you want to plot...
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Determining map title
    main_map_title<-map_title
    if(verbose) print(main_map_title)
    
    # Determining map subtitle
    if (select_dimension=="*&^! no dimensions"){
      main_map_subtitle<-map_subtitle
    }else{
      main_map_subtitle<-paste0(map_subtitle," ",select_dimension)}
    if(verbose) print(main_map_subtitle)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Subsetting the Data
    subset<-copy(data[series_dimension==select_dimension]) # Sub-setting the fortified object to map out 1 layer/dimension (ex: year) of the variable of interest  
    
    
    #####################
    # If Data is Numeric
    #####################
    
    if(discrete_scale==F){ # If the data is numeric... 
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Defining Boundaries and Properties of the Color Ramp
      
      # If an override was provided, setting minimum to the first in the list, and maximum to the second in the list provided.
      if (!is.null(override_scale)){
        if(is.numeric(override_scale)){
          minimum<-override_scale[1]
          maximum<-override_scale[2]
        }else{
          if(override_scale=="each_dimension"){
            maximum<-max(subset[["variable"]])
            minimum<-min(subset[["variable"]])
          }else{stop("Any character input other than 'each_dimension', which will produce a color ramp from the min/max of each dimension, is not recognized.")}
        }
      }else{ #Otherwise, set the min/max of the scale to the min/max of ALL dimensions of the variable.
        maximum<-max(data[["variable"]])
        minimum<-min(data[["variable"]])
      }
      
      # Determining the diverging centerpoint in relation to the min/max, if desired
      if(!is.null(diverging_centerpoint)){
        if(diverging_centerpoint>maximum){stop("The diverging centerpoint provided is greater than the maximum value in the data set.")}
        if(diverging_centerpoint<minimum){stop("The diverging centerpoint provided is less than the minimum value in the data set.")}
        # Finding what where the specified break point is as a fraction of the total color range 
        value_range<-maximum-minimum
        difference_from_minimum<-diverging_centerpoint-minimum
        break_value<-difference_from_minimum/value_range
        color_value_breaks<-c(0,break_value,1)
        if(verbose) print(paste0("Centering color ramp at ",diverging_centerpoint,". Any other color breaks provided have been overridden."))
      }
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Creating the Map Plot in GGPlot2
      
      map_plot<-ggplot(subset) + 
        geom_raster(aes(x=lon, y=lat, fill=variable)) +
        scale_x_continuous("", breaks=NULL) + 
        scale_y_continuous("", breaks=NULL) + 
        coord_fixed(ratio=1)+
        labs(title = main_map_title, subtitle=main_map_subtitle) +
        theme_tufte(base_size = fontsize, base_family = fontfamily)+
        theme(plot.title=element_text(hjust = title_justification),plot.subtitle=element_text(hjust = title_justification))
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Adding the color ramp!
      
      if(!is.null(legend_breaks)&!is.null(legend_labels)){
        map_plot<-map_plot+scale_fill_gradientn(colours=rev(color_ramp), 
                                                limits=c(minimum, maximum),
                                                values=color_value_breaks, 
                                                breaks=legend_breaks, 
                                                labels=legend_labels)
      }else{
        map_plot<-map_plot+scale_fill_gradientn(colours=rev(color_ramp), 
                                                limits=c(minimum, maximum), 
                                                values=color_value_breaks) 
      } # Why have this if-clause? If the value of legend_breaks is NULL, then you end up not getting a legend at all. Lame!
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Adding a legend
      if (legend_position %in% c("bottom","top")){
        map_plot<-map_plot+
          guides(fill=guide_colourbar(title=legend_name, title.position="top", barheight=legend_bar_width, barwidth=legend_bar_length, label=TRUE, ticks=FALSE )) + 
          theme(legend.position=legend_position,legend.title=element_text(size=legend_font_size))} 
      if (legend_position %in% c("right","left")){
        map_plot<-map_plot+
          guides(fill=guide_colourbar(title=legend_name, title.position="top", barheight=legend_bar_length, barwidth=legend_bar_width, label=TRUE, ticks=FALSE )) +
          theme(legend.position=legend_position,legend.title=element_text(size=legend_font_size))} 
      if (legend_position %in% c("none")){
        map_plot<-map_plot+theme(legend.position="none")
      }
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Making a histogram of the distribution of that dimension's values
      
      if (histogram==TRUE){ # If you have specified that you do want the histogram at the bottom:
        if(!is.null(hist_color)){hist_color_ramp<-hist_color}else{hist_color_ramp<-color_ramp}
        
        histo<-histogram_colorstats(datavector=subset$variable,
                                    color_ramp=hist_color_ramp,
                                    minimum=minimum,
                                    maximum=maximum,
                                    color_value_breaks=color_value_breaks,
                                    dist_stats=dist_stats,
                                    mean_color=mean_color,
                                    quantile_color=quantile_color)
        
      }# If histogam==T
      
    } # if data is numeric
    
    
    #################################
    # If Data is Categorical/Ordinal
    #################################
    
    if (discrete_scale==T){
      map_plot<-ggplot(na.omit(subset)) + 
        geom_raster(aes(x=lon, y=lat, fill=variable)) +
        scale_x_continuous("", breaks=NULL) + 
        scale_y_continuous("", breaks=NULL) + 
        coord_fixed(ratio=1)+
        labs(title = main_map_title, subtitle=main_map_subtitle) +
        theme_tufte(base_size = fontsize, base_family = fontfamily)+
        theme(plot.title=element_text(hjust = title_justification),plot.subtitle=element_text(hjust = title_justification))
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Adding the color ramp!
      
      map_plot<-map_plot+scale_fill_manual(values=rev(color_list),drop = FALSE)
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Adding a legend
      map_plot<-map_plot+guides(fill=guide_legend(title=legend_name,
                                                  keywidth=patch_width,
                                                  keyheight=patch_height,
                                                  label.position = label_position))+
        theme(legend.position=legend_position)
      
      if (legend_position %in% c("none")){
        map_plot<-map_plot+theme(legend.position="none")
      }
      
      
      # Adding a "histogram" (really, in this case, a bar chart) to the bottom of the image: This is in BETA and is not currently a funcitonality in v 1.1. 
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (histogram==TRUE){ # If you have specified that you do want the histogram at the bottom:          
        #histo<-qplot(variable, data=subset, geom="bar", fill=variable)+scale_fill_manual(values=color_list)
        histo<-ggplot(na.omit(subset), aes(x=variable, fill=variable)) +
          geom_bar() + 
          labs(x=NULL, y=NULL) +
          scale_fill_manual(values=rev(color_list))+theme_tufte(base_size = fontsize, base_family = fontfamily)+theme(legend.position="none",
                                                                                                                      axis.ticks.x=element_blank(),
                                                                                                                      axis.ticks.y=element_blank())+theme(plot.title=element_text(hjust = 0.5))
      }# If histogam==T
      
    } # if it's ordinal/categorical
    
    #######################################
    # For All Data Types
    #######################################
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Adding Title and Legend Formatting
    map_plot<-map_plot +  theme(plot.title = element_text(size = title_font_size,  face=title_font_face))+ #Adding custom title that might override the legend stuff
      theme(legend.text = element_text(size = legend_font_size, face=legend_font_face))
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Adding Outline Map, if desired
    if (!is.null(outline_map)){
      map_plot<-map_plot+geom_path(data = outline_map, 
                                   aes(x = long, y = lat, group = group),
                                   color = outline_color, size = outline_size)} 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## If you just want the map plot as an object you can pass to other things...
    if (return_map_object_only==TRUE){return(map_plot)}else{
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Printing the Plot:
      if (histogram==TRUE){# Combining Histogram and Map to plot into a single image.
        grid.newpage() # Starting a new page
        pushViewport(viewport(layout = grid.layout(5, 1))) # Defining the ratio of the histogram to map to be 5 sections vertically, 1 horizontally
        vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y) # Defining a function that allows setting the layout of the viewport 
        print(map_plot, vp = vplayout(1:4, 1)) # Printing the map plot to the viewport in vertical slots 1-4, covering all the x-space
        print(histo, vp = vplayout(5, 1)) # Printing the histogram to the bottom of the map: 
      }else{print(map_plot)} #If you didn't want the histogram, just print out the map!
    } # Closing the "if return map object=TRUE" clause
  } # Closing the loop of dimensions
  if (length(destination_folder)>0){dev.off();print("PDF ready to view.")} #If you were writing this to a PDF, you can close it, and check it out!
} # Closing Function!

