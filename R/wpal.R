#' Woodson Palettes
#'
#' @description A suite of color ramps designed for beautiful chloropleth maps and other
#' data visualizations, especially where value differentiation over a wide range is desired.
#'
#' @param color Name of the color ramp. Default is NULL, which will return the whole list,
#' so you should probably enter in a color name! Use the funciton view_wpal() to see your options.
#' 
#' @param n How many colors are returned from the ramp. Default is NULL, which will provide the number
#' of colors generated when the ramp is created.
#' 
#' @param noblack Logical; Defines whether the color black is to be excluded from your color ramp. Default value is FALSE. 
#' @export 
#' 
#' @return A list of color values.
#' 
#' @examples wpal("cool_toned")


#http://www.mrao.cam.ac.uk/~dag/CUBEHELIX/cubewedges.html for more info on cubehelix

wpal<-function(color=NULL,n=NULL,noblack=FALSE){
  wpal<-list()
  wpal[["easter_to_earth"]]<-c("#000000", "#5b2c44", "#826737", "#67af6d", "#90c6de", "#ffff4c")
  wpal[["cool_toned"]]<-c("#000000", "#3E350C", "#9D4B60" , "#AB86D0" ,"#97E4DF","#ffff4c")
  wpal[["bright_greens"]]<-(cubeHelix(8, start = -0.5, r = -.3, hue = 3, gamma = 1.5)[2:7]) #bright_greens
  wpal[["bright_fire"]]<-(cubeHelix(8, start = .5, r = .4, hue = 3, gamma = 1.3)[2:7]) # Bright fire
  wpal[["eggplant_to_teal"]]<-(cubeHelix(8, start = .95, r = -.7, hue = 3, gamma = 1.3)[2:7]) # eggplant_to_teal
  wpal[["roygbiv"]]<-c("#3F004C","#8D1026","#DC2000","#ED6500","#FFAA00","#FFD400","#FFFF00","#BDFD69","#7CFBD3","#46EFDF","#11E3EB")
  
# Small Rotation colors (for low-n data)
  wpal[["cool_blue_deepindigo"]]<-cubeHelix(8, start = -2.8, r = -.15, hue = 2.5, gamma = 2)[2:7]
  wpal[["cool_blue_aqua"]]<-cubeHelix(8, start = -.35, r = -.15, hue = 2.5, gamma = 1.3)[2:7]
  wpal[["cool_blue_bright"]]<-cubeHelix(8, start = 0, r = -.15, hue = 2.5, gamma = 1.3)[2:7]
  wpal[["cool_blue_steel"]]<-cubeHelix(8, start = 2.1, r = .15, hue = 1.5, gamma = 1.4)[2:7]
  wpal[["cool_blue_jeans"]]<-cubeHelix(8, start = 2.45, r = .15, hue = 1.5, gamma = 1.4)[2:7]
  wpal[["cool_green_grassy"]]<-cubeHelix(8, start = -1.05, r = -.15, hue = 2.5, gamma = 1.3)[2:7]
  wpal[["cool_green_happy"]]<-cubeHelix(8, start = -.7, r = -.15, hue = 2.5, gamma = 1.3)[2:7]
  wpal[["cool_green_deepforest"]]<-cubeHelix(8, start = 1.4, r = .15, hue = 1.5, gamma = 1.4)[2:7]
  wpal[["cool_green_deeplake"]]<-cubeHelix(8, start = 1.75, r = .15, hue = 1.5, gamma = 1.4)[2:7]
  wpal[["warm_fire"]]<-(cubeHelix(8, start = .5, r = .3, hue = 3, gamma = 1.3)[2:7]) 
  wpal[["warm_darkfire"]]<-(cubeHelix(8, start = .5, r = .3, hue = 3, gamma = 2)[2:7]) 
  wpal[["warm_purple1"]]<-cubeHelix(8, start = -2.45, r = -.15, hue = 2.5, gamma = 1.3)[2:7]
  wpal[["warm_purple2"]]<-cubeHelix(8, start = 0, r = .15, hue = 1.5, gamma = 1.4)[2:7]
  wpal[["warm_adultpink"]]<-cubeHelix(8, start = 0.35, r = .15, hue = 1.5, gamma = 1.4)[2:7]
  wpal[["warm_kidpink"]]<-cubeHelix(8, start = -2.1, r = -.15, hue = 2.5, gamma = 1.3)[2:7]
  wpal[["warm_redpink"]]<-cubeHelix(8, start = -1.75, r = -.15, hue = 2.5, gamma = 1.3)[2:7]
  wpal[["warm_mauve"]]<-cubeHelix(8, start = 0.7, r = .15, hue = 1.5, gamma = 1.4)[2:7]
  wpal[["warm_darkpeach"]]<-cubeHelix(8, start = 0.7, r = .2, hue = 2, gamma = 1.7)[2:7]
  wpal[["warm_brown_happy"]]<-cubeHelix(8, start = 1.05, r = .15, hue = 1.5, gamma = 1.4)[2:7]
  wpal[["classy_earth"]]<-cubeHelix(11, start = .5, r = .8, hue = .5, gamma = 1)[1:10]
  wpal[["stormy_seas"]]<-cubeHelix(11, start = 1, r = .8, hue = .5, gamma = 1)[1:10]
  
# Half-Rotation Colors
  wpal[["purple_to_sea_green"]]<-cubeHelix(11, start = 0.5, r = -.5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["thanksgiving"]]<-cubeHelix(11, start = 0.5, r = .5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["purple_to_lavender"]]<-cubeHelix(11, start = 1, r = -.5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["brown_to_sea_green"]]<-cubeHelix(11, start = 1, r = .5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["brown_to_pink"]]<-cubeHelix(11, start = 1.5, r = -.5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["green_to_lavender"]]<-cubeHelix(11, start = 1.5, r = .5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["green_to_salmon"]]<-cubeHelix(11, start = 2, r = -.5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["sea_green_to_pink"]]<-cubeHelix(11, start = 2, r = .5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["deep_blue_to_pink"]]<-cubeHelix(11, start = 2.5, r = -.5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["deep_blue_to_pink"]]<-cubeHelix(11, start = 2.5, r = .5, hue = 1.5, gamma = 1)[1:10] 
  
#  .8 rotation colors
  wpal[["brown_green_blue_pink"]]<-cubeHelix(11, start = 1, r = .8, hue = 1.5, gamma = 1)[1:10] 
  wpal[["red_green_blue"]]<-cubeHelix(11, start = .5, r = .8, hue = 1.5, gamma = 1)[1:10] 
  wpal[["purple_red_green"]]<-cubeHelix(11, start = 0, r = .8, hue = 1.5, gamma = 1)[1:10] 
  wpal[["sea_green_purple_tan"]]<-cubeHelix(11, start = 2, r = .8, hue = 1.5, gamma = 1)[1:10] 
  
# A bunch of different, single-rotation color helix patterns:
  wpal[["black_to_light_1"]]<-cubeHelix(11, start = 0, r = -1, hue = 1, gamma = 1)[1:10]
  wpal[["black_to_light_2"]]<-cubeHelix(11, start = .5, r = -1, hue = 1, gamma = 1)[1:10]
  wpal[["black_to_light_3"]]<-cubeHelix(11, start = 1, r = -1, hue = 1, gamma = 1)[1:10]
  wpal[["black_to_light_4"]]<-cubeHelix(11, start = 1.5, r = -1, hue = 1, gamma = 1)[1:10]  
  wpal[["black_to_light_5"]]<-cubeHelix(11, start = 2, r = -1, hue = 1, gamma = 1)[1:10]  
  wpal[["black_to_light_6"]]<-cubeHelix(11, start = 0, r = 1, hue = 1.5, gamma = 1)[1:10]  
  wpal[["black_to_light_7"]]<-cubeHelix(11, start = .5, r = 1, hue = 1.5, gamma = 1)[1:10]  
  wpal[["black_to_light_8"]]<-cubeHelix(11, start = 1, r = 1, hue = 1.5, gamma = 1)[1:10]  
  wpal[["black_to_light_9"]]<-cubeHelix(11, start = 1.5, r = 1, hue = 1.5, gamma = 1)[1:10]  
  wpal[["black_to_light_10"]]<-cubeHelix(11, start = 2, r = 1, hue = 1.5, gamma = 1)[1:10]  
  wpal[["black_to_light_11"]]<-cubeHelix(11, start = 0, r = 1, hue = .5, gamma = 1)[1:10]  
  wpal[["black_to_light_12"]]<-cubeHelix(11, start = .5, r = 1, hue = .5, gamma = 1)[1:10]  
  wpal[["black_to_light_13"]]<-cubeHelix(11, start = 1, r = 1, hue = .5, gamma = 1)[1:10]  
  wpal[["black_to_light_14"]]<-cubeHelix(11, start = 1.5, r = 1, hue = .5, gamma = 1)[1:10]
  wpal[["black_to_light_15"]]<-cubeHelix(11, start = 2, r = 1, hue = .5, gamma = 1)[1:10]
  
  
# Diverging Pallettes Based on Intensity Pallettes
  wpal[["purple_to_green_diverging_intensity"]]<-append(rev(intensity_pallette(start=.2,r=.4,gamma=1)), intensity_pallette(start=1.75,r=.5,gamma=1))
  wpal[["blue_to_red_intensity"]]<-append(rev(intensity_pallette(start=.5,r=-.5,gamma=1)), intensity_pallette(start=0.5,r=.5,gamma=1))
  
# Diverging from black
  wpal[["pink_blue_multi_diverging_from_black"]]<-append(rev(cubeHelix(11, start = 1.5, r = -.4, hue = 1.75, gamma = 1)[1:9]), cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[1:10])
  wpal[["orange_blue_diverging_from_black"]]<-append(rev(cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[1:9]), cubeHelix(11, start = .5, r = .4, hue = 1.75, gamma = 1)[1:10])
  wpal[["green_purple_diverging_from_black"]]<-append(rev(cubeHelix(11, start = 0, r = -.4, hue = 1.75, gamma = 1)[1:9]), cubeHelix(11, start = 1, r = -.4, hue = 1.75, gamma = 1)[1:10])
  wpal[["tan_green_multi_diverging_from_black"]]<-append(rev(cubeHelix(11, start = 2, r = -.4, hue = 1.75, gamma = 1)[1:9]), cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[1:10])
  
# Diverging from white
  wpal[["pink_blue_multi_diverging_from_white"]]<-append(cubeHelix(11, start = 1.5, r = -.4, hue = 1.75, gamma = 1)[3:11], rev(cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[3:10]))
  wpal[["purple_blue_diverging_from_white"]]<-append(cubeHelix(11, start = 1, r = -.4, hue = 1.75, gamma = 1)[3:11], rev(cubeHelix(11, start = 0, r = -.4, hue = 1.75, gamma = 1)[3:10]))
  wpal[["tan_green_multi_diverging_from_white"]]<-append(cubeHelix(11, start = 2, r = -.4, hue = 1.75, gamma = 1)[3:11], rev(cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[3:10]))
 
# Diverging from Colors 
  wpal[["green_pink_diverging_from_purple"]]<-append(rev(cubeHelix(11, start = 3, r = -.4, hue = 1.75, gamma = 1)[3:10]), (cubeHelix(11, start = 3, r = .4, hue = 1.75, gamma = 1)[3:10]))
  wpal[["orange_blue_diverging_from_purple"]]<-append(rev(cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[3:10]), (cubeHelix(11, start = .5, r = .4, hue = 1.75, gamma = 1)[3:10]))
  wpal[["tan_blue_multi_diverging_from_green"]]<-append(rev(cubeHelix(11, start = 2, r = -.4, hue = 1.75, gamma = 1)[3:10]), (cubeHelix(11, start = 2, r = .4, hue = 1.75, gamma = 1)[3:10]))
  
  
  wpal[["green_purple_diverting_from_blue"]]<-append(rev(cubeHelix(11, start = 2.5, r = -.4, hue = 1.75, gamma = 2)[3:10]), 
                                                     (cubeHelix(11, start = 2.5, r = .4, hue = 1.75, gamma = 2)[3:10]))
  
  wpal[["pink_green_diverging_from_brown"]]<-append(rev(cubeHelix(11, start = 1.5, r = -.2, hue = 1.75, gamma = 1)[3:10]), 
                                                    (cubeHelix(11, start = 1.5, r = .2, hue = 1.75, gamma = 1)[3:10]))
  
  wpal[["blue_pink_divering_from_green"]]<-append(rev(cubeHelix(11, start = .3, r = -.2, hue = 1.75, gamma = 1.6)[3:10]), 
                                                  (cubeHelix(11, start = .3, r = .2, hue = 1.75, gamma = 1.6)[3:10]))
  
  wpal[["blue_pink_diverging_from_light_purple"]]<-append((cubeHelix(11, start = .3, r = -.2, hue = 1.75, gamma = 1.6)[3:10]), 
                                                          rev(cubeHelix(11, start = .3, r = .2, hue = 1.75, gamma = 1.6)[3:10]))
  
  wpal[["pink_green_diverging_from_light"]]<-append((cubeHelix(11, start = 1.3, r = -.5, hue = 1.75, gamma = 1)[3:10]), 
                                                    rev(cubeHelix(11, start = 1.3, r = .5, hue = 1.75, gamma = 1)[3:10]))
  
# Intensity Pallettes  
    wpal[["tan_to_red_intensity"]]<-intensity_pallette(start=0.5,r=.5,gamma=1)
    wpal[["lavender_to_deep_green_intensity"]]<-intensity_pallette(start=1.75,r=.5,gamma=1)
    wpal[["pink_to_purple_intensity"]]<-intensity_pallette(start=3,r=.5,gamma=1)
    wpal[["mild_green_to_dark_brown_intensity"]]<-intensity_pallette(start=1,r=.5,gamma=1)
    wpal[["lavender_green_dark_brown_intensity"]]<-intensity_pallette(start=1,r=.75,gamma=1)
    wpal[["sea_green_to_purple_intensity"]]<-intensity_pallette(start=.5,r=-.5,gamma=1)
    wpal[["pastel_to_purple_intensity"]]<-intensity_pallette(start=.2,r=.4,gamma=1)
    wpal[["sea_green_to_blue_intensity"]]<-intensity_pallette(start=3,r=-.4,gamma=1)
  
  if (!is.null(color)){ # if a specific color is requested
    
    requested_ramp<-wpal[[color]]
    # Eliminating black from the color ramp, if requested.
    
    if(noblack){ # if the person has specified that they don't want pure black included in the color scheme:
      requested_ramp<-wpal[[color]]
      requested_ramp<-copy(requested_ramp[!(requested_ramp %in% c("#000000"))]) # Eliminating black from the color ramp
    }
    if (!is.null(n)){ #If a specific number of colors are requested to be sampled
      # Creating a colorRampPallette function capable of interpolating N colors from the pallette
      pallette<-colorRampPalette(requested_ramp) 
      # Sampling n numbers of colors from that pallette (default will be 11)
      color_list<-pallette(n)
      return(color_list)
    }else{return(requested_ramp)}
    
  }else{ # If a specific color is *not* requested, return the whole list.
    return(wpal)}
  
} # End woodson pallettes storage function.
