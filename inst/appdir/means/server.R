library(shiny)
library(ggvis)
library(dplyr)
library(phoneticChange)
library(beepr)

shinyServer(function(input, output, session) {

  startTime <- Sys.time()

  selected <- reactive(input$plt_vclass)
  dobrange <- reactive(input$dob_range)
  selectedData <- reactive({

    all_means %>%
      filter(plt_vclass %in% selected(),
             between(DOB, dobrange()[1], dobrange()[2]),
             n > 5)%>%
      mutate(subsystem = c(`i`="V", `e`="V", `ae`="V", `o`="V", `uh`="V", `u`="V", `*hr`="nurse",
                           `iy`="Vy", `ey`="Vy", `eyF`="Vy2", `ay`="Vy", `ay0`="Vy2", `oy`="Vy",
                           `iw`="Vw", `uw`="Vw", `Tuw`="Vw2", `ow`="Vw", `aw`="Vw",
                           `aeh`="Vh", `oh`="Vh",
                           `iyr`="Vr", `eyr`="Vr",`ahr`="Vr", `owr`="Vr", `uwr`="Vr")[as.character(plt_vclass)],
             cardinal = c(`i`="i", `e`="e", `ae`="ae", `o`="o", `uh`="uh", `u`="u", `*hr`="nurse",
                          `iy`="i", `ey`="e", `eyF`="e", `ay`="o", `ay0`="o", `oy`="uh",
                          `iw`="i", `uw`="u", `Tuw`="u", `ow`="uh", `aw`="ae",
                          `aeh`="ae", `oh`="o",
                          `iyr`="i", `eyr`="e",`ahr`="o", `owr`="uh", `uwr`="u")[as.character(plt_vclass)])
  })


  shapeMap <- c(`i` = "circle",
                `e` = "square",
                `ae` = "diamond",
                `o` = "cross",
                `uh` = "triangle-up",
                `u` = "triangle-down",
                `nurse` = "circle")
  shape_domain = names(shapeMap)
  shape_range = unname(shapeMap)


  colorMap <- c(`i` = "#d62728",#4
                `e` = "#2ca02c",#3
                `ae` = "#ff7f0e", #2
                `o` = "#1f77b4", #1
                `uh` = "#9467bd",#5
                `u` = "#e377c2", #6
                `nurse` = "#7f7f7f" #7
  )
  color_domain = names(colorMap)
  color_range = unname(colorMap)


  strokeMap <- c(`V` = "black",
                 `Vy` = "red",
                 `Vy2` = "purple",
                `Vw` = "blue",
                `Vw2` = "green",
                `Vh` = "#f1af0e  ",
                `Vr` = "#f1af0e  ",
                `nurse` = "brown"
  )
  stroke_domain = names(strokeMap)
  stroke_range = unname(strokeMap)

  subsystemMap = c(`i`="V", `e`="V", `ae`="V", `o`="V", `uh`="V", `u`="V", `*hr`="nurse",
                   `iy`="Vy", `ey`="Vy", `eyF`="Vy2", `ay`="Vy", `ay0`="Vy2", `oy`="Vy",
                   `iw`="Vw", `uw`="Vw", `Tuw`="Vw2", `ow`="Vw", `aw`="Vw",
                   `aeh`="Vh", `oh`="Vh",
                   `iyr`="Vr", `eyr`="Vr",`ahr`="Vr", `owr`="Vr", `uwr`="Vr")
  cardinalMap =  c(`i`="i", `e`="e", `ae`="ae", `o`="o", `uh`="uh", `u`="u", `*hr`="nurse",
                   `iy`="i", `ey`="e", `eyF`="e", `ay`="o", `ay0`="o", `oy`="uh",
                   `iw`="i", `uw`="u", `Tuw`="u", `ow`="uh", `aw`="ae",
                   `aeh`="ae", `oh`="o",
                   `iyr`="i", `eyr`="e",`ahr`="o", `owr`="uh", `uwr`="u")

  all_vclass = names(subsystemMap)
  all_domain = all_vclass
  use_color_range = unname(colorMap[cardinalMap[all_vclass]])
  use_shape_range = unname(shapeMap[cardinalMap[all_vclass]])
  use_stroke_range = unname(strokeMap[subsystemMap[all_vclass]])


  all_means %>%
    filter(n > 5)%>%
    filter(plt_vclass %in% c("i", "e", "ae", "o", "uh", "u", "*hr",
                             "iy", "ey", "eyF", "ay", "ay0", "oy",
                             "iw", "uw", "Tuw", "ow", "aw",
                             "aeh", "oh",
                             "iyr", "eyr","ahr", "owr", "uwr"))%>%
    ggvis(~F2_n, ~F1_n) %>%
    layer_points(size := 10, opacity := 0.4) %>%
    layer_points(data = selectedData%>%group_by(cardinal),
                 shape = ~plt_vclass,
                 fill = ~plt_vclass,
                 stroke = ~plt_vclass,
                 size := input_slider(label = "point size", 50,500,value = 50))%>%
    scale_numeric("y", reverse = T)%>%
    scale_numeric("x", reverse= T)%>%
    scale_ordinal("shape", range = use_shape_range, domain = all_domain)%>%
    scale_ordinal("fill", range = use_color_range, domain = all_domain)%>%
    scale_ordinal("stroke", range = use_stroke_range, domain = all_domain)%>%
    add_legend(c("shape","fill", "stroke"))%>%
    #set_options(duration = 0) %>%
    bind_shiny("ggvis", "ggvis_ui")



})
