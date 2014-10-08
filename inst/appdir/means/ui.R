library(phoneticChange)
library(ggvis)
library(beepr)
shinyUI(pageWithSidebar(

  headerPanel("Philadelphia Neighborhood Corpus Means"),

  sidebarPanel(
    checkboxGroupInput("plt_vclass", "Vowels to highlight",
                       choices = (c("i", "e", "ae", "o", "uh", "u", "*hr",
                                    "iy", "ey", "eyF", "ay", "ay0", "oy",
                                    "iw", "uw", "Tuw", "ow", "aw",
                                    "aeh", "oh",
                                    "iyr", "eyr","ahr", "owr", "uwr")))
  ),
  mainPanel(
    uiOutput("ggvis_ui"),
    ggvisOutput("ggvis"),
    sliderInput("dob_range", label = "Date of Birth Range",
                format = "###0.#####",
                min = 1889, max = 1998, c(1889, 1998), step = 1)
  )
))

