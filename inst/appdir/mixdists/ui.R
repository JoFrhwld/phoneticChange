library("ggvis")
shinyUI(pageWithSidebar(
  div(),
  sidebarPanel(
    sliderInput(inputId = "mixture", 
                label = "mixture", 
                min = 0, 
                max = 1, 
                step = 0.1,
                value = 0,
                animate = animationOptions(interval = 2000))
  ),
  mainPanel(
    uiOutput("ggvis_ui"),
    ggvisOutput("ggvis"),
    tableOutput("info")
  )
))

