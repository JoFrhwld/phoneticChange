library(shiny)
library(ggvis)
library(moments)
library(dplyr)
shinyServer(function(input, output, session) {
  # A reactive subset of mtcars

  mixture <- reactive(input$mixture)
  nsamp <- 50000
  distribution <- reactive({
    data.frame(F1_n = c(rnorm(round(nsamp*(1-input$mixture)),
                              mean = 1.3, sd = 0.5),
                        rnorm(round(nsamp*input$mixture),
                              mean = 0.28, sd = 0.5)),
               dist = c(rep("low", round(nsamp*(1-input$mixture))),
                        rep("raised", round(nsamp*input$mixture))),
               mix = c(rep(1-input$mixture, round(nsamp*(1-input$mixture))),
                       rep(input$mixture, round(nsamp*(input$mixture)))))
  })

  dist_info <- reactive({
    dist <- distribution()
    dist %>%
      summarise(mean = mean(F1_n),
                sd = sd(F1_n),
                kurtosis = kurtosis(F1_n))%>%
      mutate(dist = "mix", mix = NA)-> mixinfo

    dist%>%
      group_by(dist)%>%
      summarise(mean = mean(F1_n),
                sd = sd(F1_n),
                kurtosis = kurtosis(F1_n),
                mix = mean(mix))->dists

    rbind_all(list(mixinfo, dists))
  })
  #     distribution <- function(mix){
  #       data.frame(F1_n = c(rnorm(round(20000*mix), mean = 1.3, sd = 0.5),
  #                           rnorm(round(20000*(1-mix)), mean = 0.28, sd = 0.5)))
  #     }

  distribution %>%
    ggvis(~F1_n) %>%
    layer_densities(stroke := "red") %>%
    group_by(dist)%>%
    layer_densities(fill = ~dist)%>%
    scale_numeric("y", domain = c(0,0.8))%>%
    scale_numeric("x", domain = c(-2, 4), reverse= T)%>%
    scale_ordinal("fill", domain = c("low","raised"))%>%
    bind_shiny("ggvis", "ggvis_ui")


  output$info <- renderPrint({
    dist_info()
  })
})
