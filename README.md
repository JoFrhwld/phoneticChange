# To Get the Talk

    install.packages(c("devtools", "knitr"))
    library(devtools)
    install_github("jjallaire/revealjs")
    install_github("jofrhwld/phoneticChange", build_vignettes = T)
    library(phoneticChange)
    vignette("talk")
    
# List of data sets available
    data(package = "phoneticChange")