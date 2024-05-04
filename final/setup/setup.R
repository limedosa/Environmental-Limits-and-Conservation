# This R script automatically downloads/loads the weplot function and improves PDF formatting


# Makes it so that all chunks that follow will have wrapped text in knitted PDFs
library(formatR)
knitr::opts_chunk$set(message=TRUE, tidy.opts=list(width.cutoff=85), tidy=TRUE) 

# Downloads latest version of weplot
download.file(url = "https://raw.githubusercontent.com/AldenGriffith/weplot/main/current-version/weplot.R",
              destfile= "setup/weplot.R",
              quiet = TRUE)

# Loads the weplot functions
source("setup/weplot.R")
