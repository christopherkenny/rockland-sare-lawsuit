suppressMessages({
  # general purpose ----
  library(here)
  library(tidyverse)
  library(cli)
  library(fs)
  library(readxl)
  library(dataverse)
  library(gt)
  
  # spatial ----
  library(sf)
  
  # alarm project ----
  library(ggredist)
  library(geomander)
  
  # plotting ----
  library(patchwork)
})

walk(Sys.glob(here('R/utils/*.R')), source)
