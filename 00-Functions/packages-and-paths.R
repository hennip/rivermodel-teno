library(rjags)
library(runjags)
load.module("mix")
#library(xlsx)
library(tidyverse)
library(ggmcmc)
library(readxl)
library(forcats)
library(lubridate)
library(stringr)
require(gridExtra)

options(max.print=1000000)

source("00-Functions/tidy-functions.r")
source("00-Functions/my-palette.r")


# Path for input data
#pathIn<-"H:/Projects/ISAMA/data/der/input/Utsjoki-smolts/"

# Path for simulation output
pathOut<-"H:/Projects/ISAMA/prg/output/rivermodel-teno/"

source("01-Data/data-baltic-north.r")
source("01-Data/data-Utsjoki.r")
source("01-Data/data-Inari.r")
source("01-Data/data-combine.r")

