library(googledrive)
library(googlesheets4)
library(tidyverse)


gs4_auth("hernan.hernandez@uner.edu.ar")

#Cargo el csv

dataset <- read_sheet("https://docs.google.com/spreadsheets/d/1LAZuwHn2ONQu1JLniHgTai4B0Ny7Z4v7_KplqhAx0gw/edit?resourcekey#gid=2068206867")
