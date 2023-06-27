library(tidyverse)
library(tidyr)
library(lme4)
library(glmmTMB)
library(ggplot2)
library(conflicted)
library(usethis)

exam_data <- read.csv("watermelon_net.csv") |> 
  janitor::clean_names()
conflicts_prefer(dplyr::filter)

exam_year <- exam_data |> 
  mutate(years = mdy(fdate)) |> 
  mutate(year = year(years)) |>
  select("year",
         "plant",
         "genus",
         "farmcode",
         "fdate",
         "species") |> 
  filter(year > "2004")


use_github()
