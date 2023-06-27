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


# Filtering needed variables 
nonbomb <- exam_year |> 
  filter(genus != "Bombus")

bombus <- exam_year |> 
  filter(genus == "Bombus")

lasio <- exam_year |> 
  filter(genus == "Lasioglossum") |> 
  filter(species == "hitchensi" | species == "imitatum" | species == "pilosum" | species == "versatum")

augo_pura <- exam_year |> 
  filter(genus == "Augochlora")
# assuming all Augochlora = Augochlora pura

cera <- exam_year |> 
  filter(genus == "Ceratina")

# Preparing data for glmm
netted_bees <- exam_year |>  # Based on all bees
  group_by(year, 
           farmcode,
           fdate) |> 
  summarise(count = n())

netted_b <- bombus |>  # Based on all Bombus
  group_by(year, 
           farmcode,
           fdate) |> 
  summarise(count_b = n()) 

netted_nb <- nonbomb |>  # Based on all non-bombus
  group_by(year, 
           farmcode,
           fdate) |> 
  summarise(count_nb = n())

netted_l <- lasio |>  # Based on all Lasioglossum
  group_by(year,
           farmcode,
           fdate) |> 
  summarise(count_l = n())

netted_ap <- augo_pura |>  # Based on all Augochlora
  group_by(year,
           farmcode,
           fdate) |> 
  summarise(count_ap = n())

netted_c <- cera |>  # Based on all Ceratina
  group_by(year, 
           farmcode,
           fdate) |> 
  summarise(count_c = n())

# Combining to one new dataset
merge1 <- merge(netted_bees, 
                netted_b,
                all.x = TRUE)
merge2 <- merge(merge1, 
                netted_nb,
                all.x = TRUE)
merge3 <- merge(merge2,
                netted_l, 
                all.x = TRUE)
merge4 <- merge(merge3, 
                netted_ap, 
                all.x = TRUE)
merge_all1 <- merge(merge4, 
                    netted_c, 
                    all.x = TRUE)
merge_all2 <- mutate_all(merge_all1,
                         ~replace_na(.,0))
#mutate(count_b = coalesce(count_b, 0))


# Fitting modells with glmmTMB
model_bees <- glmmTMB(count ~ year + (1|farmcode), # glmm all bees
                      family = nbinom2(link = "log"),
                      data = merge_all2)
summary(model_bees)

model_b <- glmmTMB(count_b ~ year + (1|farmcode), # glmm all bombus
                   family = nbinom2(link = "log"),
                   data = merge_all2)
summary(model_b)

model_nb <- glmmTMB(count_nb ~ year + (1|farmcode), # glmm all non-bombus wild bees
                    family = nbinom2(link = "log"),
                    data = merge_all2)
summary(model_nb)

model_l <- glmmTMB(count_l ~ year + (1|farmcode), # glmm all lasioglossum
                   family = nbinom2(link = "log"),
                   data = merge_all2)
summary(model_l)

model_ap <- glmmTMB(count_ap ~ year + (1|farmcode), # glmm all augochlora
                    family = nbinom2(link = "log"),
                    data = merge_all2)
summary(model_ap)

model_c <- glmmTMB(count_c ~ year + (1|farmcode), # glmm all ceratina
                   family = nbinom2(link = "log"),
                   data = merge_all2)
summary(model_c)
