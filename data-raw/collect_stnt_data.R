
library(tidyverse)

deep_hollow_bedding <- read_tsv(
  "data-raw/deep_hollow_bedding.txt",
  col_types = cols(
    .default = col_double(),
    `Dip Quad` = col_character(),
    Units = col_character()
  )
) %>%
  mutate(type = "bedding")

deep_hollow_cleavage <- read_tsv(
  "data-raw/deep_hollow_cleavage.txt",
  col_types = cols(
    .default = col_double(),
    `Dip Quad` = col_character(),
    Units = col_character()
  )
) %>%
  mutate(type = "cleavage")

deep_hollow <- bind_rows(deep_hollow_bedding, deep_hollow_cleavage) %>%
  select(type, strike = Strike, dip = Dip, dip_quad = `Dip Quad`)

usethis::use_data(deep_hollow, overwrite = TRUE)

