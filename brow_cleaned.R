library(tidyverse)
library(janitor)
library(lubridate)

brow <- read_csv("data/broward_full_records.csv")

glimpse(brow)

brow <- brow |> filter(party_type == "Defendant") 

brow <- brow |> mutate(full_name = str_replace(full_name, 
                                 pattern = "^([^,]+),\\s*(.*)$", 
                                 replacement = "\\2 \\1"))
view(brow)

write_csv(brow, "data/broward_full_records.csv")
