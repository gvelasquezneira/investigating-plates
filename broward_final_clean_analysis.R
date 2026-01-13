library(tidyverse)
library(janitor)

cleaned_brow <- read_csv("broward_cleaned_v2.csv")
uncleaned <- read_csv("broward_full_records.csv")

glimpse(cleaned_brow)

uniques <- cleaned_brow |> count(case_number)
# There are 82 unique cases here

print(uniques) 

glimpse(uncleaned)

un_uniques <- uncleaned |> count(case_number)

print(un_uniques) 

# There are 90 cases here, meaning we have a slight mismatch in case numbers
glimpse(uncleaned)

uncleaned_names <- uncleaned |>  count(full_name)

print(uncleaned_names)


# cleaning the uncleaned names
name_counts <- uncleaned |>
  filter(party_type == "Defendant") |> 
  mutate(
    full_name_clean = full_name |> 
      str_remove(" ID#.*$") |> 
      str_replace_all("\\s+,", ",") |> 
      str_replace_all(",\\s*", ", ") |> 
      str_remove(",\\s*$") |> 
      str_trim() |> 
      str_to_title()
  ) |>
  group_by(full_name_clean) |>
  summarise(
    times_charged = n(),
    cases = paste(unique(case_number), collapse = ", ")
  ) |>
  arrange(desc(times_charged))

print(name_counts)

# Pivoting to charge_descriptions for more accurate counts

analysis_data <- uncleaned |> select(case_number, charge_descriptions, party_type) |> 
  filter(party_type == "Defendant")

glimpse(analysis_data)

analysis_data <- analysis_data |> 
  mutate(charge_descriptions = str_trim(charge_descriptions), 
         total_charges = ifelse(charge_descriptions == "NA" | charge_descriptions == "", 0,
                                lengths(str_split(charge_descriptions, "\\s*\\|\\s*"))))

head(analysis_data)

# Some data is messed up. Manually inputting
analysis_data <- analysis_data|>
  mutate(charge_descriptions = case_when(
    case_number == "062025CT163652A88840" ~ "M2",
    case_number == "062025CT165981A88810" ~ "M2 | 0",
    case_number == "062025CT166207A88830" ~ "M2",
    case_number == "062025CT167843A88810" ~ "M2 | M2 | 0 | 0",
    case_number == "062025CT167854A88810" ~ "M2 | M2 | 0 | 0",
    case_number == "062025CT171734A88830" ~ "M2 | 0",
    case_number == "062025CT178353A88830" ~ "M2",
    case_number == "062025CT179533A88840" ~ "M2 | 0 | 0",
    
    TRUE ~ charge_descriptions
  ))|>
  # recalculating
  mutate(total_charges = str_count(charge_descriptions, "\\|") + 1)
  
glimpse(analysis_data)

head(analysis_data)

analysis_data |> filter(total_charges == 1)
# 32/91*100 = 35%

analysis_data |> filter(total_charges > 1)

# 59 of 91 = 65%

analysis_data |> filter(total_charges > 2)
 # 33/91 *100 = 36%
analysis_data |> filter(total_charges > 3)
# 16/91*100 = 17.6%

# Is it used as a pretext to charge for more serious crimes? 

analysis_data|> 
  filter(str_detect(charge_descriptions, "F"))
