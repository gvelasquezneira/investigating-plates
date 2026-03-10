library(tidyverse)
library(lubridate)
library(janitor)

raw_data <- read_csv("broward_full_records.csv") |> clean_names()

cleaned_vitals <- raw_data |>
  filter(party_type == "Defendant") |>
  mutate(dob = mdy(dob))

cleaned_long <- cleaned_vitals |>
  separate_rows(charge_descriptions, charge_degrees, sep = " \\| ") |>
  mutate(
    raw_charge_text = str_remove_all(charge_degrees, "^\\(|\\)$")
  )

final_clean_data <- cleaned_long |>
  mutate(
    degree_clean = str_extract(charge_descriptions, "F\\d|M\\d|NF"),
    charge_name = str_extract(raw_charge_text, "^.*?(?=Date Filed:|$)") |> str_trim(),
    date_filed = str_extract(raw_charge_text, "(?<=Date Filed: )\\d{2}/\\d{2}/\\d{4}") |> mdy(),
    
    statute = str_extract(raw_charge_text, "(?<=Current Statute: ).*?(?= Filing Type:|$)") |> str_trim(),
    
    agency = str_extract(raw_charge_text, "(?<=Filing Agency: ).*?(?= Original Statute:|$)") |> str_trim()
  ) |>
  select(
    case_number, full_name, dob, race, gender, 
    charge_name, degree_clean, statute, date_filed, agency, 
    all_dispositions, address
  )


write_csv(final_clean_data, "broward_cleaned_v2.csv")
