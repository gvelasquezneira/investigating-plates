library(tidyverse)
library(janitor)
library(lubridate)

#----Importing Data---------

data <- read_csv("data/320.061-Data Project.csv")

glimpse(data)

data <- clean_names(data)

glimpse(data)



# ----------Are the stops pretextual?------

data <- data |> 
  mutate(race_category = case_when(
    toupper(race) %in% c("HISPANIC", "H") ~ "Hispanics",
    toupper(race) %in% c("BLACK", "B") ~ "Blacks",
    toupper(race) %in% c("WHITE", "W") ~ "Whites",
    toupper(race) %in% c("ASIAN", "A") ~ "Asian",
    toupper(race) %in% c("INDIGENOUS", "I") ~ "Indigenous",
    toupper(race) %in% c("OTHER", "O") ~ "Mixed",
    TRUE ~ "OTHER" # These are my NA values. It currently adds up to about 24, which is correct.
  ))

data <- data |> 
  mutate(gender = case_when(
    toupper(gender) %in% c("MALE", "M") ~ "Male",
    toupper(gender) %in% c("FEMALE", "F") ~ "Female",
    TRUE ~ "OTHER" 
  ))

# Race_ana seems to be about right on pop targets just from eyeballing

race_ana <- data |> group_by(race_category) |> summarise(count = n()) |> 
  mutate(percentage = round((count / sum(count)) * 100, 2)) |> 
  arrange(desc(percentage))

# gender is disproportionate on who gets tickets. Women tend to not be ticketed. Men do.
# not a crazy find
gender_ana <-data |> group_by(gender) |> summarise(count = n()) |> 
  mutate(percentage = round((count / sum(count)) * 100, 2)) |> 
  arrange(desc(percentage))

# ------Checking ages------
data <- data |>
  mutate(
    dob_date = mdy(dob),
    age = 2025 - year(dob_date),
    age_group = cut(age, 
                    breaks = c(0, 21, 30, 40, 50, 100), 
                    labels = c("Under 21", "21-30", "31-40", "41-50", "Over 50"))
  )

# age_summary <- data |>
#   group_by(age_group) |>
#   summarise(
#     Avg_Charges = mean(num_charges, na.rm = TRUE),
#     Total_Cases = n()
#   )
# 
# view(age_summary)

# A lot of cases failed to parse here. it's prelim. but younger drivers are getting charges at higher rates.

# -------Tacked on?------

analyze_charges <- function(degree_str) {
  if (is.na(degree_str) || degree_str == "") return(c(0, 0, 0))
  
  # parse by pipe
  charges <- str_split(degree_str, "\\|")[[1]]
  charges <- trimws(charges)
  
  total <- length(charges)
  felonies <- sum(str_detect(toupper(charges), "F"))
  infractions <- sum(str_detect(charges, "0"))
  
  return(c(total, felonies, infractions))
}

charge_counts <- t(sapply(data$degree_clean, analyze_charges))
data$total_charges <- charge_counts[, 1]
data$felony_charges <- charge_counts[, 2]
data$infraction_charges <- charge_counts[, 3]


summary_stats <- data |>
  group_by(race_category) |>
  summarise(
    Avg_Total_Charges = median(total_charges, na.rm = TRUE),
    Avg_Felonies = median(felony_charges, na.rm = TRUE),
    Avg_Infractions = median(infraction_charges, na.rm = TRUE),
    Sample_Size = n()
  ) |>
  arrange(desc(Avg_Total_Charges))
# 
# view(summary_stats)

# checking mean
summary_stats <- data |>
  group_by(race_category) |>
  summarise(
    Avg_Total_Charges = mean(total_charges, na.rm = TRUE),
    Avg_Felonies = mean(felony_charges, na.rm = TRUE),
    Avg_Infractions = mean(infraction_charges, na.rm = TRUE),
    Sample_Size = n()
  ) |>
  arrange(desc(Avg_Total_Charges))

# view(summary_stats)

# Let's pivot. How many times out of the whole did people get things tacked on? 

data <- data |>
  rowwise() |>
  mutate(num_charges = if_else(
    is.na(degree_clean) | degree_clean == "" | degree_clean == "0", 
    0, 
    length(str_split(degree_clean, "\\|")[[1]])
  )) |>
  ungroup()

total_cases <- nrow(data)
tacked_on_cases <- sum(data$num_charges > 1)

percentage <- round((tacked_on_cases / total_cases) * 100, 2)

print(percentage)
# 31.4% of cases had additional things tacked on after the initial stop.

#How many might have started with a speeding charge?

data_analysis <- data |>
  rowwise() |>
  mutate(
    num_charges = if_else(
      is.na(degree_clean) | degree_clean == "" | degree_clean == "0", 
      0, 
      length(str_split(degree_clean, "\\|")[[1]])
    ),
    all_charges_text = paste(c_across(starts_with("charge_")), collapse = " "),
    has_speeding = str_detect(toupper(all_charges_text), "SPEED|MPH|SPD")
  ) |>
  ungroup()

speeding_extra_cases <- data_analysis |>
  filter(has_speeding == TRUE)

total_with_extra = sum(data_analysis$num_charges > 1)
count_both = nrow(speeding_extra_cases)

glimpse(count_both)

# view(speeding_extra_cases)

glimpse(speeding_extra_cases)

#Speeding then tag
#362025CF017867000ACH Javier Alvarez Alvarez DUI & CRASHED 
#062025CT164576A88840, 062025CT178576A88840

ex_speeding <- data_analysis |>
  filter(
    has_speeding == FALSE | 
      trimws(case_number) %in% c(
        "2025 116783 CTDB", 
        "412025CT004986CTAXMA", 
        "522025CF010001000APC"))


ex_speeding <- ex_speeding |> 
  mutate(county = substr(trimws(case_number), start=1, stop=2)) 

ex_speeding <- full_join(ex_speeding, county_counts, by = "county")

ex_speeding <- ex_speeding |> select(case_number, county_name, full_name,degree_clean, num_charges, all_charges_text, all_dispositions, dob, race, gender, agency)



total_with_extra = sum(ex_speeding$num_charges > 1)
total = sum(data_analysis$num_charges >= 0)

glimpse(total)

total_with_extra/total*100
#When removing the cases that begun with speeding, 28% of all driver's were hit with extra charges. 

#The following werwe stopped for plate then hit with speeding
#2025 116783 CTDB, 2025 116783 CTDB, #412025CT004986CTAXMA

#Not hit with speeding
#522025CF010001000APC


count_charges <- function(degree_str) {
  if (is.na(degree_str) || degree_str == "" || degree_str == "0") {
    return(0)
  }
  parts <- str_split(degree_str, "\\|")[[1]]
  return(length(trimws(parts)))
}

top_cases <- data_analysis |>
  rowwise() |>
  mutate(num_charges = count_charges(degree_clean)) |>
  ungroup() |>
  filter(num_charges >=2) |>
  arrange(desc(num_charges))

top_cases <- top_cases |>
  filter(
    has_speeding == FALSE | 
      trimws(case_number) %in% c(
        "2025 116783 CTDB", 
        "412025CT004986CTAXMA", 
        "522025CF010001000APC"))


request_list <- top_cases |>
  select(case_number, full_name, dob, race_category,agency, num_charges, degree_clean, charge_1, charge_2, charge_3, charge_4, charge_5, charge_6, plea)

view(request_list)

#This gave me an idea. Is there a county that tacked on cases more than other in proportion to the amount of cases it had in the dataset?

#---SKIP: Recovering County Counts from a diff script----
plates <- read_csv("20260109_liscense-plate.txt", 
                   locale = locale(encoding = "UTF-16"))

plates <- clean_names(plates)

plates <- plates |> select(court_docket_no, 
                           sequence_no,
                           no_of_counts, 
                           court_designator, 
                           county, 
                           arrest_name, 
                           final_name, 
                           sex, 
                           race, 
                           init_arrest_dt, 
                           offense_dt, 
                           in_charge_status, 
                           charge_level, 
                           charge_degree, 
                           in_flst_chap, 
                           in_flst_sect) 

county_counts <- plates|>
  group_by(county)|>
  tally(name = "case_count")|>
  arrange(desc(case_count))


pop <- read_csv("DECENNIALPL2020.P1-Data.csv")
pop <- pop |> rename(county_name = County)
pop <- pop |> mutate(county_name = str_remove(county_name, " County, Florida"))
pop <- pop |> mutate(county_name = str_trim(county_name))
names <- read_csv("county_names.csv")
names <- names |> rename(county = code) 
names <- names |> select(county, county_name)
county_counts <- county_counts |> inner_join(names, by = "county")
county_counts <- county_counts |>drop_na(county)
county_counts <- county_counts |> mutate(county_name = str_trim(county_name))
county_counts <- county_counts |> full_join(pop, by = "county_name")

glimpse(county_counts)

glimpse(data_analysis)

# Joining county and data_analysis

data_analysis <- data_analysis |> 
  mutate(county = substr(trimws(case_number), start=1, stop=2)) 

test <- inner_join(data_analysis, county_counts, by = "county") |> 
  filter(!is.na(case_number))

count_charges_fixed <- function(degree, ...) {
  # Step A: If degree_clean exists, count the parts separated by "|"
  if (!is.na(degree) && degree != "" && tolower(degree) != "nan") {
    parts <- unlist(str_split(degree, "\\|"))
    return(length(trimws(parts[parts != ""])))
  }
  
  # Step B: If degree_clean is missing, count the charge columns (1-8)
  charge_cols <- unlist(list(...))
  valid_charges <- charge_cols[!is.na(charge_cols) & 
                                 charge_cols != "" & 
                                 tolower(charge_cols) != "nan"]
  
  if (length(valid_charges) == 0) return(0)
  
  # Split by "|" and count unique segments (handles Palm Beach format)
  all_parts <- unlist(str_split(valid_charges, "\\|"))
  clean_parts <- trimws(all_parts)
  return(length(unique(clean_parts[clean_parts != ""])))
}

test <- test |> 
  filter(has_speeding == FALSE | 
                    trimws(case_number) %in% c(
                      "2025 116783 CTDB", 
                      "412025CT004986CTAXMA", 
                      "522025CF010001000APC"))


test <- test|>
  rowwise()|>
  mutate(total_charges = count_charges_fixed(
    degree_clean, 
    charge_1, charge_2, charge_3, charge_4, 
    charge_5, charge_6, charge_7, charge_8
  ))|>
  ungroup()

county_comparison <- test|>
  group_by(county_name)|>
  summarise(
    Total_Stops = n(),
    Avg_Charges = round(mean(total_charges, na.rm = TRUE), 2),
    Cases_With_Extra = sum(total_charges > 1, na.rm = TRUE),
    Tacked_On_Percentage = round((Cases_With_Extra / Total_Stops) * 100, 2)
  )|>
  # Filter for counties with at least 5 stops to ensure statistical relevance
  filter(Total_Stops >= 5) |> 
  arrange(desc(Tacked_On_Percentage))

# Review the results
print(county_comparison)




