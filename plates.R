library(tidyverse)
library(janitor)

plates <- read_csv("20260109_liscense-plate.txt", 
                 locale = locale(encoding = "UTF-16"))

#-------Dictionary-----

# 1. Message/Transaction Type for Disposed Cases
# 2. Offender Based Transaction System Number
# 3. Court Docket Number
# 4. Uniform Case Number
# 5. Sequence Number
# 6. Date Record Updated
# 7. Court Designator
# 8. Name of Defendant/Juvenile at Arrest/Time Case Initiated
# 9. Name of Defendant/Juvenile at Final Disposition
# 10. Sex of Defendant/Juvenile
# 11. Race of Defendant/Juvenile
# 12. Birth Date of Defendant/Juvenile
# 13. Social Security Number
# 14. Local Arresting Agency’s Case Number/Arrest Number (Originating Case #)
# 15. State Identification Number
# 16. Federal Bureau of Investigation Number
# 17. Arresting Agency's ORI Number/Agency Responsible for Issuing Notice to Appear
# 18. Date of Initial Arrest, Notice to Appear/Summons Served
# 19. Date of Offense
# 20. Maximum Date of Offense
# 21. Clerk Clock-in Date
# 22. Date Capias/Warrant/

#--------Cleaning Data-----
plates <- clean_names(plates)

glimpse(plates)


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

# Do we have repeats? 

uniques <- plates|> 
  summarise(total_people = n_distinct(arrest_name))

print(uniques)

# Who repeats

top_people <- plates|>
  count(arrest_name, sort = TRUE)|>
  head(10)

print(top_people)

# How many per county?

county_counts <- plates|>
  group_by(county)|>
  tally(name = "case_count")|>
  arrange(desc(case_count))

print(county_counts)

91+57+24+19

191/342

91+57+24

172/342

(91+57)/342

# According to this, there's only 1 case in Miami???
# Switching to Broward

broward_plates <- plates |> 
  filter(county == "06")

glimpse(broward_plates)

# Writing file to use in scraper

brow_plates <- broward_plates |> select(court_docket_no)

write_csv(brow_plates, "broward_plates.csv")

west_plates <- plates |> filter(county == "50") |> select(court_docket_no)

write_csv(west_plates, "west-palm_recrods.csv")


leon_plates <- plates |> filter(county == "36") |> select(court_docket_no)

write_csv(leon_plates, "leon_records.csv")

vol_plates <- plates |> filter(county == "64") |> select(court_docket_no)

write_csv(vol_plates, "vol_records.csv")
