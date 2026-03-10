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


pop <- read_csv("DECENNIALPL2020.P1-Data.csv")
glimpse(pop)
fips <- read_delim("Records/st12_fl_cou2020.txt", delim = "|")

glimpse(fips)

fips <- fips |> rename(county = COUNTYNAME)

?str_remove

fips <- fips |> mutate(county = str_remove(county, " County"))

glimpse(fips)

pop <- pop |> rename(county_name = County)

pop <- pop |> mutate(county_name = str_remove(county_name, " County, Florida"))

pop <- pop |> mutate(county_name = str_trim(county_name))

glimpse(pop)

names <- read_csv("county_names.csv")

glimpse(names)

names <- names |> rename(county = code) 

names <- names |> select(county, county_name)

glimpse(names)

glimpse(county_counts)

?inner_join

county_counts <- county_counts |> inner_join(names, by = "county")

glimpse(county_counts)

county_counts <- county_counts |>drop_na(county)
glimpse(county_counts)


county_counts <- county_counts |> mutate(county_name = str_trim(county_name))
glimpse(county_counts)

                                 
?inner_join
county_counts <- county_counts |> full_join(pop, by = "county_name")

glimpse(county_counts)

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

#write_csv(brow_plates, "Records/broward_plates.csv")

west_plates <- plates |> filter(county == "50") |> select(court_docket_no)

# write_csv(west_plates, "Records/west-palm_records.csv")


leon_plates <- plates |> filter(county == "36") |> select(court_docket_no)

# Weird Clerk

# write_csv(leon_plates, "Records/leon_records.csv")

vol_plates <- plates |> filter(county == "64") |> select(court_docket_no)

# write_csv(vol_plates, "Records/vol_records.csv")


# Weird Clerk

lee_plates <- plates |> filter(county == "36") |> select(court_docket_no)
# 
# write_csv(lee_plates, "Records/lee_records.csv")

#-----11. Lee Plates------



lee_plates <- plates |> filter(county == "36") |> select(court_docket_no)
glimpse(lee_plates)
view(lee_plates)
#362025CF017867000ACH
lee_plates <-lee_plates |> mutate(court_docket_no = str_replace(
  court_docket_no,
  pattern = ".{2}(\\d{4})([A-Z]{2})(\\d{6})([A-Z]{2})([A-Z]{2}).*", replacement = "\\2\\3\\5"))


#-----9. Vol Plates

vol_plates <- plates |> filter(county == "64") |> select(court_docket_no)
glimpse(vol_plates)
view(vol_plates)

vol_plates <- vol_plates |> 
  mutate(court_docket_no = str_replace(
    court_docket_no,
    pattern = "^64(\\d{4})([A-Z]{2})(\\d{6})[A-Z]{4}([A-Z]{2})$",
    replacement = "\\1 \\3 \\2\\4"
  ))
view(vol_plates)

#-----10. Manatee Plates-----

mantatee_plates <- plates |> filter(county == "41") |> select(court_docket_no)

view(mantatee_plates)
mantatee_plates <- mantatee_plates |> mutate(court_docket_no = str_replace(
  court_docket_no,
  pattern = ".{2}(\\d{4})([A-Z]{2})(\\d{6})([A-Z]{2})([A-Z]{2}).*", replacement = "\\1\\2\\3\\5"))

view(mantatee_plates)

write_csv(manatee_plates, "Records/marion_records.csv")

# Weird Clerk

#-----12. Brevard Plates-----

brev_plates <- plates |> filter(county == "05") |> select(court_docket_no)

view(brev_plates)
brev_plates <- brev_plates |> mutate(court_docket_no = str_replace(
  court_docket_no,
  pattern = ".{2}(\\d{4})([A-Z]{2})(\\d{6})([A-Z]{2})([A-Z]{2}).*", replacement = "\\1\\2\\3\\5"))

view(brev_plates)

#----13. Nassau Clerk of Court-----

nassau_plates <- plates |> filter(county == "45") |> select(court_docket_no)
nassau_plates <- nassau_plates |> mutate(court_docket_no = str_replace(
  court_docket_no,
  pattern = "^(\\d{2})(\\d{4})[A-Z]{2}(\\d{6})[A-Z]+$", 
  replacement = "\\2\\1\\3"))
# 202645001733
# 452025CT001143CTAXYX

#write_csv(nassau_plates, "Records/nassau_records.csv")
#-----14. Pinellas County -----

pinnellas_plates <- plates |> filter(county == "52") |> select(court_docket_no)

pinnellas_plates <- pinnellas_plates %>%
  mutate(court_docket_no = str_replace(
    court_docket_no,
    # Pattern breakdown:
    # 1. ^52           -> Matches the county code
    # 2. 20(\\d{2})    -> Group 1: Matches '20' and captures the last 2 year digits (25)
    # 3. ([A-Z]{2})    -> Group 2: Captures the Court Type (MM)
    # 4. 0(\\d{5})     -> Group 3: Skips a leading zero and captures the next 5 digits (14619)
    # 5. \\d{3}[A-Z]+$ -> Consumes the trailing zeros (000) and suffix (APC)
    pattern = "^5220(\\d{2})([A-Z]{2})0(\\d{5})\\d{3}[A-Z]+$",
    
    # Replacement: YY-Sequence-Type
    replacement = "\\1-\\3-\\2"
  ))

glimpse(pinnellas_plates)
view(pinnellas_plates)


#write_csv(pinnellas_plates, "Records/pinnellas_records.csv")

brevard_plates <- plates |> filter(county == "05") |> select(court_docket_no)

write_csv(brevard_plates, "Records/brevard_records.csv")

# ----15. Martin County ----------
martin_plates <- plates |> filter(county == "43") |> select(court_docket_no)
# 432025CT003813CTAXMX
martin_plates <- martin_plates |> 
  mutate(court_docket_no = str_replace(
    court_docket_no,
    pattern = "^43(\\d{4})([A-Z]{2})(\\d{6})[A-Z]{4}([A-Z]{2})$",
    replacement = "\\1 \\3 \\2"
  ))
view(martin_plates)

# write_csv(martin_plates, "Records/miami_records.csv")

sara_plates <- plates |> filter(county == "58") |> select(court_docket_no)
glimpse(sara_plates)

sara_plates <- sara_plates |> 
  mutate(
    # 1. Clean up hidden spaces first
    court_docket_no = str_trim(court_docket_no),
    
    # 2. Apply the specific regex
    court_docket_no = str_replace(
      court_docket_no, 
      pattern = ".{2}(\\d{4})([A-Z]{2})(\\d{6}).*([A-Z]{2})$", 
      replacement = "\\1 \\2 \\3 \\4"
    )
  )

#write_csv(sara_plates, "Records/sara_records.csv")

# -------16. Seminole County -------

sem_plates <- plates |> filter(county == "59") |> select(court_docket_no)
view(sem_plates)

glimpse(sem_plates)

sem_plates <- sem_plates |> mutate(court_docket_no = str_replace(court_docket_no, pattern = ".{2}(\\d{4})([A-Z]{2})(\\d{6}).*", replacement = "\\1\\2\\3"))

# -------17. Lake County -------

lake_plates <- plates |> filter(county == "35") |> select(court_docket_no)
glimpse(lake_plates)

view(lake_plates)


# -------18. Escambia County -------

esc_plates <- plates |> filter(county == "17") |> select(court_docket_no)
glimpse(esc_plates)

# 172025CF004519XXXAXA

esc_plates <- esc_plates |> 
  mutate(court_docket_no = str_replace(
    court_docket_no,
    pattern = "^17(\\d{4})([A-Z]{2})(\\d{6})[A-Z]{4}([A-Z]{2})$",
    replacement = "\\1 \\2 \\3"
  ))




# Weird Clerk

# I'm tired of going back and forthc

county_counts <- county_counts |> 
  mutate(Total_Population = case_when(
    county == "13" ~ 2701767,
    TRUE ~ as.numeric(Total_Population)
  ))

#-----19. Miami County -----
miami_plates <- plates |> filter(county == "13") |> select(court_docket_no)
glimpse( miami_plates)

#-----20. Pasco County -----
pasco_plates <- plates |> filter(county == "51") |> select(court_docket_no)
glimpse(pasco_plates)
head(pasco_plates)
view(pasco_plates)

#-----21. Levy County -----
levy_plates <- plates |> filter(county == "38") |> select(court_docket_no)
glimpse(levy_plates)
head(levy_plates)
view(levy_plates)

#-----22. Charlotte County -----
char_plates <- plates |> filter(county == "08") |> select(court_docket_no)
glimpse(char_plates)
head(char_plates)
view(char_plates)

#-----23. Glades County -----
glades_plates <- plates |> filter(county == "22") |> select(court_docket_no)
glimpse(glades_plates)
head(glades_plates)
view(glades_plates)

#-----24. Hernando County -----
hern_plates <- plates |> filter(county == "27") |> select(court_docket_no)
glimpse(hern_plates)
head(hern_plates)
view(hern_plates)

#-----25. Collier County -----
collier_plates <- plates |> filter(county == "11") |> select(court_docket_no)
glimpse(collier_plates)
head(collier_plates)
view(collier_plates)

#-----26. DeSoto County -----
desoto_plates <- plates |> filter(county == "14") |> select(court_docket_no)
glimpse(desoto_plates)
head(desoto_plates)
view(desoto_plates)

#-----27. Hamilton County -----
hamilton_plates <- plates |> filter(county == "24") |> select(court_docket_no)
glimpse(hamilton_plates)
head(hamilton_plates)
view(hamilton_plates)

#-----28. Leon County -----
leon_plates <- plates |> filter(county == "37") |> select(court_docket_no)
glimpse(leon_plates)
head(leon_plates)
view(leon_plates)

#372025CT001694A000MX
#2000 TR 000001

le_plates <- leon_plates |> 
  mutate(court_docket_no = str_replace(
    court_docket_no,
    pattern = "^37(\\d{4})([A-Z]{2})(\\d{6})[A-Z]{1}(\\d{3})([A-Z]{2})$",
    replacement = "\\1 \\2 \\3"
  ))

#-----29. Marion County -----
marion_plates <- plates |> filter(county == "42") |> select(court_docket_no)
glimpse(marion_plates)
head(marion_plates)
view(marion_plates)

#-----30. Okee County -----
oke_plates <- plates |> filter(county == "47") |> select(court_docket_no)
glimpse(oke_plates)
head(oke_plates)
view(oke_plates)

#-----31. St. Lucie County -----
lucie_plates <- plates |> filter(county == "56") |> select(court_docket_no)
glimpse(lucie_plates)
head(lucie_plates)
view(lucie_plates)

lucie_plates <- lucie_plates |> 
  mutate(court_docket_no = str_replace(
    court_docket_no,
    pattern = "^56(\\d{4})([A-Z]{2})(\\d{6})[A-Z]{6}$",
    replacement = "\\1 \\2 \\3"
  ))

view(lucie_plates)

#-----32. Sumter County -----
sumter_plates <- plates |> filter(county == "60") |> select(court_docket_no)
glimpse(sumter_plates)
head(sumter_plates)
view(sumter_plates)

#-----33. Bay County -----
bay_plates <- plates |> filter(county == "03") |> select(court_docket_no)
glimpse(bay_plates)
head(bay_plates)
view(bay_plates)

#-----34. Col County -----
col_plates <- plates |> filter(county == "12") |> select(court_docket_no)
glimpse(col_plates)
head(col_plates)
view(col_plates)


#-----35. Miami  County -----
Miami_plates <- plates |> filter(county == "13") |> select(court_docket_no)
glimpse(Miami_plates)
head(Miami_plates)
view(Miami_plates)

#-----36. Hills  County -----
Hills_plates <- plates |> filter(county == "29") |> select(court_docket_no)
glimpse(Hills_plates)
head(Hills_plates)
view(Hills_plates)

#-----37. Holmes  County -----
Holmes_plates <- plates |> filter(county == "30") |> select(court_docket_no)
glimpse(Holmes_plates)
head(Holmes_plates)
view(Holmes_plates)

#-----38. Monroe  County -----
Monroe_plates <- plates |> filter(county == "44") |> select(court_docket_no)
glimpse(Monroe_plates)
head(Monroe_plates)
view(Monroe_plates)

#-----39. Putname  County -----
Putnam_plates <- plates |> filter(county == "54") |> select(court_docket_no)
glimpse(Putnam_plates)
head(Putnam_plates)
view(Putnam_plates)

#-----40. Suwanee  County -----
Suwanee_plates <- plates |> filter(county == "61") |> select(court_docket_no)
glimpse(Suwanee_plates)
head(Suwanee_plates)
view(Suwanee_plates)


data <- read_csv("data/320.061-Data Project.csv")

no_data <- data |> filter(full_name == "NO CASE FOUND") |> select(case_number)
osce <- plates |> filter(county == "49") |>  select(court_docket_no)

#---County Counts------
county_counts <- county_counts |> 
  mutate(status = case_when(
    county == "06" ~ "Done",
    county == "50" ~ "Done",
    county == "36" ~ "Done",
    county == "05" ~ "Done",
    county == "59" ~ "Done",
    county == "64" ~ "Done",
    county == "35" ~ "Done",
    county == "41" ~ "Done",
    county == "17" ~ "Done",
    county == "43" ~ "Done",
    county == "58" ~ "Done",
    county == "08" ~ "Done",
    county == "22" ~ "Done",
    county == "27" ~ "Done",
    county == "11" ~ "Done",
    county == "45" ~ "Done",
    county == "52" ~ "Failed",
    county == "51" ~ "Done",
    county == "38" ~ "Done",
    county == "14" ~ "Done",
    county == "24" ~ "Done",
    county == "37" ~ "Done",
    county == "42" ~ "Done",
    county == "47" ~ "Done",
    county == "56" ~ "Done",
    county == "60" ~ "Done",
    county == "03" ~ "Done",
    county == "12" ~ "Done",
    county == "13" ~ "Done",
    county == "29" ~ "Done",
    county == "54" ~ "Done",
    TRUE ~ "Not Completed" 
  ))

done <- county_counts |> 
  filter(status == "Done") |> 
  summarize(total_cases = sum(case_count, na.rm = TRUE))

remainder <- county_counts |> 
  summarize(total_cases = sum(case_count, na.rm = TRUE))

(done/remainder)*100

write_csv(county_counts, "county_codes.csv")





