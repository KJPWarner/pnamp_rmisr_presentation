library(devtools)

#install rmisr
devtools::install_github("MattCallahan-NOAA/rmisr", force = TRUE)


#load packages, several aren't used in this workflow
p<- c("dplyr", "RCurl", "purrr", "tibble", "stringr", "DBI", "readr", "rvest", "RSQLite", 
      "tidyverse", "knitr", "kableExtra", "xtable", "gtable", "gt", "webshot2", "ggplot2",
      "RColorBrewer","httr", "jsonlite", "magrittr", "rmisr", "janitor", "keyring")

#pass list of packages in object p. Then we pass object p using lapply to load the previously required packages

lapply(p, require, character.only = TRUE)

# you'll need to obtain a API key from RMIS administrators to supply the token argument. This example uses the keyring package
# to maintain credential security
token <- key_get("rmis_api", "token")

# retrieve all location data from rmis
loc_all <- get_location(token = token)


######split location data into LUTS by location type. This could be a future application of a helper function

#release LUT
relt <- loc_all |> 
  filter(location_type == 4) |> 
  rename(release_location_name = name,
         release_location_description = description,
         release_location_code = location_code) |> 
  select(-c(id,
            format_version,
            record_code,
            submission_date,
            reporting_agency,
            location_type
  ))

##recovery LUT
rect <- loc_all |> 
  filter(location_type == 1) |> 
  rename(recovery_location_name = name,
         recovery_description = description,
         recovery_location_code = location_code) |> 
  select(-c(id,
            format_version,
            record_code,
            submission_date,
            reporting_agency,
            location_type
  ))

#hatchery LUT
hatt <- loc_all |> 
  filter(location_type == 3) |> 
  rename(hatchery_name = name,
         hatchery_description = description,
         hatchery_location_code = location_code) |> 
  select(-c(id,
            format_version,
            record_code,
            submission_date,
            reporting_agency,
            location_type
  ))

#stock LUT
stockt <- loc_all |> 
  filter(location_type == 5) |> 
  rename(stock_name = name,
         stock_location_code = location_code,
         stock_description = description) |>
  select(-c(id,
            format_version,
            record_code,
            submission_date,
            reporting_agency,
            location_type
  ))



########################## base r variant

# 1. Set up parameters- rmisr assumes some familiarity
# with labeling conventions within rmis.
# note: this may be a barrier to entry and require 
# frequent reference to the data specifications document:
#https://www.rmpc.org/wp-content/uploads/2023/11/Specification-V4.2-2023-12.pdf

#hatchery of interest location codes, found on the hatt data frame
hatchery <- c("3F42001  270017 H",
              "3F42001  270002 H")

years <- 2020:2023

species <- 1:3

#put previous params in single df
query_grid <- expand.grid(brood_year = years,
                          hatchery_location_code = hatchery,
                          species = species)


# 2. Initialize a releases list and progress bar
rel_list <- vector("list", nrow(query_grid))

pb <- txtProgressBar(min = 0, max = nrow(query_grid), style = 3)

# 3. The release loop
for (i in 1:nrow(query_grid)) {
  # Extract parameters for this iteration
  curr_year <- query_grid$brood_year[i]
  curr_hatch <- query_grid$hatchery_location_code[i]
  curr_spec <- query_grid$species[i]
  
  rel_list[[i]] <- tryCatch({
    get_release(token = token, brood_year = curr_year,
                species = curr_spec,
                hatchery_location_code = curr_hatch)
  }, error = function(e) {
    return(NULL) # NULLs are ignored later
  })
  
  setTxtProgressBar(pb, i) # Update progress bar
}
close(pb)

# 4. Bind all results into a single data frame, and use the distinct call to remove redundant rows
rel_df <- do.call(rbind, rel_list) |> 
  distinct()

#### 5. Set recovery parameters. 
#again, rmisr assumes some familiarity with the data spec- in this case we'll use the tag codes from the release query

tags <- rel_df$tag_code_or_release_id

#repeating process from before, this step is unneccessary but we'll leave it to allow for addition of more parameters into the grid.
# note- column names must match the headings from the recoveries table
query_grid <- expand.grid(tag_code = tags)

#initialize recovery list and the progress bar
rec_list <- vector("list", nrow(query_grid))
pb <- txtProgressBar(min = 0, max = nrow(query_grid), style = 3)

# 6. The recovery loop
for (i in 1:nrow(query_grid)) {
  # Extract parameters for this iteration
  curr_tag <- query_grid$tag_code[i]
  
  rec_list[[i]] <- tryCatch({
    get_recovery(token = token, tag_code = curr_tag)
  }, error = function(e) {
    return(NULL) # NULLs are ignored later
  })
  
  setTxtProgressBar(pb, i) # Update progress bar
}
close(pb)

rec_df <- do.call(rbind, rec_list) |> 
  distinct()

############cleanup joins to add release data to the recovery data, and trims to needed columns. There's a bit of qc here that also requires an
#understanding of rmis's data structure. See comments below.

rec1 <- rec_df |> 
  filter(tag_status == 1,#tag status 1 are only verified tag recoveries/reads. 
         #result will not include mismatched data such as a field call that doesn't match a tag code for species id
         fishery %in% c(50,
                        52,
                        54)) |> 
  distinct()

rel2 <- rel_df |> 
  left_join(stockt |> select(c(stock_location_code, stock_name)), by = "stock_location_code") |> 
  left_join(relt |> select(c(release_location_code, release_location_name, release_location_description)), by = "release_location_code") |> 
  left_join(hatt |> select(c(hatchery_location_code, hatchery_name)), by = "hatchery_location_code")

combined_df <- rec1 |>
  left_join(rect |> select(c(recovery_location_code, recovery_location_name)), by = "recovery_location_code") |> 
  left_join(rel2 |> select(c(run,
                             tag_code_or_release_id,
                             stock_name,
                             hatchery_name,
                             release_location_name)), by = c("tag_code" = "tag_code_or_release_id")) |>
  mutate(hatchery_name = case_when(is.na(hatchery_name) ~ stock_name, #this line substitutes hatchery-less releases with their associated stocks to avoid NAs
                                   TRUE ~ hatchery_name)) |> 
  drop_na(number_cwt_estimated) #remove NAs in the expanded cwt estimates in order to allow for summation

##annual sums by chosen criteria
rec_summary <- combined_df |>
  group_by(#hatchery_name,
    run_year,
    fishery,
    run,
    species,
    recovery_location_name) |> 
  summarise(total_estd_cwt_recovered = sum(number_cwt_estimated))

#example figure

ggplot(rec_summary, aes(x = (run_year), y = (total_estd_cwt_recovered), fill = fishery)) +
  geom_col() +
  scale_x_continuous(breaks = c(2021, 2022, 2023, 2024)) +
  labs(title = "Estimated recovery of Kalama River Hatchery CWTs in terminal areas (2021-2024)",
       x = "Run Year", y = "Total Estimated Coded Wire Recoveries")
