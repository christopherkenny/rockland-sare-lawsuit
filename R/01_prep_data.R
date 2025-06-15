
if (!file_exists(here('data/rockland_2016_2020.geojson'))) {
  # 2016 and 2020 use same shapes ----
  shp_16 <- st_read(here('data-raw/gis/Cty_ED_Jan_15_2016.shp')) |> 
    janitor::clean_names() |> 
    mutate(precinct = ed_key)
  
  ## elections 2016
  pre_16 <- read_rockland(here('data-raw/elections/6GNYROCK_PRESIDENT.xlsx')) |> 
    janitor::clean_names() |> 
    rename_with(.fn = \(x) str_replace(x, 'presidential_electors_for_president_and_vice_president_', 'pre_16_')) |> 
    rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('pre_')) |> 
    rename(precinct = district, ballot_total_16 = ballots_cast_total_ballots_cast) |>  
    filter(precinct != 'COUNTY TOTALS') |> 
    mutate(across(starts_with('pre_'), \(x) replace_na(as.integer(x), 0L))) |> 
    select(precinct, ballot_total_16, starts_with('pre')) |> 
    mutate(
      precinct = paste0(
        str_sub(precinct, 1, 1), 
        str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
      )
    )
  ush_16 <- read_rockland(here('data-raw/elections/6GNYROCK_REP_CONGRESS.xlsx')) |> 
    janitor::clean_names() |> 
    rename_with(.fn = \(x) str_replace(x, 'representative_in_congress_', 'ush_16_')) |> 
    rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('ush_')) |> 
    rename(precinct = district) |>  
    filter(precinct != 'COUNTY TOTALS') |>  
    mutate(across(starts_with('ush_'), \(x) replace_na(as.integer(x), 0L))) |> 
    select(precinct, starts_with('ush')) |> 
    mutate(
      precinct = paste0(
        str_sub(precinct, 1, 1), 
        str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
      )
    )
  uss_16 <- read_rockland(here('data-raw/elections/6GNYROCK_US_SENATOR.xlsx')) |> 
    janitor::clean_names() |> 
    rename_with(.fn = \(x) str_replace(x, 'united_states_senator_', 'uss_16_')) |> 
    rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('uss_')) |> 
    rename(precinct = district) |>  
    filter(precinct != 'COUNTY TOTALS') |>  
    mutate(across(starts_with('uss_'), \(x) replace_na(as.integer(x), 0L))) |> 
    select(precinct, starts_with('uss')) |> 
    mutate(
      precinct = paste0(
        str_sub(precinct, 1, 1), 
        str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
      )
    )
  
  
  ## elections 2020
  pre_20 <- read_rockland(here('data-raw/elections/0GNYROCK_PRESIDENT.xlsx')) |> 
    janitor::clean_names() |> 
    rename_with(.fn = \(x) str_replace(x, 'presidential_electors_for_president_and_vice_president_', 'pre_20_')) |> 
    rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('pre_')) |> 
    rename(precinct = district, ballot_total_20 = ballots_cast_total_ballots_cast) |>  
    filter(precinct != 'COUNTY TOTALS') |> 
    mutate(across(starts_with('pre_'), \(x) replace_na(as.integer(x), 0L))) |> 
    select(precinct, ballot_total_20, starts_with('pre')) |> 
    mutate(
      precinct = paste0(
        str_sub(precinct, 1, 1), 
        str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
      )
    )
  ush_20 <- read_rockland(here('data-raw/elections/0GNYROCK_REP_IN_CONGRESS.xlsx')) |> 
    janitor::clean_names() |> 
    rename_with(.fn = \(x) str_replace(x, 'representative_in_congress_', 'ush_20_')) |> 
    rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('ush_')) |> 
    rename(precinct = district) |>  
    filter(precinct != 'COUNTY TOTALS') |>  
    mutate(across(starts_with('ush_'), \(x) replace_na(as.integer(x), 0L))) |> 
    select(precinct, starts_with('ush')) |> 
    mutate(
      precinct = paste0(
        str_sub(precinct, 1, 1), 
        str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
      )
    )
  
  elec_16 <- list(shp_16, pre_16, ush_16, pre_20, ush_20) |> 
    reduce(left_join, by = c('precinct'))
  
  # 2024 uses new precinct shapes ----
  shp_24 <- st_read(here('data-raw/gis/Cty_ED_Jan_1_2024.shp')) |> 
    janitor::clean_names()
  
  ## elections 2024
  all_24 <- read_rockland(here('data-raw/elections/GE 2024_EXPORT_ALL.CSV')) |> 
    janitor::clean_names() |> 
    filter(precinct_name != 'COUNTY TOTALS')
  pre_24 <- all_24 |> 
    select(
      precinct_name, ballot_total_24 = ballots_cast_total_ballots_cast, starts_with('electors_for_pre')
    ) |> 
    rename_with(.fn = \(x) str_replace(x, 'electors_for_president_and_vice_president_', 'pre_24_')) |> 
    rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('pre_')) |> 
    mutate(
      ballot_total_24 = replace_na(as.integer(ballot_total_24), 0L),
      ed_key = paste0(
        str_sub(precinct_name, 1, 1), 
        str_pad(str_extract(precinct_name, '\\d+'), side = 'left', pad = '0', width = 2)
      ),
      .after = precinct_name
    ) |> 
    mutate(across(starts_with('pre_'), \(x) replace_na(as.integer(x), 0L)))
  
  ush_24 <- all_24 |> 
    select(
      precinct_name, starts_with('representative_in_congress_17th')
    ) |> 
    rename_with(.fn = \(x) str_replace(x, 'representative_in_congress_17th_district_17th_congressional_district_', 'ush_24_')) |> 
    rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('ush_')) |> 
    mutate(
      ed_key = paste0(
        str_sub(precinct_name, 1, 1),
        str_pad(str_extract(precinct_name, '\\d+'), side = 'left', pad = '0', width = 2)
      ),
      .after = precinct_name
    ) |> 
    select(-precinct_name) |> 
    mutate(across(starts_with('ush_'), \(x) replace_na(as.integer(x), 0L)))
  
  uss_24 <- all_24 |> 
    select(
      precinct_name, starts_with('united_states_senator_')
    ) |> 
    rename_with(.fn = \(x) str_replace(x, 'united_states_senator_', 'uss_24_')) |>
    rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('uss_')) |>
    mutate(
      ed_key = paste0(
        str_sub(precinct_name, 1, 1),
        str_pad(str_extract(precinct_name, '\\d+'), side = 'left', pad = '0', width = 2)
      ),
      .after = precinct_name
    ) |> 
    select(-precinct_name) |> 
    mutate(across(starts_with('uss_'), \(x) replace_na(as.integer(x), 0L)))
    
  
  elec_24 <- list(shp_24, pre_24, ush_24, uss_24) |>
    reduce(left_join, by = c('ed_key')) |> 
    select(ed_key, precinct_name, town, ed_num, ballot_total_24, starts_with(c('pre_', 'ush_', 'uss_')))
  
  # save data ----
  st_write(elec_16, here('data/rockland_2016_2020.geojson'))
  st_write(elec_24, here('data/rockland_2024.geojson'))
} else {
  elec_16 <- st_read(here('data/rockland_2016_2020.geojson'))
  elec_24 <- st_read(here('data/rockland_2024.geojson'))
}
