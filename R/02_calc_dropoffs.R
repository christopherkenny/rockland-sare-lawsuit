
# Calculate basic dropoff metrics for 2024 election data ----
elec_24_precs <- elec_24 |> 
  mutate(
    pre_24_dem_sum = pre_24_dem + pre_24_wor,
    pre_24_rep_sum = pre_24_rep + pre_24_con,
    ush_24_dem_sum = ush_24_dem, # working party candidate not Mondaire Jones
    ush_24_rep_sum = ush_24_rep + ush_24_con,
    uss_24_dem_sum = uss_24_dem + uss_24_wor,
    uss_24_rep_sum = uss_24_rep + uss_24_con
  ) |> 
  select(
    town, ed_num, ballot_total_24, ends_with('_sum')
  ) |> 
  mutate(
    dropoff_24_dem_uss = ((pre_24_dem_sum - uss_24_dem_sum) / pre_24_dem_sum) * 100,
    dropoff_24_rep_uss = ((pre_24_rep_sum - uss_24_rep_sum) / pre_24_rep_sum) * 100,
    dropoff_24_dem_ush = ((pre_24_dem_sum - ush_24_dem_sum) / pre_24_dem_sum) * 100,
    dropoff_24_rep_ush = ((pre_24_rep_sum - ush_24_rep_sum) / pre_24_rep_sum) * 100
  )

elec_24_sum <- elec_24_precs |> 
  as_tibble() |> 
  group_by(town) |>
  summarize(
    n = n(), 
    ballot_total_24 = sum(ballot_total_24, na.rm = TRUE),
    pre_24_dem_sum = sum(pre_24_dem_sum, na.rm = TRUE),
    pre_24_rep_sum = sum(pre_24_rep_sum, na.rm = TRUE),
    uss_24_dem_sum = sum(uss_24_dem_sum, na.rm = TRUE),
    uss_24_rep_sum = sum(uss_24_rep_sum, na.rm = TRUE),
    ush_24_dem_sum = sum(ush_24_dem_sum, na.rm = TRUE),
    ush_24_rep_sum = sum(ush_24_rep_sum, na.rm = TRUE),
    dropoff_24_dem_uss = ((pre_24_dem_sum - uss_24_dem_sum) / pre_24_dem_sum) * 100,
    dropoff_24_rep_uss = ((pre_24_rep_sum - uss_24_rep_sum) / pre_24_rep_sum) * 100,
    dropoff_24_dem_ush = ((pre_24_dem_sum - ush_24_dem_sum) / pre_24_dem_sum) * 100,
    dropoff_24_rep_ush = ((pre_24_rep_sum - ush_24_rep_sum) / pre_24_rep_sum) * 100
  )

# Calculate 2020 election dropoff metrics ----
# NO US Senate race in NY in 2020, so only US House as baseline
elec_20_precs <- elec_16 |> 
  mutate(
    pre_20_dem_sum = pre_20_dem + pre_20_wor,
    pre_20_rep_sum = pre_20_rep + pre_20_con,
    ush_20_dem_sum = ush_20_dem + ush_20_wor,
    ush_20_rep_sum = ush_20_rep # conservative candidate not Maureen McArdle Schulman
  ) |> 
  select(
    town, ed_num, ballot_total_20, ends_with('_sum')
  ) |> 
  mutate(
    dropoff_20_dem_ush = ((pre_20_dem_sum - ush_20_dem_sum) / pre_20_dem_sum) * 100,
    dropoff_20_rep_ush = ((pre_20_rep_sum - ush_20_rep_sum) / pre_20_rep_sum) * 100
  )

elec_20_sum <- elec_20_precs |> 
  as_tibble() |> 
  group_by(town) |>
  summarize(
    n = n(), 
    ballot_total_20 = sum(as.integer(ballot_total_20), na.rm = TRUE),
    pre_20_dem_sum = sum(pre_20_dem_sum, na.rm = TRUE),
    pre_20_rep_sum = sum(pre_20_rep_sum, na.rm = TRUE),
    ush_20_dem_sum = sum(ush_20_dem_sum, na.rm = TRUE),
    ush_20_rep_sum = sum(ush_20_rep_sum, na.rm = TRUE),
    dropoff_20_dem_ush = ((pre_20_dem_sum - ush_20_dem_sum) / pre_20_dem_sum) * 100,
    dropoff_20_rep_ush = ((pre_20_rep_sum - ush_20_rep_sum) / pre_20_rep_sum) * 100
  )

# Calculate 2016 election dropoff metrics ----
# House was uncontested (just Nita Lowey)
elec_16_precs <- elec_16 |> 
  mutate(
    pre_16_dem_sum = pre_16_dem + pre_16_wor + pre_16_wep,
    pre_16_rep_sum = pre_16_rep + pre_16_con,
    uss_16_dem_sum = uss_16_dem + uss_16_wor + uss_16_wep + uss_16_ind,
    uss_16_rep_sum = uss_16_rep + uss_16_con + uss_16_ref
  ) |> 
  select(
    town, ed_num, ballot_total_16, ends_with('_sum')
  ) |> 
  mutate(
    dropoff_16_dem_uss = ((pre_16_dem_sum - uss_16_dem_sum) / pre_16_dem_sum) * 100,
    dropoff_16_rep_uss = ((pre_16_rep_sum - uss_16_rep_sum) / pre_16_rep_sum) * 100
  )

elec_16_sum <- elec_16_precs |>
  as_tibble() |> 
  group_by(town) |>
  summarize(
    n = n(), 
    ballot_total_16 = sum(as.integer(ballot_total_16), na.rm = TRUE),
    pre_16_dem_sum = sum(pre_16_dem_sum, na.rm = TRUE),
    pre_16_rep_sum = sum(pre_16_rep_sum, na.rm = TRUE),
    uss_16_dem_sum = sum(uss_16_dem_sum, na.rm = TRUE),
    uss_16_rep_sum = sum(uss_16_rep_sum, na.rm = TRUE),
    dropoff_16_dem_uss = ((pre_16_dem_sum - uss_16_dem_sum) / pre_16_dem_sum) * 100,
    dropoff_16_rep_uss = ((pre_16_rep_sum - uss_16_rep_sum) / pre_16_rep_sum) * 100
  )
