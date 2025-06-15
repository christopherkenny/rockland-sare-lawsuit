
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

# Using Senate as baseline for dropoff ----
elec_24_precs |> 
  mutate(ed_num = as.integer(ed_num)) |>
  as_tibble() |> 
  filter(town != 'Ramapo') |> 
  pivot_longer(
    cols = starts_with('dropoff_24_') & ends_with('uss'),
    names_to = 'party',
    values_to = 'dropoff'
  ) |>
  mutate(
    party = case_when(
      str_detect(party, 'dem') ~ 'Dem.',
      str_detect(party, 'rep') ~ 'Rep.'
    ),
  ) |> 
  ggplot() + 
  geom_hline(
    aes(yintercept = 0), color = 'black'
  ) +
  geom_point(
    aes(x = ed_num, y = dropoff, color = party)
  ) + 
  geom_hline(
    data = elec_24_sum |> filter(town != 'Ramapo'),
    aes(yintercept = dropoff_24_dem_uss),
    linetype = 'solid', color = '#0063B1'
  ) +
  geom_hline(
    data = elec_24_sum |> filter(town != 'Ramapo'),
    aes(yintercept = dropoff_24_rep_uss),
    linetype = 'solid', color = '#A0442C'
  ) +
  facet_wrap(~town, scales = 'free_x') +
  scale_color_manual(
    values = c('Dem.' = '#638BC6', 'Rep.' = '#C27568'),
    name = 'Party'
  ) +
  theme_bw() + 
  scale_x_continuous(name = 'Election District Number within Town',
                     breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(name = 'President - US Senate Ballot Difference') +
  #lims(y = c(-25, 100)) +
  theme(strip.background = element_blank())


# Using House as baseline for dropoff ----
elec_24_precs |> 
  mutate(ed_num = as.integer(ed_num)) |>
  as_tibble() |> 
  filter(town != 'Ramapo') |> 
  pivot_longer(
    cols = starts_with('dropoff_24_') & ends_with('ush'),
    names_to = 'party',
    values_to = 'dropoff'
  ) |>
  mutate(
    party = case_when(
      str_detect(party, 'dem') ~ 'Dem.',
      str_detect(party, 'rep') ~ 'Rep.'
    ),
  ) |> 
  ggplot() + 
  geom_hline(
    aes(yintercept = 0), color = 'black'
  ) +
  geom_point(
    aes(x = ed_num, y = dropoff, color = party)
  ) + 
  geom_hline(
    data = elec_24_sum |> filter(town != 'Ramapo'),
    aes(yintercept = dropoff_24_dem_ush),
    linetype = 'solid', color = '#0063B1'
  ) +
  geom_hline(
    data = elec_24_sum |> filter(town != 'Ramapo'),
    aes(yintercept = dropoff_24_rep_ush),
    linetype = 'solid', color = '#A0442C'
  ) +
  facet_wrap(~town, scales = 'free_x') +
  scale_color_manual(
    values = c('Dem.' = '#638BC6', 'Rep.' = '#C27568'),
    name = 'Party'
  ) +
  theme_bw() + 
  scale_x_continuous(name = 'Election District Number within Town',
                     breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(name = 'President - US House Ballot Difference') +
  #lims(y = c(-25, 100)) +
  theme(strip.background = element_blank())

