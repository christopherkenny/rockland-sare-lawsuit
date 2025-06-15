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