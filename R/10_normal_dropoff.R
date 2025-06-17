drop_24 <- elec_24 |>
  as_tibble() |> 
  select(
    -ends_with(c('_ove', '_und'))
  ) |> 
  rowwise() |> 
  mutate(
    pre_24 = sum(c_across(starts_with('pre_24_'))),
    uss_24 = sum(c_across(starts_with('uss_24_'))),
    ush_24 = sum(c_across(starts_with('ush_24_')))
  ) |> 
  ungroup() |>
  select(ed_key:ballot_total_24, pre_24, uss_24, ush_24) |> 
  mutate(
    dropoff_24_pre_24 = pre_24 / ballot_total_24,
    dropoff_24_uss_24 = uss_24 / ballot_total_24,
    dropoff_24_ush_24 = ush_24 / ballot_total_24,
    ed_num = as.integer(ed_num)
  )

drop_24 |> 
  ggplot() +
  geom_point(aes(x = ed_num, y = dropoff_24_pre_24)) +
  facet_wrap(~town, scales = 'free_x') +
  scale_x_continuous(name = 'Election District Number within Town',
                     breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(name = 'Percent of Ballots Casting a Vote for President',
                     labels = scales::label_percent(), limits = c(0, 1)) +
  theme_blog()

ggsave(
  filename = here('figures/dropoff_pre_24.png'),
  width = 8, height = 6, dpi = 300
)

drop_24 |> 
  ggplot() +
  geom_point(aes(x = ed_num, y = dropoff_24_uss_24, size = ballot_total_24)) +
  facet_wrap(~town, scales = 'free_x') +
  scale_x_continuous(name = 'Election District Number within Town',
                     breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(name = 'Percent of Ballots Casting a Vote for US Senate',
                     labels = scales::label_percent(), limits = c(0, 1)) +
  scale_size_continuous(name = 'Number of Ballots Cast', range = c(0.1, 3)) +
  theme_blog() + 
  theme(
    legend.position = 'inside', 
    legend.position.inside = c(0.8, 0.2)
  )

ggsave(
  filename = here('figures/dropoff_uss_24.png'),
  width = 8, height = 6, dpi = 300
)

drop_24 |> 
  ggplot() +
  geom_point(aes(x = ed_num, y = dropoff_24_ush_24, size = ballot_total_24)) +
  facet_wrap(~town, scales = 'free_x') +
  scale_x_continuous(name = 'Election District Number within Town',
                     breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(name = 'Percent of Ballots Casting a Vote for US House',
                     labels = scales::label_percent(), limits = c(0, 1)) +
  scale_size_continuous(name = 'Number of Ballots Cast', range = c(0.1, 3)) +
  theme_blog() + 
  theme(
    legend.position = 'inside', 
    legend.position.inside = c(0.8, 0.2)
  )

ggsave(
  filename = here('figures/dropoff_ush_24.png'),
  width = 8, height = 6, dpi = 300
)
