towns <- st_read(here('data-raw/gis/Towns_Village_Thalweg.shp'))

weird_precs <- elec_24_precs |>
  filter(town == 'Ramapo') |>
  mutate(
    weird = pre_24_dem_sum < 10 & uss_24_dem_sum > 100
  ) |> 
  filter(weird)

elec_24_precs |>
  filter(town == 'Ramapo') |>
  mutate(
    dropoff_24_dem_uss_raw = pre_24_dem_sum - uss_24_dem_sum,
  ) |>
  ggplot() +
  geom_sf(aes(fill = dropoff_24_dem_uss_raw)) +
  geom_sf(
    data = towns |> filter(TOWN == 'Ramapo', !is.na(VILLAGE)),
    fill = NA,
    color = 'white'
  ) + 
  geom_sf_text(
    data = weird_precs, 
    aes(label = ed_num),
    size = 3,
    color = 'white'
  ) +
  geom_sf_text(
    data = towns |> filter(TOWN == 'Ramapo', !is.na(VILLAGE)),
    aes(label = VILLAGE),
    size = 3,
    color = 'black'
  ) +
  scale_fill_wa_c(
    na.value = 'white',
    name = 'Democratic Dropoff\nPresident - Senate',
    palette = 'puget',
    breaks = c(100, 0, -200, -400, -600, -800)
  ) +
  theme_map()

ggsave(
  filename = here('figures/ramapo_24_dem_dropoff.png'),
  width = 8, height = 6, dpi = 300
)

weird_precs <- elec_20_precs |>
  filter(town == 'Ramapo') |>
  mutate(
    weird = pre_20_dem_sum < 10 & ush_20_dem_sum > 100
  ) |> 
  filter(weird)

elec_20_precs |>
  filter(town == 'Ramapo') |>
  mutate(
    dropoff_20_dem_ush_raw = pre_20_dem_sum - ush_20_dem_sum,
  ) |>
  ggplot() +
  geom_sf(aes(fill = dropoff_20_dem_ush_raw)) +
  geom_sf(
    data = towns |> filter(TOWN == 'Ramapo', !is.na(VILLAGE)),
    fill = NA,
    color = 'white'
  ) + 
  geom_sf_text(
    data = weird_precs, 
    aes(label = ed_num),
    size = 3,
    color = 'white'
  ) +
  geom_sf_text(
    data = towns |> filter(TOWN == 'Ramapo', !is.na(VILLAGE)),
    aes(label = VILLAGE),
    size = 3,
    color = 'black'
  ) +
  scale_fill_wa_c(
    na.value = 'white',
    name = 'Democratic Dropoff\nPresident - House',
    palette = 'puget',
    breaks = c(100, 0, -200, -400, -600, -800)
  ) +
  theme_map()

ggsave(
  filename = here('figures/ramapo_20_dem_dropoff.png'),
  width = 8, height = 6, dpi = 300
)
