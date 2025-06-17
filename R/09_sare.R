sloatsburg_area <- towns |> 
  slice(27, 31, 35) # Montebello, Sloatsburg, and unnamed area

elec_24_sloats <- elec_24 |> 
  geo_filter(to = sloatsburg_area) |> 
  geo_trim(to = sloatsburg_area) |>
  mutate(affidavit = ed_key %in% c('R39', 'R62'))

elec_24_sloats |> 
  ggplot() + 
  geom_sf(aes(fill = affidavit)) + 
  geom_sf(
    data = sloatsburg_area,
    fill = NA,
    linewidth = 2,
    color = 'white'
  ) +
  geom_sf(fill = NA) +
  geom_sf_text(aes(label = ed_key), size = 3, color = 'black') + 
  geom_district_text(
    data = sloatsburg_area,
    aes(label = VILLAGE, grouping = VILLAGE),
    color = 'black'
  ) +
  scale_fill_manual(
    values = c('TRUE' = '#CC742A', 'FALSE' = '#E9D9A1'),
    name = 'Sare Affidavit Filing',
    labels = c('TRUE' = 'Affidavit for District', 'FALSE' = 'No District Affidavit')
  ) +
  theme_map()

ggsave(
  filename = here('figures/sloatsburg_area_affidavit.png'),
  width = 8, height = 6, dpi = 300
)

elec_24_sloats |> 
  ggplot() + 
  geom_sf(aes(fill = uss_24_lar)) + 
  geom_sf(
    data = sloatsburg_area,
    fill = NA,
    linewidth = 2,
    color = 'white'
  ) +
  geom_sf(fill = NA) +
  geom_sf_text(aes(label = ed_key), size = 3, color = 'black') + 
  geom_district_text(
    data = sloatsburg_area,
    aes(label = VILLAGE, grouping = VILLAGE),
    color = 'black'
  ) +
  scale_fill_continuous(
    name = 'Votes for Sare',
    low = 'white', high = '#CC742A'
  ) +
  theme_map()

ggsave(
  filename = here('figures/sloatsburg_area_sare.png'),
  width = 8, height = 6, dpi = 300
)

