path_cvr_zip <- 'data-raw/cvrs/cvrs.zip'

if (!file_exists(here(path_cvr_zip))) {
  zip_raw <- dataverse::get_file_by_name('cvrs.zip', dataset = 'doi:10.7910/DVN/PQQ3KV')
  writeBin(zip_raw, here(path_cvr_zip))
  unzip(here(path_cvr_zip), exdir = here('data-raw/cvrs'))
}

cvrs <- arrow::open_dataset(here('data-raw/cvrs/release'))
# follow the code in the paper: 
# contested races in battlegrounds w/ senate + president
cvrs_sub <- cvrs |>
  filter(
    # drop GEORGIA too bc it had 2 senate races in 2020
    !state %in% c('GEORGIA'),
    # subset to just two offices
    office %in% c('US PRESIDENT', 'US SENATE')
  ) |>
  collect() |>
  # Contested contests
  filter(any(party == 'REP') & any(party == 'DEM'),
         .by = c(state, office, district)
  ) |>
  # Ballots with Presidential vote
  filter(any(office == 'US PRESIDENT'),
         .by = c(state, county_name, cvr_id)
  ) |> 
  # Ballots with Senate vote
  filter(any(office == 'US SENATE'),
         .by = c(state, county_name, cvr_id)
  )

cvrs_wide <- cvrs_sub |> 
  pivot_wider(
    id_cols = c(state, county_name, cvr_id),
    names_from = office,
    values_from = party
  ) |> 
  janitor::clean_names()

cvrs_national <- cvrs_wide |> 
  summarize(
    n = n(),
    n_dem = sum(us_president == 'DEM' & us_senate == 'DEM', na.rm = TRUE),
    n_rep = sum(us_president == 'REP' & us_senate == 'REP', na.rm = TRUE),
    n_demrep = sum(us_president == 'DEM' & us_senate == 'REP', na.rm = TRUE),
    n_repdem = sum(us_president == 'REP' & us_senate == 'DEM', na.rm = TRUE),
    n_demother = sum(us_president == 'DEM' & us_senate != 'REP' & us_senate != 'DEM', na.rm = TRUE),
    n_repother = sum(us_president == 'REP' & us_senate != 'REP' & us_senate != 'DEM', na.rm = TRUE),
    n_otherdem = sum(us_president != 'REP' & us_president != 'DEM' & us_senate == 'DEM', na.rm = TRUE),
    n_otherrep = sum(us_president != 'REP' & us_president != 'DEM' & us_senate == 'REP', na.rm = TRUE),
    n_other = n - (n_dem + n_rep + n_demrep + n_repdem + n_demother + n_repother + n_otherdem + n_otherrep),
    .groups = 'drop'
  )


cvrs_national_tbl <- tibble(
  Senate = c('Democratic', 'Republican', 'Other'),
  Democratic = c(cvrs_national$n_dem, 
           cvrs_national$n_demrep, 
           cvrs_national$n_demother),
  Republican = c(cvrs_national$n_repdem, 
            cvrs_national$n_rep,
            cvrs_national$n_repother),
  Other = c(cvrs_national$n_otherdem,
            cvrs_national$n_otherrep, 
            cvrs_national$n_other)
)

write_csv(
  cvrs_national_tbl,
  here('data/cvrs_national.csv')
)

cvrs_national_tbl |> 
  gt() |> 
  tab_spanner(
    label = 'President',
    columns = c(Democratic, Republican, Other)
  ) |> 
  fmt_number(
    columns = c(Democratic, Republican, Other),
    decimals = 0
  ) |> 
  gtsave(
    filename = here('figures/cvrs_national.html'),
    inline_css = TRUE
  )

cvrs_national_tbl |> 
  mutate(across(c(Democratic, Republican, Other), function(x) x / cvrs_national$n)) |> 
  gt() |> 
  tab_spanner(
    label = 'President',
    columns = c(Democratic, Republican, Other)
  ) |> 
  fmt_percent(
    columns = c(Democratic, Republican, Other),
    decimals = 1
  ) |> 
  gtsave(
    filename = here('figures/cvrs_national_percent.html'),
    inline_css = TRUE
  )

cvrs_stats <- cvrs_wide |> 
  group_by(
    state, county_name
  ) |> 
  summarize(
    n = n(),
    n_dem = sum(us_president == 'DEM' & us_senate == 'DEM', na.rm = TRUE),
    n_rep = sum(us_president == 'REP' & us_senate == 'REP', na.rm = TRUE),
    n_demrep = sum(us_president == 'DEM' & us_senate == 'REP', na.rm = TRUE),
    n_repdem = sum(us_president == 'REP' & us_senate == 'DEM', na.rm = TRUE),
    n_demother = sum(us_president == 'DEM' & us_senate != 'REP' & us_senate != 'DEM', na.rm = TRUE),
    n_repother = sum(us_president == 'REP' & us_senate != 'REP' & us_senate != 'DEM', na.rm = TRUE),
    n_otherdem = sum(us_president != 'REP' & us_president != 'DEM' & us_senate == 'DEM', na.rm = TRUE),
    n_otherrep = sum(us_president != 'REP' & us_president != 'DEM' & us_senate == 'REP', na.rm = TRUE),
    n_other = n - (n_dem + n_rep + n_demrep + n_repdem + n_demother + n_repother + n_otherdem + n_otherrep),
    .groups = 'drop'
  ) |> 
  mutate(
    across(starts_with('n_'), .fns = \(x) x / n * 100, .names = 'pct_{col}'),
    rolloff_dem = ((n_dem + n_demrep + n_demother) - (n_dem + n_repdem + n_otherdem)) / 
      (n_dem + n_demrep + n_demother),
    rolloff_rep = ((n_rep + n_repdem + n_repother) - (n_rep + n_demrep + n_otherrep)) /
      (n_rep + n_repdem + n_repother),
    pct_split = (n - n_dem - n_rep - n_other) / n * 100
  )

cvrs_stats |> 
  ggplot() + 
  geom_histogram(aes(x = pct_split), bins = 40) + 
  scale_x_continuous(
    name = 'Percent of Ballots with Split Vote by County',
    breaks = 0:14
  ) +
  scale_y_continuous(name = 'Number of Counties', breaks = 0:12) +
  theme_blog()

ggsave(
  filename = here('figures/cvrs_split_vote.png'),
  width = 8, height = 6, dpi = 300
)

elec_24_comparison <- elec_24_sum |> 
  summarize(
    across(where(is.numeric), sum),
    dropoff_24_dem_uss = (((pre_24_dem_sum) - uss_24_dem_sum) / pre_24_dem_sum),
    dropoff_24_rep_uss = (((pre_24_rep_sum) - uss_24_rep_sum) / pre_24_rep_sum)
  ) |> 
  select(
    dropoff_24_dem_uss, dropoff_24_rep_uss
  ) |> 
  pivot_longer(
    cols = everything(),
    names_to = 'rolloff_type',
    values_to = 'value'
  ) |>
  mutate(
    rolloff_type = recode(rolloff_type, 
                          dropoff_24_dem_uss = 'Democratic',
                          dropoff_24_rep_uss = 'Republican')
  )
  

cvrs_stats |> 
  select(state, county_name, starts_with('rolloff')) |>
  pivot_longer(
    cols = c(rolloff_dem, rolloff_rep),
    names_to = 'rolloff_type',
    names_prefix = 'rolloff_'
  ) |> 
  mutate(
    rolloff_type = recode(rolloff_type, dem = 'Democratic', rep = 'Republican')
  ) |> 
  ggplot() + 
  geom_histogram(aes(x =  value), bins = 40) + 
  geom_vline(
    data = elec_24_comparison,
    aes(xintercept = value, color = rolloff_type),
    linetype = 'dashed'
  ) +
  facet_wrap(~rolloff_type) +  
  scale_x_continuous(name = 'President - US Senate Ballot Difference', labels = scales::label_percent(),
                     breaks = scales::pretty_breaks()) + 
  scale_y_continuous(name = 'Number of Counties') +
  scale_color_manual(
    values = c('Democratic' = '#638BC6', 'Republican' = '#C27568'),
    name = 'Party'
  ) +
  theme_blog() 

ggsave(
  filename = here('figures/cvrs_rolloff.png'),
  width = 8, height = 6, dpi = 300
)
