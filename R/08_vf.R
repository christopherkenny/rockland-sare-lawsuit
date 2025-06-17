vf <- vf::vf_read_ny(here('data-private/AllNYSVoters_20250616.txt')) |> 
  filter(county == 44)

vf <- vf |> 
  mutate(
    voted_2024 = str_detect(voter_history, 'GENERAL ELECTION 2024')
  )

voters <- read_csv(here('data-private/vf.csv'))

voters |> 
  select(-name_middle) |> 
  left_join(vf, by = join_by(election_district, name_last, name_first, name_suffix)) |> 
  select(voter_id, election_district, name_last, name_first, voted_2024)
