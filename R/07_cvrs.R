path_cvr_zip <- 'data-raw/cvrs/cvrs.zip'

if (!file_exists(here(path_cvr_zip))) {
  zip_raw <- dataverse::get_file_by_name('cvrs.zip', dataset = 'doi:10.7910/DVN/PQQ3KV')
  writeBin(zip_raw, here(path_cvr_zip))
  unzip(here(path_cvr_zip), exdir = here('data-raw/cvrs'))
}