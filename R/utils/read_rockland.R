read_rockland <- function(x) {
  
  if (fs::path_ext(x) == 'xlsx') {
    z <- read_xlsx(x, .name_repair = \(x) vctrs::vec_as_names(x, repair = 'unique', quiet = TRUE)) |> 
      select(-starts_with('...'))
    noms <- paste(
      names(z),
      sapply(z |> slice(1:2), \(y) paste0(na.omit(y), collapse = '_')),
      sep = '_'
    ) |> 
      str_remove_all('\\.{3}\\d+')
  } else {
    z <- read_csv(x, col_names = TRUE, 
                  name_repair = \(x) vctrs::vec_as_names(x, repair = 'unique', quiet = TRUE),
                  na = c('', 'NA', 'N/A'), 
                  col_types = cols(.default = col_character())) |> 
      select(-starts_with('...'))
    noms <- paste(
      names(z),
      sapply(z |> slice(2), \(y) paste0(na.omit(y), collapse = '_')),
      sep = '_'
    ) |> 
      str_remove_all('\\.{3}\\d+') |> 
      str_remove('_$')
  }

  z |> 
    slice(-c(1:2)) |> 
    setNames(noms) |> 
    mutate(file = x)
}
