get_na_table <- function(df, vars) {
  # asegurar que vars sea un vector de caracteres
  if (is.list(vars)) {
    vars <- unique(unlist(vars, recursive = TRUE, use.names = FALSE))
  }
  if (!is.character(vars)) {
    stop("`vars` debe ser un vector de caracteres o una lista de caracteres.")
  }
  
  missing_vars <- setdiff(vars, names(df))
  if (length(missing_vars) > 0) {
    warning("Estas variables no existen en el data.frame: ", paste(missing_vars, collapse = ", "))
  }
  
  # función para obtener la moda
  get_mode <- function(x) {
    if (all(is.na(x))) return(NA_character_)
    ux <- na.omit(unique(x))
    tab <- tabulate(match(x, ux))
    as.character(ux[which.max(tab)])
  }
  
  res <- df %>%
    select(any_of(vars)) %>%
    summarise(across(
      everything(),
      list(
        na_count  = ~ sum(is.na(.)),
        min_value = ~ as.character(suppressWarnings(min(., na.rm = TRUE))),
        max_value = ~ as.character(suppressWarnings(max(., na.rm = TRUE))),
        mean      = ~ as.character(suppressWarnings(mean(as.numeric(.), na.rm = TRUE))),
        median    = ~ as.character(suppressWarnings(median(as.numeric(.), na.rm = TRUE))),
        mode      = ~ get_mode(.)
      ),
      .names = "{col}_{fn}"
    )) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("variable", ".value"),
      names_pattern = "(.*)_(na_count|min_value|max_value|mean|median|mode)"
    ) %>%
    mutate(
      total = nrow(df),
      pct   = na_count / total * 100,
      non_na = total - na_count
    ) %>%
    arrange(desc(na_count))
  
  return(res)
}




