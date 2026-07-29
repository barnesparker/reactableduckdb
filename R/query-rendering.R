# Rendering and predicate application: the seam between the lazy dbplyr
# query and the raw-SQL wrapper. Filter values only ever enter SQL here,
# through dbplyr expressions, which bind them as escaped literals.

render_sql <- function(tbl) {
  as.character(dbplyr::sql_render(tbl))
}

# Apply parsed filter predicates lazily. No predicates -> the base query
# unchanged.
apply_predicates <- function(tbl, predicates) {
  if (length(predicates) == 0) {
    return(tbl)
  }
  dplyr::filter(tbl, !!!predicates)
}
