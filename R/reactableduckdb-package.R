#' @keywords internal
#' @importFrom lifecycle deprecated
#' @importFrom rlang %||%
"_PACKAGE"

# The custom server backend API this package builds on. It exists only in
# the reactable *development* version (>= 0.4.5.9000). The canonical source
# is the GitHub repository (glin/reactable); a Posit Package Manager
# repository that mirrors it is a convenient distribution channel rather
# than a requirement.
required_reactable_exports <- c(
  "reactableServerInit",
  "reactableServerData",
  "resolvedData"
)

required_resolveddata_formals <- c("data", "rowCount", "maxRowCount")

has_reactable_backend_api <- function() {
  exports <- tryCatch(
    getNamespaceExports("reactable"),
    error = function(cnd) character(0)
  )
  if (!all(required_reactable_exports %in% exports)) {
    return(FALSE)
  }
  # Deliberately a subset check rather than identical(): the development
  # version is a moving target (the mirror tracks the latest repository
  # state), so arguments added upstream must not make a working install
  # look broken.
  resolved_formals <- tryCatch(
    names(formals(reactable::resolvedData)),
    error = function(cnd) character(0)
  )
  all(required_resolveddata_formals %in% resolved_formals)
}

reactable_install_hint <- function() {
  c(
    "i" = "reactableduckdb needs the reactable {.emph development} version (>= 0.4.5.9000), which exports {.fn reactableServerInit}, {.fn reactableServerData}, and {.fn resolvedData}.",
    "*" = "Install it from GitHub: {.code pak::pak(\"glin/reactable\")}",
    "*" = "Or from a Posit Package Manager repository that mirrors that repository."
  )
}

# Called from every exported entry point. This warns rather than aborts, on
# purpose: the reactable development version is a moving target, so a failed
# detection is at least as likely to mean this check has gone stale as it is
# to mean the install is unusable. If the API genuinely is missing, the
# failure still surfaces loudly and immediately from reactable itself (an
# unused `server` argument, or a missing `resolvedData`) — this warning just
# explains it in advance, and names both ways to fix it.
#
# This is not a hole in the refusal principle: nothing here returns wrong
# data or silently degrades to eager execution. It only declines to
# second-guess a dependency whose exact signature we cannot pin.
warn_reactable_backend_api <- function(has_api = has_reactable_backend_api()) {
  if (has_api) {
    return(invisible(TRUE))
  }
  installed <- tryCatch(
    as.character(utils::packageVersion("reactable")),
    error = function(cnd) "not installed"
  )
  cli::cli_warn(
    c(
      "!" = "The installed reactable ({.val {installed}}) does not appear to export the custom server backend API.",
      reactable_install_hint()
    ),
    class = "reactableduckdb_api_warning",
    .frequency = "once",
    .frequency_id = "reactableduckdb-backend-api"
  )
  invisible(FALSE)
}

.onAttach <- function(libname, pkgname) {
  if (!has_reactable_backend_api()) {
    packageStartupMessage(
      "reactableduckdb: the installed reactable does not appear to export ",
      "the custom server backend API. Install the development version ",
      "(>= 0.4.5.9000) from GitHub (glin/reactable) or from a Posit ",
      "Package Manager repository that mirrors it."
    )
  }
}
