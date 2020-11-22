# nocov start
.onLoad <- function(libname, pkgname) {
  # Register external methods for 'dplyr' package. Not directly exporting
  # because it is not guaranteed that 'dplyr' is installed.
  if (requireNamespace("dplyr", quietly = TRUE)) {
    # Methods for longcr
    register_s3_method("dplyr", "select", "longcr", select.longcr)
    register_s3_method("dplyr", "rename", "longcr", rename.longcr)
    register_s3_method("dplyr", "mutate", "longcr", mutate.longcr)
    register_s3_method("dplyr", "summarise", "longcr", summarise.longcr)
    register_s3_method("dplyr", "group_by", "longcr", group_by.longcr)
    register_s3_method("dplyr", "ungroup", "longcr", ungroup.longcr)
    register_s3_method("dplyr", "distinct", "longcr", distinct.longcr)
    register_s3_method("dplyr", "do", "longcr", do.longcr)
    register_s3_method("dplyr", "arrange", "longcr", arrange.longcr)
    register_s3_method("dplyr", "filter", "longcr", filter.longcr)
    register_s3_method("dplyr", "slice", "longcr", slice.longcr)
    register_s3_method("dplyr", "inner_join", "longcr", inner_join.longcr)
    register_s3_method("dplyr", "left_join", "longcr", left_join.longcr)
    register_s3_method("dplyr", "right_join", "longcr", right_join.longcr)
    register_s3_method("dplyr", "full_join", "longcr", full_join.longcr)
    register_s3_method("dplyr", "semi_join", "longcr", semi_join.longcr)
    register_s3_method("dplyr", "anti_join", "longcr", anti_join.longcr)
    # Methods for widecr
    register_s3_method("dplyr", "select", "widecr", select.widecr)
    register_s3_method("dplyr", "rename", "widecr", rename.widecr)
    register_s3_method("dplyr", "mutate", "widecr", mutate.widecr)
    register_s3_method("dplyr", "summarise", "widecr", summarise.widecr)
    register_s3_method("dplyr", "group_by", "widecr", group_by.widecr)
    register_s3_method("dplyr", "ungroup", "widecr", ungroup.widecr)
    register_s3_method("dplyr", "distinct", "widecr", distinct.widecr)
    register_s3_method("dplyr", "do", "widecr", do.widecr)
    register_s3_method("dplyr", "arrange", "widecr", arrange.widecr)
    register_s3_method("dplyr", "filter", "widecr", filter.widecr)
    register_s3_method("dplyr", "slice", "widecr", slice.widecr)
    register_s3_method("dplyr", "inner_join", "widecr", inner_join.widecr)
    register_s3_method("dplyr", "left_join", "widecr", left_join.widecr)
    register_s3_method("dplyr", "right_join", "widecr", right_join.widecr)
    register_s3_method("dplyr", "full_join", "widecr", full_join.widecr)
    register_s3_method("dplyr", "semi_join", "widecr", semi_join.widecr)
    register_s3_method("dplyr", "anti_join", "widecr", anti_join.widecr)
    # Methods for h2h_long
    register_s3_method("dplyr", "select", "h2h_long", select.h2h_long)
    register_s3_method("dplyr", "rename", "h2h_long", rename.h2h_long)
    register_s3_method("dplyr", "mutate", "h2h_long", mutate.h2h_long)
    register_s3_method("dplyr", "summarise", "h2h_long", summarise.h2h_long)
    register_s3_method("dplyr", "group_by", "h2h_long", group_by.h2h_long)
    register_s3_method("dplyr", "ungroup", "h2h_long", ungroup.h2h_long)
    register_s3_method("dplyr", "distinct", "h2h_long", distinct.h2h_long)
    register_s3_method("dplyr", "do", "h2h_long", do.h2h_long)
    register_s3_method("dplyr", "arrange", "h2h_long", arrange.h2h_long)
    register_s3_method("dplyr", "filter", "h2h_long", filter.h2h_long)
    register_s3_method("dplyr", "slice", "h2h_long", slice.h2h_long)
    register_s3_method("dplyr", "inner_join", "h2h_long", inner_join.h2h_long)
    register_s3_method("dplyr", "left_join", "h2h_long", left_join.h2h_long)
    register_s3_method("dplyr", "right_join", "h2h_long", right_join.h2h_long)
    register_s3_method("dplyr", "full_join", "h2h_long", full_join.h2h_long)
    register_s3_method("dplyr", "semi_join", "h2h_long", semi_join.h2h_long)
    register_s3_method("dplyr", "anti_join", "h2h_long", anti_join.h2h_long)
  }

  invisible()
}

# Adapted from github.com/tidyverse/googledrive ('dplyr-compat.R')
register_s3_method <- function(pkg, generic, class, fun) {
  envir <- asNamespace(pkg)

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = envir)
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = envir)
    }
  )
}
# nocov end
