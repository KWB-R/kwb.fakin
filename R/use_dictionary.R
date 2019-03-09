# use_dictionary ---------------------------------------------------------------
use_dictionary <- function(x, dict, method = "full")
{
  if (method == "full") {

    indices <- match(x, as.character(dict))

    found <- ! is.na(indices)

    x[found] <- toPlaceholder(names(dict)[indices[found]])

  } else if (method == "part") {

    # indices <- order(as.character(dict))
    keys <- names(dict)

    # for (index in indices) {
    n <- length(keys)

    for (i in seq_len(n)) {

      key <- keys[i]

      # subst <- dict[index]
      pattern <- dict[[key]]

      replacement <- toPlaceholder(key)

      # kwb.utils::printIf(TRUE, subst, "Applying substitution")

      if (i %% 20 == 0) {

        cat(sprintf(
          "%4.1f %% Substituting '%s' with '%s'...\n",
          100 * i/n, pattern, replacement
        ))
      }

      x <- gsub(pattern, replacement, x, fixed = TRUE)
    }

  } else {

    stop(
      "use_dictionary(): method must be one of 'full', 'part'", call. = FALSE
    )
  }

  x
}
