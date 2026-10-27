#' Snail Weather Rhyme
#'
#' Display a rhyme asking a snail to
#' predict weather or make rain.
#'
#' @param rhyme A character or serial number defining the rhyme to return. Default "Scotland".
#' @param collapse Optional character string used to join the lines.
#'   If `NULL` (default), return the lines as a character vector.
#'
#' @return A character vector if `collapse = NULL`, otherwise a
#'   character string.
#'
#' @examples
#' snail_rhyme("Finland")
#'
#'
#' @export
snail_rhyme <- function(rhyme = c("Scotland", "Finland", "Somerset"),
                        collapse = NULL) {

  rhymes <- c("Scotland", "Finland", "Somerset")

  if(!is.numeric(rhyme)) {
    # rhyme <- sapply(rhymes, function(x) stringr::str_detect(x, rhyme)) |> which()
    rhyme <- sapply(rhymes, function(x) grepl(rhyme, x)) |> which()
  }

  verse <- switch(rhyme[1],
         Scotland = c(
           "Snailie, snailie, shoot out your horn,",
           "And tell us if it will be a bonnie day the morn."
         ),
         Finland = c(
           "Etana, etana n\u00e4yt\u00e4 sarves",
           "tuleeko huomenna pouta."
         ),
         Somerset = c(
           "Snail, snail, put out your horn,",
           "We want some rain to grow our corn.",
           "Out, horn, out."
         )
  )

  if (is.null(collapse)) {
    verse
  } else {
    paste(verse, collapse = collapse)
  }
}
