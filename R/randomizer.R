#' Randomize quiz questions
#'
#' Randomize quiz questions
#'
#' @details
#' Assumes that there is one line per question and that the question column is
#'   named `"Question"` and that answers begin with `"Answer"`.
#'
#' @param data A data.frame with questions and possible answers
#' @param id An optional ID column.  If `NULL` will generate a numeric ID based
#'   on row order
#' @param randomize_choices Logical, if `TRUE` will also randomize the order of
#'   the choices
#' @param n_questions The total number of questions to sample.  If `NULL`,
#'   returns all; set to an integer to return that many or use a proportion of
#'   items where the total selected is rounded down.
#' @export
#' @examples
#' example_quiz <- data.frame(
#'   Question = c("First question?", "Second question?",
#'                "Third question?", "Fourth question?"),
#'   Answer_A = c("A", "1", "A1", "1A"),
#'   Answer_B = c("B", "2", "B2", "2B"),
#'   Answer_C = c("C", "3", "C3", "3C"),
#'   Answer_D = c("D", "4", "D4", "4D")
#' )
#'
#' randomize_quiz(example_quiz)
#' randomize_quiz(example_quiz, randomize_choices = TRUE)
#' randomize_quiz(example_quiz, n_questions = 2)
#' randomize_quiz(example_quiz, n_questions = 0.80)

randomize_quiz <- function(data, id = NULL, randomize_choices = FALSE, n_questions = NULL) {
  dims <- dim(data)
  stopifnot(is.data.frame(data),
            (n <- dims[1]) > 1)

  if (is.null(n_questions)) {
    n_questions <- dims[1]
  }

  if (n_questions <= 0) {
    stop("n_questions must be more than 0")
  } else if (n_questions != round(n_questions)) {
    n_questions <- floor(n_questions * dims[1])
  }

  stopifnot(n_questions > 1)

  ns <- 1:n
  cn <- colnames(data)

  if (!is.null(id)) {
    if (is.numeric) {
      if (id > dims[2])
      ids <- tryCatch(
        data[[id]],
        error = function(e) {
          warning("Index out of range, using row number as id",
                  call. = FALSE)
          id <- 0
          ns
        })
    } else if (is.character(id)) {
      if (!id %in% cn) {
        warning("Index name not found in data, using row number as id", call. = FALSE)
        id <- 0
        ns
      } else {
        ids <- data[[id]]
      }
    }

  } else {
    id <- 0
    ids <- ns
  }

  if (id == 0) {
    data <- cbind(id = ids, data)
    cn <- c("id", cn)
  }

  # questions <- grep("^[Qq]uestion", cn)

  out <- data[sample(ns, n_questions, replace = FALSE), ]
  rownames(out) <- NULL

  if (randomize_choices) {
    answers <- grep("^[Aa]nswer", cn)
    stopifnot((la <- length(answers)) > 1)

    ranswers <- apply(out, 1, function(x) {
      x[sample(answers, la, replace = FALSE)]
    })
    ranswers <- t(ranswers)
    colnames(ranswers) <- cn[answers]

    out <- cbind(out[, -answers, drop = FALSE], ranswers)
  }

  out
}
