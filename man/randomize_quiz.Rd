% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/randomizer.R
\name{randomize_quiz}
\alias{randomize_quiz}
\title{Randomize quiz questions}
\usage{
randomize_quiz(data, id = NULL, randomize_choices = FALSE, n_questions = NULL)
}
\arguments{
\item{data}{A data.frame with questions and possible answers}

\item{id}{An optional ID column.  If \code{NULL} will generate a numeric ID based
on row order}

\item{randomize_choices}{Logical, if \code{TRUE} will also randomize the order of
the choices}

\item{n_questions}{The total number of questions to sample.  If \code{NULL},
returns all; set to an integer to return that many or use a proportion of
items where the total selected is rounded down.}
}
\description{
Randomize quiz questions
}
\details{
Assumes that there is one line per question and that the question column is
named \code{"Question"} and that answers begin with \code{"Answer"}.
}
\examples{
example_quiz <- data.frame(
  Question = c("First question?", "Second question?",
               "Third question?", "Fourth question?"),
  Answer_A = c("A", "1", "A1", "1A"),
  Answer_B = c("B", "2", "B2", "2B"),
  Answer_C = c("C", "3", "C3", "3C"),
  Answer_D = c("D", "4", "D4", "4D")
)

randomize_quiz(example_quiz)
randomize_quiz(example_quiz, randomize_choices = TRUE)
randomize_quiz(example_quiz, n_questions = 2)
randomize_quiz(example_quiz, n_questions = 0.80)
}
