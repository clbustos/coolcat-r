\name{attributesTable.partition}
\alias{attributesTable.partition}
\title{Percentages or frequency for attribute, ordered by cluster}
\usage{
  attributesTable.partition(x, dd = NULL,
    values = "percentage")
}
\arguments{
  \item{x}{partition object}

  \item{dd}{data.frame with original data. If not used, use
  the data from partition object}

  \item{total}{if true, returns counts, not percentages
  (useful for X^2 tests)}
}
\value{
  a list. Every dataframe with percentages has the name of
  the attribute
}
\description{
  Percentages or frequency for attribute, ordered by
  cluster
}

