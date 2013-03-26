\name{detectBadPoints}
\alias{detectBadPoints}
\title{Detect bad fitting points on data}
\usage{
  detectBadPoints(xx, m, indexes = 1:nrow(xx$data))
}
\arguments{
  \item{xx}{partition object}

  \item{m}{percentage of data to refit}

  \item{first}{for first element to scan}

  \item{last}{for last element to scan}
}
\value{
  indexes for bad fit data
}
\description{
  Detect bad fitting points on data
}

