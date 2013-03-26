\name{detect.bad.points}
\alias{detect.bad.points}
\title{Detect bad fitting points on data}
\usage{
  detect.bad.points(xx, m, indexes = 1:nrow(xx$data))
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

