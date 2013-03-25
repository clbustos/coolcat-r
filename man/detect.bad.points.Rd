\name{detect.bad.points}
\alias{detect.bad.points}
\title{Detect bad fitting points on data}
\usage{
  detect.bad.points(xx, m, first = 1, last = nrow(xx$data))
}
\arguments{
  \item{xx}{partition object}

  \item{m}{percentage of data to refit}

  \item{index}{for first element to scan}

  \item{index}{for last element to scan}
}
\value{
  indexes for bad fit data
}
\description{
  Detect bad fitting points on data
}

