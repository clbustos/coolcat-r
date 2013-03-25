\docType{package}
\name{coolcat}
\alias{coolcat}
\alias{coolcat-package}
\title{coolcat}
\usage{
  coolcat(x, k, m.replacement = 0.1, batch = 100,
    final.refitting = T, trace.log = F)
}
\arguments{
  \item{x}{dataframe}

  \item{k}{clusters number}

  \item{m.replacement}{proportion of point to re-place
  after each batch}

  \item{batch}{batch size for replacement of bad fitting
  points}

  \item{final.refitting}{If TRUE, will do a final refitting
  of all items}

  \item{trace.log}{If TRUE, writes a log of actions}
}
\value{
  a partition object
}
\description{
  coolcat

  Coolcat! Algoritmo de clustering, basado en la entropia.
  Tiene su gracia Reemplazo la búsqueda de desigualdad
  inicial basado en entropia por una basada en Humming.
  Estoy seguro que es monotónico creciente la función entre
  ambas, así que no debería ser problema.
}
