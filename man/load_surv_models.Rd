% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabular_input.R
\name{load_surv_models}
\alias{load_surv_models}
\title{Load a set of survival fits}
\usage{
load_surv_models(location, survival_specs, use_envir)
}
\arguments{
\item{location}{base directory}

\item{survival_specs}{information about fits}

\item{use_envir}{an environment}
}
\value{
A list with two elements:  \itemize{
\item{\code{best_models},
a list with the fits for each data file passed in; and}
\item{\code{envir},
an environment containing the models so they can be referenced to
get probabilities.}
}
}
\description{
Load a set of survival fits
}
