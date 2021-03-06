% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runStudy.R
\name{runStudy}
\alias{runStudy}
\title{runStudy}
\usage{
runStudy(connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  resultsDatabaseSchema = resultsDatabaseSchema, cdmVersion = cdmVersion,
  outComeId = outComeId, outComeName = outComeName, numThread = numThread,
  results_path = results_path)
}
\arguments{
\item{connectionDetails}{The connection details of the database.}

\item{cdmDatabaseSchema}{The name of cdm database schema.}

\item{resultsDatabaseSchema}{The name of results database schema.}

\item{cdmVersion}{The name of cdm version, should be 5}

\item{outComeId}{The outcome Id for which study need to be executed (3 = HbA1c, 4 = MI, 5 = KD and 6 = ED)}

\item{outComeName}{Name of the outcome.}

\item{numThread}{Number of threads.}
}
\details{
This function can be used to perform the DiabetesTxPathway analysis
for a given outcome of interest. Briefly, for a given drug combinations, the
function will construct a treatment, a comparator and an outcome cohort. Will
use the CohortMethod and perform the analysis.
}
\author{
Rohit Vashisht
}
