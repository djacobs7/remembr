% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/call_counts.R
\name{convertCallCountsToHashTable}
\alias{convertCallCountsToHashTable}
\title{convert call counts to hash table}
\usage{
convertCallCountsToHashTable(call_counts_hash_table, time = NULL)
}
\arguments{
\item{call_counts_hash_table}{A call counts hash table ( like the one you would
get from getCallCountsHashTable() )}

\item{time}{The current time.  So that the hash table can have the corret time
since you last reviewed.}
}
\description{
Helper method for parsing the call_counts_hash_table environment and presenting it as a data frame
}
