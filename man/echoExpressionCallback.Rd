% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_functions.R
\name{echoExpressionCallback}
\alias{echoExpressionCallback}
\title{This just echos the input expressoion}
\usage{
echoExpressionCallback(expr, value, status, visible, data)
}
\arguments{
\item{value}{result of the expression evaluation}

\item{status}{logical indicating success or not}

\item{visible}{was the output printed}

\item{param}{s-language expression}

\item{a}{data object that is accessible to the callback ( passed in from addTaskCallback)}
}
\value{
I guess this indicates if the callback succeeded?
}
\description{
This just echos the input expressoion
}