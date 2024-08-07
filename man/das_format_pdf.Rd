% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/das_format_pdf.R
\name{das_format_pdf}
\alias{das_format_pdf}
\title{DAS format requirements}
\usage{
das_format_pdf(file, ...)
}
\arguments{
\item{file}{character, the name of the file where the PDF will be saved}

\item{...}{passed on to \code{\link[base:files]{file.copy}}; might included named argument \code{overwrite}}
}
\value{
output of \code{\link[base:files]{file.copy}};
  \code{TRUE} if writing of file was successful, and \code{FALSE} otherwise
}
\description{
Save the PDF document describing the DAS format required by \code{swfscDAS} to a specified file
}
\details{
A wrapper function for \code{\link[base:files]{file.copy}}.
  This function saves the PDF document describing the DAS data format requirements by
  copying the PDF document located at \code{system.file("DAS_Format.pdf", package = "swfscDAS")}
  to \code{file}

  This file can also be downloaded from
  \url{https://github.com/swfsc/swfscDAS/blob/master/inst/DAS_Format.pdf}
}
\examples{
das_format_pdf(file.path(tempdir(), "DAS_Format.pdf"), overwrite = FALSE)

}
