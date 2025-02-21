% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ssimLibrary.R
\name{ssimLibrary}
\alias{ssimLibrary}
\alias{ssimLibrary,SsimObject-method}
\alias{ssimLibrary,missingOrNULLOrChar-method}
\title{Create or open a SsimLibrary}
\usage{
ssimLibrary(
  name = NULL,
  summary = NULL,
  package = NULL,
  session = NULL,
  addon = NULL,
  template = NULL,
  forceUpdate = FALSE,
  overwrite = FALSE,
  useConda = FALSE
)

\S4method{ssimLibrary}{SsimObject}(
  name = NULL,
  summary = NULL,
  package = NULL,
  session = NULL,
  addon = NULL,
  template = NULL,
  forceUpdate = FALSE,
  overwrite = FALSE,
  useConda = FALSE
)

\S4method{ssimLibrary}{missingOrNULLOrChar}(
  name = NULL,
  summary = NULL,
  package = NULL,
  session = NULL,
  addon = NULL,
  template = NULL,
  forceUpdate = FALSE,
  overwrite = FALSE,
  useConda = FALSE
)
}
\arguments{
\item{name}{\code{\link{SsimLibrary}}, \code{\link{Project}} or 
\code{\link{Scenario}} object, or character string (i.e. path to a SsimLibrary 
    or SsimObject)}

\item{summary}{logical. Default is \code{TRUE}}

\item{package}{character. The package type. Default is "stsim"}

\item{session}{\code{\link{Session}} object. If \code{NULL} (default), session()
will be used}

\item{addon}{character or character vector. One or more addon packages. See 
\code{\link{addon}} for options (optional)}

\item{template}{character. Creates the SsimLibrary with the specified template
(optional)}

\item{forceUpdate}{logical. If \code{FALSE} (default) user will be prompted to approve 
any required updates. If \code{TRUE}, required updates will be applied silently}

\item{overwrite}{logical. If \code{TRUE} an existing SsimLibrary will be overwritten}

\item{useConda}{logical. If set to TRUE, then all packages associated with the 
Library will have their Conda environments created and Conda environments will
be used during runtime.If set to FALSE, then no packages will have their 
Conda environments created and Conda environments will not be used during runtime.
Default is FALSE.}
}
\value{
Returns a \code{\link{SsimLibrary}} object.
}
\description{
Creates or opens a \code{\link{SsimLibrary}} object.
If \code{summary = TRUE}, returns SsimLibrary summary info.
If \code{summary = NULL}, returns SsimLibrary summary info if ssimObject is a SsimLibrary, 
SsimLibrary object otherwise.
}
\details{
Example arguments:
\itemize{
  \item {If name is SyncroSim Project or Scenario: }{Returns the 
         \code{\link{SsimLibrary}} associated with the Project or Scenario.}
  \item {If name is \code{NULL}: }{Create/open a SsimLibrary in the current working 
         directory with the filename SsimLibrary.ssim.}
  \item {If name is a string: }{If string is not a valid path treat as filename 
         in working directory. If no file suffix provided in string then add 
         .ssim. Attempts to open a SsimLibrary of that name. If SsimLibrary does not 
         exist creates a SsimLibrary of type package in the current working directory.}
  \item {If given a name and a package: }{Create/open a SsimLibrary called <name>.ssim. 
         Returns an error if the SsimLibrary already exists but is a different type 
         of package.}
}
}
\examples{
\donttest{
# Make sure packages are installed
addPackage("stsim")
addPackage("stsimsf")

# Create or open a SsimLibrary using the default Session
myLibrary <- ssimLibrary(name = file.path(tempdir(), "mylib"))

# Create SsimLibrary using a specific Session
mySession <- session()

myLibrary <- ssimLibrary(name = file.path(tempdir(), "mylib"),
                         session = mySession)

# Retrieve SsimLibrary properties
session(myLibrary)

# Load a SsimLibrary with addon package
myLibrary <- ssimLibrary(name = file.path(tempdir(), "mylib"),
                         overwrite = TRUE, package = "stsim",
                         addon = "stsimsf")

# Create SsimLibrary from template
addPackage("helloworldSpatial")
mySession <- session()
myLibrary <- ssimLibrary(name = file.path(tempdir(), "mylib"), 
                         session = mySession,
                         package = "helloworldSpatial",
                         template = "example-library",
                         overwrite = TRUE)
                         
}

}
