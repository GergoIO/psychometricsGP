% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fnStartFolders.R
\name{fnStartFolders}
\alias{fnStartFolders}
\title{Run functions to create and set variables for save folders}
\usage{
fnStartFolders(
  workingDirectory = NULL,
  verNum = NULL,
  createOutputFolder = NULL,
  createPulseSubfolder = NULL,
  createImagesFolder = NULL
)
}
\arguments{
\item{workingDirectory}{Define the working directory, where save folders will be created}

\item{verNum}{Set a version number - appended to save folders and file names}

\item{createOutputFolder}{(TRUE/FALSE/NULL) Create an outputs folder in the working directory (with appended version number) if TRUE}

\item{createImagesFolder}{(TRUE/FALSE/NULL) Create an Images folder in the working directory (with appended version number) if TRUE}

\item{createPulseFolder}{(TRUE/FALSE/NULL) Create a folder called Pulse in the Output folder if TRUE}
}
\value{
The working directory is set to the location of the script file and save as variable folderWD.
Two folders for figures and other outputs are also created and saved as folderOutput and folderPlots
}
\description{
Run functions to create and set variables for save folders
}
\examples{
fnStartFolders(workingDirectory = getwd(), verNum = "v1", createOutputFolder = TRUE, createPulseSubfolder = FALSE, createImagesFolder = TRUE)
}