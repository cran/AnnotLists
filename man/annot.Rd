\name{annot}
\alias{annot}
\title{
Data extraction function.
}
\description{
Extract data annotations about lists of identifiers or microarray probes ID from an annotations file.
Batch processing of identifiers lists is allowed.
}
\usage{
annot(n, path)
}
\arguments{
  \item{n}{
TRUE/FALSE to use the batch processing mode.
}
  \item{path}{
Folder path where are placed the resulting files. Default path is the working directory.
}
}
\value{
Annotated data files are placed @ path or in your working directoty.
If the batch mode is used, the annotated lists would be named: list names + "_annot.txt".
If only one list is annotated the resulting list is named "list_annot.txt".
}
\references{
http://www.mrbioinfo.com
}
\author{
Nicolas Cagnard
}
