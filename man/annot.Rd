\name{annot}
\alias{annot}
\title{
Data extraction function.
}
\description{
Annotate multiple lists from a specific annotation file.
}
\usage{
annot(annot_file="", data_dir="", res_path="", display=TRUE)
}
\arguments{
  \item{annot_file}{
Path for the annotations file.
}
  \item{data_dir}{
Path of the folder where are placed the lists.
}
  \item{res_path}{
Path of the resulting annotated lists.
}
  \item{display}{
If informations could be displayed during the process.
}
}
\value{
The resulting annotated lists are placed in a folder named "AnnotLists_" in the working directory.
Thelists will be named: "Annot_" + lists names.
If a list could not be fully annotated a file - "Rest_" + lists names - with the renmaining unannotated identifers will be created.
The percentage of annotated identifier for each list is displayed during the process.
}
\references{
http://www.mrbioinfo.com
}
\author{
Nicolas Cagnard
}
