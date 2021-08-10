
rm(list = ls(all=TRUE))

if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)


if(!require(devtools)){install.packages("devtools")}
library(devtools)

if(!require(knitr)){install.packages("knitr")}
library(knitr)

devtools::install_github("klutometis/roxygen")

library(roxygen2)


if(!require(testthat)){install.packages("testthat")}
library(testthat)

create("cats")

setwd("./cats")
document()



setwd("..")

install("cats")



install.packages("PrepCDM")
library("PrepCDM")

CreateConceptDatasets()

??CreateConceptDatasets


install_github('PrepCDM','relbersumcu')

setwd("C:/PrepCDM")
devtools::install()
usethis::use_roxygen_md()

roxygen2::roxygenise()

usethis::use_test("CreateConceptDatasets")

devtools::test()
rcmdcheck::rcmdcheck(args = "--no-manual", error_on = "error")


help(CreateConceptDatasets)
