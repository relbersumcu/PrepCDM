
rm(list = ls(all=TRUE))

invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)),   # Unload add-on packages
                 detach,
                 character.only = TRUE, unload = TRUE))


if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)


if(!require(devtools)){install.packages("devtools")}
library(devtools)

if(!require(knitr)){install.packages("knitr")}
library(knitr)

if(!require(devtools)) {devtools::install_github("klutometis/roxygen")}
library(roxygen2)

if(!require(testthat)){install.packages("testthat")}
library(testthat)


#Test functions according to specified unit tests
x <- NULL
x <- devtools::test(stop_on_failure = T)
#devtools::check()



#If test is correct then
if(!is.null(x)){

      #update RD, Namespace
      #roxygen2::roxygenise()
      devtools::document(pkg = ".", roclets = c('rd', 'collate', 'namespace', 'vignette'), quiet = FALSE)

      #Build
      devtools::build(quiet = T)

      #Detach all user installed packages
      invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)),   # Unload add-on packages
                       detach,
                       character.only = TRUE, unload = TRUE))


      #Reinstall
      if("PrepCDM" %in% .packages(all.available = TRUE)) remove.packages("PrepCDM")
      library("devtools")
      devtools::install()

      library("PrepCDM")

      help(package = "PrepCDM")



}



#install_github('PrepCDM','relbersumcu')






