#!/usr/bin/Rscript --vanilla

this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	akpath <- "C:/github/omilika/akilimo-recommendations"
} else {
	akpath <- "."
}
srcdir <- file.path(akpath, "R")


library(limSolve)
library(tidyr)
library(plyr)
library(dplyr)
library(randomForest)
library(caret)
library(webshot)

ff <- grep("api", list.files(srcdir), invert=TRUE, value=TRUE)
for (f in ff) source(file.path(srcdir, f))

library(plumber)
pr <- pr(file.path(akpath, "api-wrapper.R"))
pr_set_debug(pr)
pr_run(pr, port = 8000)

