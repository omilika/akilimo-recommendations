#!/usr/bin/Rscript --vanilla

this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	akpath <- "C:/github/omilika/akilimo-recommendations"
} else {
	akpath <- "."
}
srcdir <- akpath
datadir <- akpath

#srcdir <- "."


library(limSolve)
library(tidyr)
library(plyr)
library(dplyr)
library(randomForest)
library(caret)
library(webshot)


#source(file.path(srcdir, "AkilimoFunctions_5D_orig.R"))
source(file.path(srcdir, "AkilimoFunctions_5D.R"))
source(file.path(srcdir, "AkilimoMain.R"))
source(file.path(srcdir, "process-FR.R"))
source(file.path(srcdir, "process-IC.R"))
source(file.path(srcdir, "process-PP.R"))
source(file.path(srcdir, "process-SP.R"))
source(file.path(srcdir, "sms_email.R"))

library(plumber)
pr <- pr(file.path(datadir, "api-wrapper.R"))
pr_set_debug(pr)
pr_run(pr, port = 8000)

