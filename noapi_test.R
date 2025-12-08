#!/usr/bin/Rscript --vanilla

this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	akpath <- "C:/github/omilika/akilimo-recommendations"
} else {

}
srcdir <- akpath
datadir <- akpath
testdir <- file.path(akpath, "tests")

#srcdir <- "."


library(limSolve)
library(tidyr)
library(plyr)
library(dplyr)
library(randomForest)
library(caret)


#source(file.path(srcdir, "AkilimoFunctions_5D_orig.R"))
source(file.path(srcdir, "api-wrapper.R"))
source(file.path(srcdir, "AkilimoFunctions_5D.R"))
source(file.path(srcdir, "AkilimoMain.R"))
source(file.path(srcdir, "process-FR.R"))
source(file.path(srcdir, "process-IC.R"))
source(file.path(srcdir, "process-PP.R"))
source(file.path(srcdir, "process-SP.R"))


test <- function(i) {
	cat(i, "\n")
	js <- readLines(paste0(testdir, gsub("xxx", i, "/input/in_xxx.json")))
	process_json(js)
	cat("--- + ---\n")
}

out <- lapply(1:27, test)

cmp <- readRDS(file.path(testdir, "test_out.rds"))

for (i in 1:27) {
	x <- jsonlite::fromJSON(cmp[[i]])$data
	y <- out[[i]]$data
	a <- tinytest::expect_equal(x$recommendation, y$recommendation[1])
	if (!a) print(a)
	if ((length(x$recommendations) > 0) & (length(y$recommendations) > 0)) {
		b <- tinytest::expect_equivalent(x$recommendations, y$recommendations, tolerance=0.0001)
		if (!b) print(b)
	}
	cat(i, " ------\n")
}

