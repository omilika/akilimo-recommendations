#!/usr/bin/Rscript --vanilla

this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	akpath <- "C:/github/omilika/akilimo-recommendations"
} else {
	akpath <- "."

}
setwd(akpath)
srcdir <- file.path(akpath, "R")
testdir <- file.path(akpath, "tests")

library(limSolve)
library(tidyr)
library(plyr)
library(dplyr)
library(randomForest)
library(caret)


for (f in grep("api", list.files(srcdir), invert=TRUE, value=TRUE)) source(file.path(srcdir, f))


test <- function(i) {
	cat(i, "\n")
	js <- readLines(paste0(testdir, gsub("xxx", i, "/input/in_xxx.json")))
	run_akilimo(js)
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

