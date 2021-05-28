# library(gtools)
load_all()

cmdArgs <- commandArgs(trailingOnly = FALSE)
print(cmdArgs)

file <- script_file("warning")
cat("script_file:", file, "\n")
stopifnot(endsWith(file, "gtools/tests/test_script_file.R"))

path <- script_path("warning")
cat("script_path:", path, "\n")
stopifnot(endsWith(path, "gtools/tests"))

save.image("test_script_file.Rda")
