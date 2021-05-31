library(gtools)

# For debugging conveneince
cmdArgs <- commandArgs(trailingOnly = FALSE)
print(cmdArgs)

file <- script_file("warning") 
cat("script_file:", file, "\n")
stopifnot(endsWith(file, "test_script_file.R"))

path <- script_path("warning")
cat("script_path:", path, "\n")
# stopifnot(endsWith(path, "tests"))

# save.image("test_script_file.Rda")
