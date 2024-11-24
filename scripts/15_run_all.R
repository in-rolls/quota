# Define the directory containing the R scripts
scripts_dir <- "scripts"

# Define a regex pattern to match filenames starting with the desired prefixes
pattern <- "^(00_|0[1-9][a-z]?_|1[0-3][a-z]?_).*\\.R$"

# Get all matching scripts in the directory
scripts <- list.files(scripts_dir, pattern = pattern, full.names = TRUE)

# Sort the files to ensure they are executed in the correct order
scripts <- sort(scripts)

# Source each script
for (script in scripts) {
  cat("Running script:", script, "\n")
  source(script, echo = TRUE)
}