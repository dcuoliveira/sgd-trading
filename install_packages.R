# install_packages.R

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Function to install and load a package
install_and_load <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

# Install packages from CRAN
packages <- c(
  "dplyr",
  "tidyr",
  "data.table",
  "roll",
  "rlang",
  "reshape2",
  "dlm",
  "bvartools",
  "here",
  "optparse",
  "ggplot2",
  "padr",
  "lubridate",
  "zoo"
)

# Loop through the list of packages and install them
for (pkg in packages) {
  install_and_load(pkg)
}

# # Install rollRegres from GitHub using remotes
# if (!requireNamespace("remotes", quietly = TRUE)) {
#   install.packages("remotes", dependencies = TRUE)
# }
# remotes::install_github("sfirke/rollRegres")

# Load all the packages to verify installation
library(dplyr)
library(tidyr)
library(data.table)
library(roll)
library(rlang)
library(reshape2)
library(dlm)
library(bvartools)
library(here)
library(parallel)
library(optparse)
library(ggplot2)
# library(rollRegres)
library(padr)
library(lubridate)
library(zoo)

cat("All specified packages have been installed and loaded successfully.\n")
