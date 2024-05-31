# install_packages.R

# Set CRAN mirror to São Paulo
options(repos = c(CRAN = "https://cran.fiocruz.br"))

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
  "rlang",
  "reshape2",
  "dlm",
  "here",
  "optparse",
  "padr",
  "lubridate",
  "zoo"
)

# Loop through the list of packages and install them
for (pkg in packages) {
  install_and_load(pkg)
}

cat("All specified packages have been installed and loaded successfully.\n")
