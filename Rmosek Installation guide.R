##### Rmosek installation: ####
# Make sure you are using the latest version of R, Rtools and Rstudio

# Visit the Mosek website and follow the instructions to download the rmosek for your OS
## https://docs.mosek.com/10.2/install/installation.html#general-setup

#Visit this website: https://docs.mosek.com/latest/rmosek/install-interface.html
# Download Rtools and install it: https://cran.r-project.org/bin/windows/Rtools/rtools44/rtools.html

# Rtools installation (Follow instructions here: https://cran.r-project.org/bin/windows/Rtools/)

## Verify Existing PATH (This is for Windos, but the same applies to other OS)

# Press Windows + R, type sysdm.cpl, and press Enter.
# Navigate to Advanced > Environment Variables.
# Look for the PATH variable in the "System variables" section.
# Confirm the following paths are listed:
# Rtools: C:\rtools44\usr\bin
# MOSEK: C:\Program Files\Mosek\10.2\tools\platform\win64x86\bin (or your MOSEK installation directory).

#If any are missing:
# Select the PATH variable and click Edit.
# Click New and add the required paths.
# Click OK to save the changes.


# Rmosek installation (Follow instructions available at: https://docs.mosek.com/latest/rmosek/install-interface.html)
# setwd("C:/Program Files/Mosek/10.2/tools/platform/win64x86/rmosek") # Set directory where Rmosek builder is located after downloading for the first time
source("<RMOSEKDIR>/builder.R") # This can be use to install again
attachbuilder()
install.rmosek()


# Test installation
library("Rmosek")
#install.packages("Matrix")
library(Matrix)

# You have to request a mosek license to test the installation
# Request it here: https://www.mosek.com/products/academic-licenses/
# You will receive an email and then be able to download: mosek.lic 
# Make sure to save her C:\Users\YOUR_USER_NAME\mosek\mosek.lic (Windows)
# Where YOUR_USER_NAME is your user ID on the computer.
# You might need to reset R for the license to activate
# For problems with license please revise: https://docs.mosek.com/latest/licensing/client-setup.html#with-a-local-license-file


# Custom Function to test installation

check_rmosek_installation <- function(script_path) {
  # Ensure Rmosek package is installed
  if (!requireNamespace("Rmosek", quietly = TRUE)) {
    message("Rmosek is not installed. Please install it using install.packages('Rmosek').")
    return(FALSE)
  }
  
  # Check if the specified script exists
  if (!file.exists(script_path)) {
    message("The specified script file does not exist: ", script_path)
    return(FALSE)
  }
  
  # Try running the script
  tryCatch({
    source(script_path)
    message("Rmosek is installed and the script ran successfully.")
    return(TRUE)
  }, error = function(e) {
    message("An error occurred while running the script: ", e$message)
    return(FALSE)
  })
}

# Example usage:
script_path <- "C:/Program Files/Mosek/10.2/tools/examples/rmosek/lo1.R"
check_rmosek_installation(script_path)
