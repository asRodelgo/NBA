# -------------------------------------
# Server for the NBA simulation app
#
# By: alberto.sanchez.rodelgo@gmail.com
# -------------------------------------

source("global_utils.R", local = TRUE) #global functions available for the whole session

function(input, output, session) {
  
  # Source all files from server_files directory and subdirectories
  files <- list.files("server_files", full.names = TRUE, recursive = TRUE)
  for (f in files) source(f, local = TRUE)
  
}