# This function will create the buttons for the datatable, they will be unique
# Used on datatables to ID shiny inputs inserted on cells
shinyInput <- function(FUN, len, id, ...) {inputs <- character(len)
for (i in seq_len(len)) {
  inputs[i] <- as.character(FUN(paste0(id, i), ...))}
inputs
}

# vector form of the above function. For cases where we want to use links on table values (labels)
shinyInput_vector <- function(FUN, len, id, label, ...) {
  
  inputs <- character(len)
  
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), label[i], ...))}
  
  inputs
  
}

# Same as above but generating a random id
shinyInput_random <- function(FUN, len, id, ...) {
  
  random_set <- ceiling(runif(len,1,100))
  inputs <- character(len)
  
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, random_set[i]), ...))}
  
  inputs
  
}

