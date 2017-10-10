# Save Neural Network model to be loaded at the start

library(rlist) # write list as file

# compute models
nn_Offense <- .selectedModel("PTS") # from neural_networks.R
nn_Defense <- .selectedModel("PTSA") # from neural_networks.R
# save models
list.save(nn_Offense, "data/nn_Offense.rds")
list.save(nn_Defense, "data/nn_Defense.rds")

################
# Write MxNet Neural network

library(rlist) # write list as file

# compute models
nn_Offense <- .selectedModel_MxNet("PTS") # from neural_networks.R
nn_Defense <- .selectedModel_MxNet("PTSA") # from neural_networks.R
# save models
list.save(nn_Offense, "data/nn_Offense.rds")
list.save(nn_Defense, "data/nn_Defense.rds")
