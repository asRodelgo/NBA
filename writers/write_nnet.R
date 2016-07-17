# Save Neural Network model to be loaded at the start

library(rlist) # write list as file

# compute models
nn_Offense <- .selectedModel("PTS")
nn_Defense <- .selectedModel("PTSA")
# save models
list.save(nn_Offense, "data/nn_Offense.rds")
list.save(nn_Defense, "data/nn_Defense.rds")

