# get dependencies for current directory
library(renv)

project_path = file.path(getwd(), 'renv.lock')
renv::snapshot(lockfile = project_path, type = 'all', prompt = FALSE)

# unload renv
# detach("package:renv", unload=TRUE)

# Restore env
current_dir <- strsplit(getwd(), "/")[[1]] |> tail(1)
if(current_dir == "CommunityFeedbackApp"){
  renv::restore(lockfile = 'renv.lock')
}

