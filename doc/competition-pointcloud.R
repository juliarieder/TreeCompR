## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE, eval=FALSE--------------------------
#  library(TreeCompR)
#  
#  CI <- compete_pc(forest_source = "../tests/testthat/testdata/neighborhood.txt",
#                  tree_source = "../tests/testthat/testdata/tree.txt",
#                  comp_method = "both", # calculate CI in cylinder and cone
#                  center_position = "crown_pos", #center for cone and cylinder
#                  cyl_r = 4, #radius in m
#                  h_cone = 0.6, #set height where the cone starts
#                  #to open in relation to tree height (0.6 = 60 % of tree height)
#                  print_progress = "some" #get some information during processing
#                  )

## ----neighbour, warning=FALSE, eval=FALSE-------------------------------------
#  neighbours <- read_pc("../tests/testthat/testdata/neighborhood.txt")
#  tree <- read_pc("../tests/testthat/testdata/tree.txt")
#  
#  CI <- compete_pc(forest_source = neighbours,
#                  tree_source = tree,
#                  comp_method = "both", # calculate CI in cylinder and cone
#                  center_position = "crown_pos", #center for cone and cylinder
#                  cyl_r = 4, #radius in m
#                  h_cone = 0.6, #set height where the cone starts
#                  #to open in relation to tree height (0.6 = 60 % of tree height)
#                  print_progress = "full" #get full information during processing
#                  )

## ----lut, echo=FALSE, message=FALSE, warning=FALSE----------------------------
# Define the base paths
neighborhood_path <- "/path/to/neighborhood/"
trees_path <- "/path/to/trees/"

# Create example TreeIDs
TreeIDs <- paste0("Tree_", 1:5)

# Create example file paths
neighborhood_files <- paste0(neighborhood_path, "neighborhood_", 1:5, ".las")
tree_files <- paste0(trees_path, "tree_", 1:5, ".las")

# Create the lookup table as a data frame
lookup_table <- data.frame(
  TreeID = TreeIDs,
  forest_source = neighborhood_files,
  tree_source = tree_files,
  stringsAsFactors = FALSE
)



## ----lookup, echo = TRUE, eval=FALSE------------------------------------------
#  
#  # Define the base paths
#  neighborhood_path <- "/path/to/neighborhood/"
#  trees_path <- "/path/to/trees/"
#  
#  # List files in the neighborhood and tree folders
#  neighborhood_files <- list.files(neighborhood_path, full.names = TRUE, pattern = "\\.las$")
#  tree_files <- list.files(trees_path, full.names = TRUE, pattern = "\\.las$")
#  
#  # Create TreeIDs if needed
#  # Assuming the same number of neighborhood and tree files, and each tree has a corresponding neighborhood file
#  TreeIDs <- paste0("Tree_", seq_along(neighborhood_files))
#  
#  # Create the lookup table as a data frame
#  lookup_table <- data.frame(
#    TreeID = TreeIDs,
#    forest_source = neighborhood_files,
#    tree_source = tree_files,
#    stringsAsFactors = FALSE
#  )

## ----print lut----------------------------------------------------------------
# Print the lookup table
print(lookup_table)

## ----batch, echo = TRUE, eval=FALSE-------------------------------------------
#  #calculate both methods at the same time
#  library(dplyr)
#  lookup <- lookup_table %>% dplyr::select(forest_source, tree_source)
#  #define parameter settings within the lookup table
#  lookup_cone50_cyl5 <- lookup %>% dplyr::mutate(comp_method = "both", center_position = "crown_pos", cyl_r = 5, h_cone = 0.5, z_min = 100, h_xy = 0.3, print_progress = "some")
#  #use pmap (alternative to lapply) to loop over your table
#  lookup_results_cone50_cyl5 <- lookup_cone50_cyl5 %>% purrr::pmap(compete_pc)
#  
#  #re-structure results from list to dataframe
#  results <- as.data.frame(do.call(rbind, lookup_results_cone50_cyl5))

