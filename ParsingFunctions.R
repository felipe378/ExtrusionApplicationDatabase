#'Updates:
#'I have fixed getAttribtue to include annealing section
#'
#'I will look to continue with the redline edits, going to temeprature zones, special equipment,
#'and checking dimension units (single out mm PPS and check length units)



#This file contains the functions necessary to PPS object and parse the text.


singleExtrusionParameters <- function(PPS, attribute_names){
  #this will parse a single extrusion PPS to get all the relevent information
  #it will create an environment to store the PPS sections that will be called by the parsing 
  #functions specific to an attribute
  
  section_list <- splitPPS(PPS)
  se.e$section_list <- section_list
  se.e$tooling_section <- section_list$'tooling section'
  se.e$parameter_section <- section_list$'parameter section'
  se.e$attribute_section <- section_list$'attribute section'
  se.e$pre_anneal_section <- section_list$'pre anneal section'
  se.e$post_anneal_section <- section_list$'post anneal section'
  se.e$note_section <- section_list$'note section'
  
  #these parameters list whether the section has already been used
  #This will prevent getAttribtue from entering an inifnite recurvsive loop
  se.e$used_post_anneal_section <- FALSE
  se.e$used_pre_anneal_section <- FALSE
  se.e$used_attribute_section <- FALSE
  
  column_length <- ncol(PPS)
  row_length <- nrow(PPS)
  
  
  ## Tooling names ##
  die_names <- c("die")
  dieland_names <- c("die land")
  tip_names <- c("mandrel", "tip")
  tipland_names <- c("mandrel land", "mandrel land length", "tip/mandrel land", "mandrel/tip land",
                     "tip land")
  
  ## Temperature names ##
  feedthroat_names <- c("feed", "feedthroat", "circulator")
  barrel_names <- c("barrel", "zone")
  clamp_names <- c("clamp")
  adapter_names <- c("adapter")
  
  
  ## Attribute names ##
  od_names <- c("Finished OD", "OD", "outer diameter")
  id_names <- c("Finished ID", "ID", "inner diameter")
  wall_names <- c("average wall", "wall", "wall thickness")
  oor_names <- c("OOR", "out of roundness", "out-of-roundness", "out of round", "ovality", 
                 "roundness")
  concentricity_names <- c("concentricity", "wall uniformity")
  length_names <- c("length", "spool")
  perpendicularity_names <- c("perpendicularity")
  
  output <- rep(list(-1000), length(attribute_names)) #will store the outputs into a list
  
  names(output) <- attribute_names
  
  
  ###output[["Extrusion Type"]] <- getExtrusionType(PPS)
  
  {#get tooling
    output[["Die Size (in)"]] <- getTooling(section_list$'tooling section', die_names)
    output[["Die Land Length (in)"]] <- getTooling(section_list$'tooling section', dieland_names)
    output[["Tip Size (in)"]] <- getTooling(section_list$'tooling section', tip_names)
    output[["Tip Land Length (in)"]] <- getTooling(section_list$'tooling section', tipland_names)
  } #end get Tooling
  
  {#get screw
    output[["Screw Print"]] <- getScrew(section_list$'tooling section')
  }
  
  
  {#get FCA
    output[["Feedthroat Temperature (F)"]] <- getFCATemperature(section_list$'parameter section', feedthroat_names)
    output[["Clamp Temperature (F)"]] <- getFCATemperature(section_list$'parameter section', clamp_names)
    output[["Adapter Temperature (F)"]] <- getFCATemperature(section_list$'parameter section', adapter_names)  
  }#end get FCA
  {#get barrel and die temperatures
    barrel_temperatures <- getBarrelTemperatures(section_list$'parameter section', barrel_names)
    
    output[["Barrel Zone 1 Temperature (F)"]] <- barrel_temperatures[1]
    output[["Barrel Zone 2 Temperature (F)"]] <- barrel_temperatures[2]
    output[["Barrel Zone 3 Temperature (F)"]] <- barrel_temperatures[3]
    
    die_temperatures <- getDieTemperatures(section_list$'parameter section', die_names)
    
    output[["Die 1 Temperature (F)"]] <- die_temperatures[1]
    output[["Die 2 Temperature (F)"]] <- die_temperatures[2]
  }#end barrel and die temperatures
  {#get attributes
    if (se.e$use_postanneal == TRUE){
      #if there is a post anneal section
      output[["Inner Diameter (in)"]] <- getAttribute(section_list$'post anneal section', id_names, "Target")
      output[["Outer Diameter (in)"]] <- getAttribute(section_list$'post anneal section', od_names, "Target")
      output[["Wall Thickness (in)"]] <- getAttribute(section_list$'post anneal section', wall_names, "Target")
      output[["Out-of-Roundness (in)"]] <- getAttribute(section_list$'post anneal section', oor_names, "USL")
      output[["Concentricity (in)"]] <- getAttribute(section_list$'post anneal section', concentricity_names, "USL")
      output[["Length (in)"]] <- getAttribute(section_list$'post anneal section', length_names, "Target")
      output[["Perpendicularity (in)"]] <- getAttribute(section_list$'post anneal section', perpendicularity_names, "USL")
    }
    else if (se.e$used_pre_anneal_section == TRUE){
      #if there is no post anneal section, check pre anneal
      se.e$used_pre_anneal_section <- TRUE
      output[["Inner Diameter (in)"]] <- getAttribute(section_list$'pre anneal section', id_names, "Target")
      output[["Outer Diameter (in)"]] <- getAttribute(section_list$'pre anneal section', od_names, "Target")
      output[["Wall Thickness (in)"]] <- getAttribute(section_list$'pre anneal section', wall_names, "Target")
      output[["Out-of-Roundness (in)"]] <- getAttribute(section_list$'pre anneal section', oor_names, "USL")
      output[["Concentricity (in)"]] <- getAttribute(section_list$'pre anneal section', concentricity_names, "USL")
      output[["Length (in)"]] <- getAttribute(section_list$'pre anneal section', length_names, "Target")
      output[["Perpendicularity (in)"]] <- getAttribute(section_list$'pre anneal section', perpendicularity_names, "USL")
    }
    else{
      #use the attribute section with nothing else
      se.e$used_attribute_section <- TRUE
      output[["Inner Diameter (in)"]] <- getAttribute(section_list$'attribute section', id_names, "Target")
      output[["Outer Diameter (in)"]] <- getAttribute(section_list$'attribute section', od_names, "Target")
      output[["Wall Thickness (in)"]] <- getAttribute(section_list$'attribute section', wall_names, "Target")
      output[["Out-of-Roundness (in)"]] <- getAttribute(section_list$'attribute section', oor_names, "USL")
      output[["Concentricity (in)"]] <- getAttribute(section_list$'attribute section', concentricity_names, "USL")
      output[["Length (in)"]] <- getAttribute(section_list$'attribute section', length_names, "Target")
      output[["Perpendicularity (in)"]] <- getAttribute(section_list$'attribute section', perpendicularity_names, "USL")
    }
    
    
  }#end attributes
  
  return(output) #returns the list of outputs
  
} #end singleExtrusionParameters

taperedExtrusionParameters <- function(PPS, attribute_names){
  #this will get the parameters for tapered extrusion in a similar format to single extrusion
  
  column_length <- ncol(PPS)
  row_length <- nrow(PPS)
  
  section_list <- splitPPS(PPS)
  first_section <- section_list$'first section'
  second_section <- section_list$'second section'
  third_section <- section_list$'third section'
  
  
  #### Names to Search through the PPS ####
  
  ## Tooling names ##
  die_names <- c("die")
  dieland_names <- c("die land", "die land length")
  tip_names <- c("mandrel", "tip", "tip/mandrel", "mandrel/tip")
  tipland_names <- c("mandrel land", "mandrel land length", "tip/mandrel land", 
                     "tip/mandrel land lengths", "mandrel/tip land", "mandrel/tip land lengths",
                     "tip land", "tip land length")
  
  
  ## Temperature names ##
  feedthroat_names <- c("feed", "feedthroat", "circulator")
  barrel_names <- c("barrel", "zone")
  clamp_names <- c("clamp")
  adapter_names <- c("adapter")
  die_names <- c("die")
  
  
  ## Attribute names ##
  prox_od_names <- c("Proximal Finished OD", "Proximal OD", "Proximal outer diameter",
                     "Prox Finished OD", "Prox OD", "Prox outer diameter",
                     "Finished OD, proximal", "Proximal OD, proximal", "Proximal outer diameter, proximal",
                     "Finished OD, prox", "Proximal OD, prox", "Proximal outer diameter, prox",
                     "OD @ A")
  prox_id_names <- c("Proximal Finished ID", "Proximal ID", "Proximal inner diameter",
                     "Prox Finished ID", "Prox ID", "Prox inner diameter",
                     "Finished ID, proximal", "ID, proximal", "inner diameter, proximal",
                     "Finished ID, prox", "ID, prox", "inner diameter, prox",
                     "ID @ A")
  prox_wall_names <- c("Proximal average wall", "Proximal wall", "Proximal wall thickness",
                       "Prox average wall", "Prox wall", "Prox wall thickness",
                       "average wall, proximal", "wall, proximal", "wall thickness, proximal",
                       "average wall, prox", "wall, prox", "wall thickness, prox")
  prox_oor_names <- c("Proximal OOR", "Proximal out of roundness", "Proximal out-of-roundness", 
                      "Proximal out of round", "Proximal ovality", "Proximal Wall Uniformity", 
                      "Proximal roundness",
                      "Prox OOR", "Prox out of roundness", "Prox out-of-roundness", 
                      "Prox out of round", "Prox ovality", "Prox Wall Uniformity", 
                      "Prox roundness",
                      "OOR, proximal", "out of roundness, proximal", "out-of-roundness, proximal", 
                      "out of round, proximal", "ovality, proximal", "Wall Uniformity, proximal", 
                      "roundness, proximal",
                      "OOR, prox", "out of roundness, prox", "out-of-roundness, prox", 
                      "out of round, prox", "ovality, prox", "Wall Uniformity, prox", 
                      "roundness, prox")
  prox_concentricity_names <- c("Proximal concentricity",
                                "Prox concentricity",
                                "concentricity, proximal",
                                "concentricity, prox")
  prox_length_names <- c("Proximal length",
                         "Prox length",
                         "length, proximal",
                         "length, prox",
                         "A to B Distance")
  prox_perpendicularity_names <- c("Proximal perpendicularity",
                                   "Prox perpendicularity",
                                   "perpendicularity, proximal",
                                   "perpendicularity, prox")
  
  dist_od_names <- c("Distal Finished OD", "Distal OD", "Distal outer diameter",
                     "Dist Finished OD", "Dist OD", "Dist outer diameter",
                     "Finished OD, distal", "OD, distal", "outer diameter, distal",
                     "Finished OD, dist", "OD, dist", "outer diameter, dist",
                     "OD @ D")
  dist_id_names <- c("Distal Finished ID", "Distal ID", "Distal inner diameter",
                     "Dist Finished ID", "Dist ID", "Dist inner diameter",
                     "Finished ID, distal", "ID, distal", "inner diameter, distal",
                     "Finished ID, dist", "ID, dist", "inner diameter, dist",
                     "ID @ D")
  dist_wall_names <- c("Distal average wall", "Distal wall", "Distal wall thickness",
                       "Dist average wall", "Dist wall", "Dist wall thickness",
                       "average wall, distal", "wall, distal", "wall thickness, distal",
                       "average wall, dist", "wall, dist", "wall thickness, dist")
  dist_oor_names <- c("Distal OOR", "Distal out of roundness", "Distal out-of-roundness", 
                      "Distal out of round", "Distal vality", "Distal Wall Uniformity", 
                      "Distal roundness",
                      "Dist OOR", "Dist out of roundness", "Dist out-of-roundness", 
                      "Dist out of round", "Dist ovality", "Dist Wall Uniformity", 
                      "Dist roundness",
                      "OOR, distal", "out of roundnes, distals", "out-of-roundness, distal", 
                      "out of round, distal", "ovality, distal", "Wall Uniformity, distal", 
                      "roundness, distal",
                      "OOR, dist", "out of roundness, dist", "out-of-roundness, dist", 
                      "out of round, dist", "ovality, dist", "Wall Uniformity, dist", 
                      "roundness, dist")
  dist_concentricity_names <- c("Distal concentricity",
                                "Dist concentricity",
                                "concentricity, distal",
                                "concentricity, dist")
  dist_length_names <- c("Distal length",
                         "Dist length",
                         "length, distal",
                         "length, dist")
  dist_perpendicularity_names <- c("Distal perpendicularity",
                                   "Dist perpendicularity",
                                   "perpendicularity, distal",
                                   "perpendicularity, dist")
  
  proxtransition_length_names <- c("Proximal + Transition Length", "A to C Distance",
                                   "Cut \"Y\" max","Cut  \"Y\"  max")
  
  transition_length_names <- c("Transition Length", "Taper Length", "Length, Transition", 
                               "Length, Taper", "Cut \"Z\" ref","Cut  \"Z\"  ref")
  
  length_names <- c("length", "total length", "overall length")
  
  
  #### Naming the Outputs ####
  
  output <- rep(list(-1000), length(attribute_names)) #will store the outputs into a list
  
  names(output) <- attribute_names
  
  ###output[["Extrusion Type"]] <- getExtrusionType(PPS)
  
  {#get tooling
    output[["Die Size (in)"]] <- getTooling(first_section, die_names)
    output[["Die Land Length (in)"]] <- getTooling(first_section, dieland_names)
    output[["Tip Size (in)"]] <- getTooling(first_section, tip_names)
    output[["Tip Land Length (in)"]] <- getTooling(first_section, tipland_names)
  } #end get Tooling
  
  {#get screw
    output[["Screw Print"]] <- getScrew(first_section)
  }
  {#get FCA
    output[["Feedthroat Temperature (F)"]] <- getFCATemperature(second_section, feedthroat_names)
    output[["Clamp Temperature (F)"]] <- getFCATemperature(second_section, clamp_names)
    output[["Adapter Temperature (F)"]] <- getFCATemperature(second_section, adapter_names)  
  }#end get FCA
  {#get barrel and die temperatures
    barrel_temperatures <- getBarrelTemperatures(second_section, barrel_names)
    
    output[["Barrel Zone 1 Temperature (F)"]] <- barrel_temperatures[1]
    output[["Barrel Zone 2 Temperature (F)"]] <- barrel_temperatures[2]
    output[["Barrel Zone 3 Temperature (F)"]] <- barrel_temperatures[3]
    
    die_temperatures <- getDieTemperatures(second_section, die_names)
    
    output[["Die 1 Temperature (F)"]] <- die_temperatures[1]
    output[["Die 2 Temperature (F)"]] <- die_temperatures[2]
  }#end barrel and die temperatures
  {#get attributes
    #prox
    output[["Proximal Inner Diameter (in)"]] <- getAttribute(third_section, prox_id_names, "Target")
    output[["Proximal Outer Diameter (in)"]] <- getAttribute(third_section, prox_od_names, "Target")
    output[["Proximal Wall Thickness (in)"]] <- getAttribute(third_section, prox_wall_names, "Target")
    output[["Proximal Out-of-Roundness (in)"]] <- getAttribute(third_section, prox_oor_names, "USL")
    output[["Proximal Concentricity (in)"]] <- getAttribute(third_section, prox_concentricity_names, "USL")
    output[["Proximal Perpendicularity (in)"]] <- getAttribute(third_section, prox_perpendicularity_names, "USL")
    
    #dist
    output[["Distal Inner Diameter (in)"]] <- getAttribute(third_section, dist_id_names, "Target")
    output[["Distal Outer Diameter (in)"]] <- getAttribute(third_section, dist_od_names, "Target")
    output[["Distal Wall Thickness (in)"]] <- getAttribute(third_section, dist_wall_names, "Target")
    output[["Distal Out-of-Roundness (in)"]] <- getAttribute(third_section, dist_oor_names, "USL")
    output[["Distal Concentricity (in)"]] <- getAttribute(third_section, dist_concentricity_names, "USL")
    output[["Distal Perpendicularity (in)"]] <- getAttribute(third_section, dist_perpendicularity_names, "USL")
    
    #lengths
    output[["Total Length (in)"]] <- getAttribute(third_section, length_names, "Target")
    output[["Proximal Length (in)"]] <- getAttribute(third_section, prox_length_names, "Target")
    output[["Distal Length (in)"]] <- getAttribute(third_section, dist_length_names, "Target")
    output[["Transition Length (in)"]] <- getAttribute(third_section, transition_length_names, "Target")
    output[["Proximal and Transition Length (in)"]] <- getAttribute(third_section, proxtransition_length_names, "Target")
    
  }#end attributes
  
  return(output) #returns the list of outputs
  
} #end taperedExtrusionParameters

multiExtrusionParameters <- function(PPS, attribute_names){
  
  column_length <- ncol(PPS)
  row_length <- nrow(PPS)
  
  section_list <- splitPPS(PPS)
  first_section <- section_list$'first section'
  second_section <- section_list$'second section'
  third_section <- section_list$'third section'
  
  ## Temperature names ##
  feedthroat_names <- c("feed", "feedthroat", "circulator")
  barrel_names <- c("barrel", "zone")
  clamp_names <- c("clamp")
  adapter_names <- c("adapter")
  die_names <- c("die")
  
  
  ## Attribute names ##
  od_names <- c("Finished OD", "OD", "outer diameter")
  id_names <- c("Finished ID", "ID", "inner diameter")
  overall_wall_names <- c("total wall", "total wall thickness", "overall wall",
                          "overall wall thickness")
  inner_wall_names <- c("inner wall", "inner layer wall", "inner wall thickness", 
                        "inner layer wall thickness")
  middle_wall_names <- c("middle wall", "middle layer wall", "middle wall thickness", 
                         "middle layer wall thickness")
  outer_wall_names <- c("outer wall", "outer layer wall", "outer wall thickness", 
                        "outer layer wall thickness")
  oor_names <- c("OOR", "out of roundness", "out-of-roundness", "out of round", "ovality", 
                 "Wall Uniformity", "roundness")
  concentricity_names <- c("concentricity")
  length_names <- c("length")
  perpendicularity_names <- c("perpendicularity")    
  
  
  
  output <- rep(list(-1000), length(attribute_names)) #will store the outputs into a list
  
  names(output) <- attribute_names
  
  
  output[["Extrusion Type"]] <- getExtrusionType(PPS)
  
  {#get FCA
    output[["Feedthroat Temperature (F)"]] <- getFCATemperature(second_section, feedthroat_names)
    output[["Clamp Temperature (F)"]] <- getFCATemperature(second_section, clamp_names)
    output[["Adapter Temperature (F)"]] <- getFCATemperature(second_section, adapter_names)  
  }#end get FCA
  {#get barrel and die temperatures
    barrel_temperatures <- getBarrelTemperatures(second_section, barrel_names)
    
    output[["Barrel Zone 1 Temperature (F)"]] <- barrel_temperatures[1]
    output[["Barrel Zone 2 Temperature (F)"]] <- barrel_temperatures[2]
    output[["Barrel Zone 3 Temperature (F)"]] <- barrel_temperatures[3]
    
    die_temperatures <- getDieTemperatures(second_section, die_names)
    
    output[["Die 1 Temperature (F)"]] <- die_temperatures[1]
    output[["Die 2 Temperature (F)"]] <- die_temperatures[2]
  }#end barrel and die temperatures
  {#get attributes
    #prox
    output[["Proximal Inner Diameter (in)"]] <- getAttribute(third_section, prox_id_names, "Target")
    output[["Proximal Outer Diameter (in)"]] <- getAttribute(third_section, prox_od_names, "Target")
    output[["Proximal Wall Thickness (in)"]] <- getAttribute(third_section, prox_wall_names, "Target")
    output[["Proximal Out-of-Roundness (in)"]] <- getAttribute(third_section, prox_oor_names, "USL")
    output[["Proximal Concentricity (in)"]] <- getAttribute(third_section, prox_concentricity_names, "USL")
    output[["Proximal Perpendicularity (in)"]] <- getAttribute(third_section, prox_perpendicularity_names, "USL")
    
    #dist
    output[["Distal Inner Diameter (in)"]] <- getAttribute(third_section, dist_id_names, "Target")
    output[["Distal Outer Diameter (in)"]] <- getAttribute(third_section, dist_od_names, "Target")
    output[["Distal Wall Thickness (in)"]] <- getAttribute(third_section, dist_wall_names, "Target")
    output[["Distal Out-of-Roundness (in)"]] <- getAttribute(third_section, dist_oor_names, "USL")
    output[["Distal Concentricity (in)"]] <- getAttribute(third_section, dist_concentricity_names, "USL")
    output[["Distal Perpendicularity (in)"]] <- getAttribute(third_section, dist_perpendicularity_names, "USL")
    
    #lengths
    output[["Total Length (in)"]] <- getAttribute(third_section, length_names, "Target")
    output[["Proximal Length (in)"]] <- getAttribute(third_section, prox_length_names, "Target")
    output[["Distal Length (in)"]] <- getAttribute(third_section, dist_length_names, "Target")
    output[["Transition Length (in)"]] <- getAttribute(third_section, transition_length_names, "Target")
    output[["Proximal and Transition Length (in)"]] <- getAttribute(third_section, proxtransition_length_names, "Target")
    
  }#end attributes
  
} #end multiExtrusionParameters

multiAndTaperedExtrusionParameters <- function(PPS, attribute_names){
  column_length <- ncol(PPS)
  row_length <- nrow(PPS)
  
  section_list <- splitPPS(PPS)
  first_section <- section_list$'first section'
  second_section <- section_list$'second section'
  third_section <- section_list$'third section'
  
  ## Temperature names ##
  feedthroat_names <- c("feed", "feedthroat", "circulator")
  barrel_names <- c("barrel", "zone")
  clamp_names <- c("clamp")
  adapter_names <- c("adapter")
  die_names <- c("die")
  
  
  #### Attribute names ####
  prox_od_names <- c("Proximal Finished OD", "Proximal OD", "Proximal outer diameter",
                     "Prox Finished OD", "Prox OD", "Prox outer diameter",
                     "Finished OD, proximal", "Proximal OD, proximal", "Proximal outer diameter, proximal",
                     "Finished OD, prox", "Proximal OD, prox", "Proximal outer diameter, prox",
                     "OD @ A")
  prox_id_names <- c("Proximal Finished ID", "Proximal ID", "Proximal inner diameter",
                     "Prox Finished ID", "Prox ID", "Prox inner diameter",
                     "Finished ID, proximal", "ID, proximal", "inner diameter, proximal",
                     "Finished ID, prox", "ID, prox", "inner diameter, prox",
                     "ID @ A")
  prox_wall_names <- c("Proximal average wall", "Proximal wall", "Proximal wall thickness",
                       "Prox average wall", "Prox wall", "Prox wall thickness",
                       "average wall, proximal", "wall, proximal", "wall thickness, proximal",
                       "average wall, prox", "wall, prox", "wall thickness, prox")
  prox_oor_names <- c("Proximal OOR", "Proximal out of roundness", "Proximal out-of-roundness", 
                      "Proximal out of round", "Proximal ovality", "Proximal Wall Uniformity", 
                      "Proximal roundness",
                      "Prox OOR", "Prox out of roundness", "Prox out-of-roundness", 
                      "Prox out of round", "Prox ovality", "Prox Wall Uniformity", 
                      "Prox roundness",
                      "OOR, proximal", "out of roundness, proximal", "out-of-roundness, proximal", 
                      "out of round, proximal", "ovality, proximal", "Wall Uniformity, proximal", 
                      "roundness, proximal",
                      "OOR, prox", "out of roundness, prox", "out-of-roundness, prox", 
                      "out of round, prox", "ovality, prox", "Wall Uniformity, prox", 
                      "roundness, prox")
  prox_concentricity_names <- c("Proximal concentricity",
                                "Prox concentricity",
                                "concentricity, proximal",
                                "concentricity, prox")
  prox_length_names <- c("Proximal length",
                         "Prox length",
                         "length, proximal",
                         "length, prox",
                         "A to B Distance")
  prox_perpendicularity_names <- c("Proximal perpendicularity",
                                   "Prox perpendicularity",
                                   "perpendicularity, proximal",
                                   "perpendicularity, prox")
  
  dist_od_names <- c("Distal Finished OD", "Distal OD", "Distal outer diameter",
                     "Dist Finished OD", "Dist OD", "Dist outer diameter",
                     "Finished OD, distal", "OD, distal", "outer diameter, distal",
                     "Finished OD, dist", "OD, dist", "outer diameter, dist",
                     "OD @ D")
  dist_id_names <- c("Distal Finished ID", "Distal ID", "Distal inner diameter",
                     "Dist Finished ID", "Dist ID", "Dist inner diameter",
                     "Finished ID, distal", "ID, distal", "inner diameter, distal",
                     "Finished ID, dist", "ID, dist", "inner diameter, dist",
                     "ID @ D")
  dist_wall_names <- c("Distal average wall", "Distal wall", "Distal wall thickness",
                       "Dist average wall", "Dist wall", "Dist wall thickness",
                       "average wall, distal", "wall, distal", "wall thickness, distal",
                       "average wall, dist", "wall, dist", "wall thickness, dist")
  dist_oor_names <- c("Distal OOR", "Distal out of roundness", "Distal out-of-roundness", 
                      "Distal out of round", "Distal vality", "Distal Wall Uniformity", 
                      "Distal roundness",
                      "Dist OOR", "Dist out of roundness", "Dist out-of-roundness", 
                      "Dist out of round", "Dist ovality", "Dist Wall Uniformity", 
                      "Dist roundness",
                      "OOR, distal", "out of roundnes, distals", "out-of-roundness, distal", 
                      "out of round, distal", "ovality, distal", "Wall Uniformity, distal", 
                      "roundness, distal",
                      "OOR, dist", "out of roundness, dist", "out-of-roundness, dist", 
                      "out of round, dist", "ovality, dist", "Wall Uniformity, dist", 
                      "roundness, dist")
  dist_concentricity_names <- c("Distal concentricity",
                                "Dist concentricity",
                                "concentricity, distal",
                                "concentricity, dist")
  dist_length_names <- c("Distal length",
                         "Dist length",
                         "length, distal",
                         "length, dist")
  dist_perpendicularity_names <- c("Distal perpendicularity",
                                   "Dist perpendicularity",
                                   "perpendicularity, distal",
                                   "perpendicularity, dist")
  
  proxtransition_length_names <- c("Proximal + Transition Length", "A to C Distance",
                                   "Cut \"Y\" max","Cut  \"Y\"  max")
  
  transition_length_names <- c("Transition Length", "Taper Length", "Length, Transition", 
                               "Length, Taper", "Cut \"Z\" ref","Cut  \"Z\"  ref")
  
  length_names <- c("length", "total length", "overall length")
  
  
  #### Naming the data ####
  output <- rep(list(-1000), length(attribute_names)) #will store the outputs into a list
  
  names(output) <- attribute_names
  
  
  output[["Extrusion Type"]] <- getExtrusionType(PPS)
  
  {#get FCA
    output[["Feedthroat Temperature (F)"]] <- getFCATemperature(second_section, feedthroat_names)
    output[["Clamp Temperature (F)"]] <- getFCATemperature(second_section, clamp_names)
    output[["Adapter Temperature (F)"]] <- getFCATemperature(second_section, adapter_names)  
  }#end get FCA
  {#get barrel and die temperatures
    barrel_temperatures <- getBarrelTemperatures(second_section, barrel_names)
    
    output[["Barrel Zone 1 Temperature (F)"]] <- barrel_temperatures[1]
    output[["Barrel Zone 2 Temperature (F)"]] <- barrel_temperatures[2]
    output[["Barrel Zone 3 Temperature (F)"]] <- barrel_temperatures[3]
    
    die_temperatures <- getDieTemperatures(second_section, die_names)
    
    output[["Die 1 Temperature (F)"]] <- die_temperatures[1]
    output[["Die 2 Temperature (F)"]] <- die_temperatures[2]
  }#end barrel and die temperatures
  {#get attributes
    #prox
    output[["Proximal Inner Diameter (in)"]] <- getAttribute(third_section, prox_id_names, "Target")
    output[["Proximal Outer Diameter (in)"]] <- getAttribute(third_section, prox_od_names, "Target")
    output[["Proximal Wall Thickness (in)"]] <- getAttribute(third_section, prox_wall_names, "Target")
    output[["Proximal Out-of-Roundness (in)"]] <- getAttribute(third_section, prox_oor_names, "USL")
    output[["Proximal Concentricity (in)"]] <- getAttribute(third_section, prox_concentricity_names, "USL")
    output[["Proximal Perpendicularity (in)"]] <- getAttribute(third_section, prox_perpendicularity_names, "USL")
    
    #dist
    output[["Distal Inner Diameter (in)"]] <- getAttribute(third_section, dist_id_names, "Target")
    output[["Distal Outer Diameter (in)"]] <- getAttribute(third_section, dist_od_names, "Target")
    output[["Distal Wall Thickness (in)"]] <- getAttribute(third_section, dist_wall_names, "Target")
    output[["Distal Out-of-Roundness (in)"]] <- getAttribute(third_section, dist_oor_names, "USL")
    output[["Distal Concentricity (in)"]] <- getAttribute(third_section, dist_concentricity_names, "USL")
    output[["Distal Perpendicularity (in)"]] <- getAttribute(third_section, dist_perpendicularity_names, "USL")
    
    #lengths
    output[["Total Length (in)"]] <- getAttribute(third_section, length_names, "Target")
    output[["Proximal Length (in)"]] <- getAttribute(third_section, prox_length_names, "Target")
    output[["Distal Length (in)"]] <- getAttribute(third_section, dist_length_names, "Target")
    output[["Transition Length (in)"]] <- getAttribute(third_section, transition_length_names, "Target")
    output[["Proximal and Transition Length (in)"]] <- getAttribute(third_section, proxtransition_length_names, "Target")
    
  }#end attributes
} #end multiAndTaperedExtrusionParameters

splitPPS <- function(PPS){
  #this function splits the PPS into six section (some may be null if there is no note or annealing
  # sections). They are (tooling, processing parameters, product attribute, pre annealing, post
  # annealing, and notes)
  #it then returns a list with each section
  #'it searches the PPS by searching key words and titles that are unique to each section. If no
  #'match is found it uses a heuristic. annealing and note sections may be blank.
  
  
  section_list <- list()
  
  column_length <- ncol(PPS) #gets the number of columns
  row_length <- nrow(PPS) #gets the numbers of rows
  
  
  #### Tooling Section ####
    
  tooling_section_indice <- grep("Material & Tooling Requirements", PPS[,1], ignore.case = TRUE)
  
  if(length(tooling_section_indice) > 1){
    tooling_section_indice <- tooling_section_indice[1]
  }
  
  if (length(tooling_section_indice) == 0){
    # incase the tooling section is not labeled 'Material & Tooling Requirements' then I will check for 
    #the string 'PPS Barcode Verification' and then 'Equipment & Tooling Requirements'
    tooling_indice_count <- 0
    while (tooling_indice_count < column_length + 1){
      #runs through the columns of PPS to look for 'PPS Barcode Verification'
      tooling_section_indice <- grep("PPS Barcode Verification", PPS[,tooling_indice_count], ignore.case = TRUE)
      
      if(length(tooling_section_indice) > 1){
        tooling_section_indice <- tooling_section_indice[1]
        break
      }
      
      if (length(tooling_section_indice) != 0){
        break;
      }# end if
      
      tooling_section_indice <- grep("Equipment & Tooling Requirements", PPS[,tooling_indice_count], 
                                   ignore.case = TRUE)
      
      if(length(tooling_section_indice) > 1){
        tooling_section_indice <- tooling_section_indice[1]
        break
      }
      
      if (length(tooling_section_indice) != 0){
        #if a match was found
        break;
      }# end if
      
      tooling_indice_count <- tooling_indice_count + 1
    } #end while
    
    if (length(tooling_section_indice) == 0){
      # if the while loop did not match a pattern to grep
      tooling_section_indice <- 1 #assign indice of 1
    }# end if
  }# end if
  
  
  #### Parameter Section ####
  
  parameter_section_indice <- grep("Processing Parameters", PPS[,1], ignore.case = TRUE)
  
  if(length(parameter_section_indice) > 1){
    parameter_section_indice <- parameter_section_indice[1]
  }
  else if (length(parameter_section_indice) == 0){
    #this will check Process Parameters after checking for Processing Parameters
    parameter_section_indice <- grep("Process Parameters", PPS[,1], ignore.case = TRUE)
    
    if(length(parameter_section_indice) > 1){
      parameter_section_indice <- parameter_section_indice[1]
    }
    
  }
  
  if (length(parameter_section_indice) == 0){
    # incase the section is not labeled 'Processing Parameters' then I will check for a subsection
    #of the section 'Extrusion Parameters' and then 'Water' for waterbath and assign the section 
    #the indice above
    parameter_indice_count <- 0
    while (parameter_indice_count < column_length){
      #runs through the columns of PPS to look for 'Extrusion Parameters' first and then 'Water'
      
      parameter_section_indice <- grep("Processing Parameters", PPS[,parameter_indice_count], ignore.case = TRUE)
      
      if(length(parameter_section_indice) > 1){
        parameter_section_indice <- parameter_section_indice[1]
        break
      }
      
      if (length(parameter_section_indice) != 0){
        break;
      }# end if
      
      
      parameter_section_indice <- grep("Extrusion Parameter", PPS[,parameter_indice_count], ignore.case = TRUE)
      
      if(length(parameter_section_indice) > 1){
        parameter_section_indice <- parameter_section_indice[1]
        break
      }
      
      if (length(parameter_section_indice) != 0){
        break;
      }# end if
      
      parameter_section_indice <- grep("Waterbath", PPS[,parameter_indice_count], ignore.case = TRUE)
      
      if(length(parameter_section_indice) > 1){
        parameter_section_indice <- parameter_section_indice[1] - 3
        break
      }
      
      if (length(parameter_section_indice) != 0){
        parameter_section_indice <- parameter_section_indice - 3
        break;
      }# end if
      
      parameter_indice_count <- parameter_indice_count + 1
    } #end while
    
    if (length(parameter_section_indice) == 0 || parameter_section_indice < 8){
      # if the while loop did not match a pattern to grep
      # or if the parameter_section_indice found a bad match and is less than 8
      parameter_section_indice <- 8 #assign indice of 8
    }# end if
  }# end if
  
  
  
  #### Annealing Section ####
  
  #'Search the columns of the PPS to see if the part is annealed. If the word anneal is found
  #'in the PPS it will attempt to split the section based on that
  
  se.e$use_annealing <- FALSE #tells the code whether to use annealing section for checking parameters
  se.e$use_preanneal <- FALSE
  se.e$use_postanneal <- FALSE
  
  anneal_count <- 1
  anneal_found <- FALSE
  while (anneal_count < column_length + 1){
    #runs through all the column until it matchs 'anneal'
    placeholder <- grep("anneal", PPS[,anneal_count], ignore.case = TRUE)
    if (length(placeholder) != 0){
      anneal_found <- TRUE
      break
    }
    anneal_count <- anneal_count + 1
  } #end while searching for anneal
  
  se.debug.e$anneal_found <- anneal_found
  
  annealing_sections_found <- FALSE #boolean if all the annealing indices were found
  anneal_section_count <- 1 #goes through the columns to search for annealing, pre, and post anneal
  annealing_index <- -1
  pre_anneal_index <- -1
  #string to search for pre anneal section
  pre_anneal_string <- c("pre anneal", "pre-anneal", "anneal pre", "anneal-pre")
  post_anneal_index <- -1
  #string to search for post anneal section
  post_anneal_string <- c("post anneal", "post-anneal", "anneal post", "anneal-post")
  
  if (anneal_found == TRUE){
    
    while (anneal_section_count < column_length + 1){
      #runs through the columns of the PPS to search for 'annealing'
      
      #makes sure that the cell starts with 'annealing'
      placeholder <- grep("^annealing", PPS[,anneal_section_count], ignore.case = TRUE)
      
      if(length(placeholder) > 0){
        #if a match was found
        annealing_index <- placeholder[1] #in case of multiple matches
        break #break out of the while loop
      }#end if for placeholder
      
      anneal_section_count <- anneal_section_count + 1
    }#end while searching for 'annealing'
    
    anneal_section_count <- 1 #reset the count to search for pre annealing
    
    while (anneal_section_count < column_length + 1){
      #runs through the columns of the PPS to search for pre annealing
      
      string_count <- 1 #the count for the pre-annealing string
      
      while (string_count < length(pre_anneal_string) + 1){
        
        placeholder <- grep(pre_anneal_string[string_count], PPS[,anneal_section_count], 
                            ignore.case = TRUE)
        
        if(length(placeholder) > 0){
          #if a match was found
          pre_anneal_index <- placeholder[1] #in case of multiple matches
          break #break out of the while loop
        }#end if for placeholder
        
        string_count <- string_count + 1
      }#end while for pre anneal string
      
      if (pre_anneal_index != -1){
        #if pre_anneal_index was assigned in the inner while loop, break out of the loop
        break
      }
      
      anneal_section_count <- anneal_section_count + 1
    }#end while searching for pre annealing
    
    anneal_section_count <- 1 #reset the count to search for post annealing
    
    while (anneal_section_count < column_length + 1){
      #runs through the columns of the PPS to search for post annealing
      
      string_count <- 1 #the count for the post-annealing string
      
      while (string_count < length(post_anneal_string) + 1){
        
        placeholder <- grep(post_anneal_string[string_count], PPS[,anneal_section_count], 
                            ignore.case = TRUE)
        
        if(length(placeholder) > 0){
          #if a match was found
          post_anneal_index <- placeholder[1] #in case of multiple matches
          break #break out of the while loop
        }#end if for placeholder
        
        string_count <- string_count + 1
      }#end while for pre anneal string
      
      if (post_anneal_index != -1){
        #if post_anneal_index was assigned in the inner while loop, break out of the loop
        break
      }
      
      anneal_section_count <- anneal_section_count + 1
    }#end while searching for post annealing
    
    
    if (annealing_index != -1 && pre_anneal_index != -1 && post_anneal_index != -1){
      #if all the section indices have been defined
      annealing_sections_found <- TRUE
    }#end if
    
    
    #in the case that pre anneal or post anneal was not found because they were improperly labelled,
    #but annealing was found, the section will be split after finding the attribute and note section
    #it will come after the note section split
    
  }#end if anneal_found is TRUE
  
  se.debug.e$annealing_sections_found <- annealing_sections_found
  
  #### Attribute Section ####
  
  attribute_section_indice <- grep("Product Attributes & Testing", PPS[,1], ignore.case = TRUE)
  se.e$use_attribute <- FALSE #whether the attribute section should be used for attributes
  
  if(length(attribute_section_indice) > 1){
    attribute_section_indice <- attribute_section_indice[1]
  }
  
  if (length(attribute_section_indice) == 0){
    # incase the section is not labeled 'Product Testing & Attributes' then I will check for a 
    #subsection of the section 'Attribute' and assign the section the indice two above
    attribute_indice_count <- 0
    while (attribute_indice_count < column_length + 1){
      #runs through the columns of PPS to look for matches
      
      attribute_section_indice <- grep("Product Attributes & Testing", PPS[,attribute_indice_count], 
                                   ignore.case = TRUE)
      
      if(length(attribute_section_indice) > 1){
        attribute_section_indice <- attribute_section_indice[1] - 1
        break;
      }
      
      if (length(attribute_section_indice) != 0){
        attribute_section_indice <- attribute_section_indice - 1
        break;
      }# end if
      
      attribute_section_indice <- grep("Attribute", PPS[,attribute_indice_count], ignore.case = TRUE)
      
      if(length(attribute_section_indice) > 1){
        attribute_section_indice <- attribute_section_indice[1] - 1
        break;
      }
      
      if (length(attribute_section_indice) != 0){
        attribute_section_indice <- attribute_section_indice - 1
        break;
      }# end if
      attribute_indice_count <- attribute_indice_count + 1
    } #end while
    
    if (length(attribute_section_indice) == 0){
      # if the while loop did not match a pattern to grep
      attribute_section_indice <- 20 #assign indice of 20
    }# end if
  }# end if
  
  
  #### Note Section ####
  
  #'this section will look for a notes section in the PPS. This is important for finding screw
  #'prints and other information
  
  #tells the code whether to use the note section for attributes
  se.e$use_note <- FALSE

  note_indice_count <- 1
  note_section_indice <- -1
  
  while (note_indice_count < column_length + 1){
    #searches through the columns fo the PPS for the notes section header
    
    placeholder <- grep("special note", PPS[, note_indice_count], ignore.case = TRUE)
    incorrect_match <- grep("see special note", PPS[,note_indice_count], ignore.case = TRUE)
    placeholder <- setdiff(placeholder, incorrect_match) #removes incorrect matches
    
    if (length(placeholder) > 0){
      #if at least one correct match was found
      note_section_indice <- placeholder[1]
      break #break out of while loope
    }
    
    #if a match was note found for 'special note' it continue to try to find 'footnote'
    
    placeholder <- grep("footnote", PPS[, note_indice_count], ignore.case = TRUE)
    incorrect_match <- grep("see footnote", PPS[,note_indice_count], ignore.case = TRUE)
    placeholder <- setdiff(placeholder, incorrect_match) #removes incorrect matches
    
    if (length(placeholder) > 0){
      #if at least one correct match was found
      note_section_indice <- placeholder[1]
      break #break out of while loope
    }
    
    note_indice_count <- note_indice_count + 1
  }#end while searching columns for note section
  

  #### Splitting the PPS Section ####
  
  #Splits the PPS into these three sections that are present in each one.
  
  tooling_section <- PPS[tooling_section_indice:(parameter_section_indice-1),]
  
  #this will encapsulate the rest of the PPS, but will be shortened if the appropriate conditions
  #are met
  parameter_section <- PPS[parameter_section_indice:row_length,]
  
  # Initialize these to NA. They will be overwritten if the appropriate conditions are met
  attribute_section <- NA
  pre_anneal_section <- NA
  post_anneal_section <- NA
  note_section <- NA
  
  debug_condition_for_split <- 0 #list where in the if statement the PPS was split
  
  if(annealing_sections_found == FALSE && attribute_section_indice != -1){
    #there is most likely no annealing so this will be the first check
    #also checks for an attribute section index
    debug_condition_for_split <- 1
    
    parameter_section <- PPS[parameter_section_indice:(attribute_section_indice - 1),]
    
    if (note_section_indice != -1){
      #if the note section was found
      attribute_section <- PPS[attribute_section_indice:(note_section_indice - 1),]
      note_section <- PPS[note_section_indice:row_length,]
      
      se.e$use_note <- TRUE
    }
    else{
      attribute_section <- PPS[attribute_section_indice:row_length,]
    }#end if and else for note
    
    se.e$use_attribute <- TRUE
    
  } #end if that no anneal was found
  else if(annealing_sections_found == TRUE){
    #if all the annealing sections were defined
    debug_condition_for_split <- 2
    
    parameter_section <- PPS[parameter_section_indice:(pre_anneal_index - 1),]
    attribute_section <- NA
    pre_anneal_section <- PPS[pre_anneal_index:(post_anneal_index - 1),]
    
    se.e$use_preanneal <- TRUE
    se.e$use_postanneal <- TRUE
    se.e$use_annealing <- TRUE
    
    if (note_section_indice != -1){
      #if the note section was found
      post_anneal_section <- PPS[post_anneal_index:(note_section_indice - 1),]
      note_section <- PPS[note_section_indice:row_length,]
      
      se.e$use_note <- TRUE
    }
    else{
      post_anneal_section <- PPS[post_anneal_index:row_length,]
      note_section <- NA
    } #end if else for note_section
  } #end if for anneal_sections_found
  else if(pre_anneal_index == -1 && post_anneal_index != -1){
    #no pre anneal index but there is a post anneal index
    debug_condition_for_split <- 3
    
    if (attribute_section_indice != -1){
      #if an attribute section was found (which it should because it will be initialized to 20)
      parameter_section <- PPS[parameter_section_indice:(attribute_section_indice - 1),]
      attribute_section <- NA
      pre_anneal_section <- PPS[attribute_section_indice:(post_anneal_index - 1),]
      
      se.e$use_preanneal <- TRUE
      se.e$use_postanneal <- TRUE
      se.e$use_annealing <- TRUE
    }
    
    if (note_section_indice != -1){
      #if the note section was found
      post_anneal_section <- PPS[post_anneal_index:(note_section_indice - 1),]
      note_section <- PPS[note_section_indice:row_length,]
      
      se.e$use_note <- TRUE
    }
    else{
      post_anneal_section <- PPS[post_anneal_index:row_length,]
      note_section <- NA
    } #end if else for note_section
  }#end else if for if no pre annealing section was found
  else if (pre_anneal_index != -1 && post_anneal_index == -1){
    #pre anneal index was found but not a post anneal index
    debug_condition_for_split <- 4
    
    parameter_section <- PPS[parameter_section_indice:(pre_anneal_index - 1),]
    
    if (attribute_section_indice > pre_anneal_index){
      #if the attribute section indice comes after the pre anneal (suggesting it is post anneal)
      
      attribute_section <- NA
      pre_anneal_section <- PPS[pre_anneal_index:(attribute_section_indice - 1),]
      
      se.e$use_preanneal <- TRUE
      se.e$use_postanneal <- TRUE
      se.e$use_annealing <- TRUE
      
      if (note_section_indice != -1){
        #if the note section was found
        post_anneal_section <- PPS[attribute_section_indice:(note_section_indice - 1),]
        note_section <- PPS[note_section_indice:row_length,]
        
        se.e$use_note <- TRUE
      }
      else{
        post_anneal_section <- PPS[attribute_section_indice:row_length,]
        note_section <- NA
      } #end if else for note_section
    }
    else{
      #if the attribute_section_indice is equal to pre_anneal_index or comes before
      
      attribute_section <- NA
      if (note_section_indice != -1){
        #if the note section was found
        pre_anneal_section <- PPS[pre_anneal_index:(note_section_indice - 1),]
        se.e$use_preanneal <- TRUE
        note_section <- PPS[note_section_indice:row_length,]
        
        se.e$use_note <- TRUE
      }
      else{
        pre_anneal_section <- PPS[pre_anneal_index:row_length,]
        se.e$use_preanneal <- TRUE
        note_section <- NA
      } #end if else for note_section
    }#end if and else for the attribute section
  }
  else{
    #if neither of the conditions are met (their is no annealing section and attribute)
    debug_condition_for_split <- 5
    
    attribute_section <- NA
    if (note_section_indice != -1){
      #if the note section was found
      parameter_section <- PPS[parameter_section_indice:(note_section_indice - 1),]
      note_section <- PPS[note_section_indice:row_length,]
      
      se.e$use_note <- TRUE
    } #end if for note_section. If the condition is note met, parameter section remains the same
  }
  
  se.debug.e$tooling_section_index <- tooling_section_indice
  se.debug.e$parameter_section_index <- parameter_section_indice
  se.debug.e$attribute_section_index <- attribute_section_indice
  se.debug.e$pre_anneal_section_index <- pre_anneal_index
  se.debug.e$post_anneal_section_index <- post_anneal_index
  se.debug.e$note_section <- note_section_indice
  se.debug.e$use_annealing <- se.e$use_annealing
  se.debug.e$use_attribute <- se.e$use_attribute
  se.debug.e$use_note <- se.e$use_note
  se.debug.e$split_if_index <- debug_condition_for_split
  
  section_list$'tooling section' <- tooling_section
  section_list$'parameter section' <- parameter_section
  section_list$'attribute section' <- attribute_section
  section_list$'pre anneal section' <- pre_anneal_section
  section_list$'post anneal section' <- post_anneal_section
  section_list$'note section' <- note_section
  
  return(section_list)
}#end splitPPS

getTooling <- function(data, names_to_match){
  #this function will parse the PPS to get the tooling sizes
  #it can get the die, die land length, tip/mandrel, and tip/mandrel land length from
  #the first section
  data_count <- 1 #will run through the while loop but it will also be the index of the column that
  #that contains the desired element since the while loop breaks
  
  row_index <- -1 #initialize the index of the row
  column_index <- -1 #initialize the index of the column
  names_length <- length(names_to_match)
  column_length <- ncol(data)
  incorrect_names <- c("die-cut", "core mandrel", "tip retainer", "die spacer", "slicking die",
                       "sparer die", "distance die")
  
  while(data_count < column_length + 1){
    # this while loop runs through the second section and searches for the column that contains
    #either string in 'names_to_match'. If it finds the element, is saves the indices and then
    #breaks out of the while loop
    
    name_count <- 1 #runs through the vector of names_to_match
    
    while (name_count < names_length + 1){
      # this while loop checks if any of the names match anywhere in the column
      placeholder <- grep(names_to_match[name_count], data[,data_count], ignore.case = TRUE)
      
      if (length(placeholder) != 0){
        placeholder_count <- 1
        
        while (placeholder_count < length(placeholder) + 1){
          #goes through placeholder searching for a correct match for the tip
          
          
          if (nchar(data[placeholder[placeholder_count],data_count]) < 50){
            #nchar ensures a note was not selected
            row_index <- placeholder[placeholder_count]
            column_index <- data_count
            
            incorrect_names_match <- grepl(paste(incorrect_names, collapse = "|"), 
                                           data[row_index, column_index], ignore.case = TRUE)
            #the collapse = '|' makes it search for any of the words in the data column
            incorrect_names_match <- incorrect_names_match[incorrect_names_match == TRUE]
            
            if (grepl(paste0("\\(", names_to_match[name_count], "\\)"), data[row_index, column_index], 
                      ignore.case = TRUE) == TRUE){
              #'this makes sure that if a match is found for "(mandrel)", "(tip)", etc.
              #'that it resets the indices
              row_index = -1
              column_index = -1
            }
            else if (length(incorrect_names_match) > 0){
              #if there was at least one match
              
              #this make sure a match is not found when the code notices die-cut
              #has been found to occur for PPS 90057309 and 90057311
              #this makes sure a match is not found for the core mandrel. An example is PPS 90951412
              #this makes sure is a match not found for the tip retainer. An example is PPS 90971813
              #this makes sure is a match not found for the die spacer. An example is PPS 90971813
              #this makes sure is a match not found for the slicking die. An example is PPS 90971813
              #this makes sure is a match not found for the sparker die. An example is PPS 90971813
              #this makes sure a match is not found for the distance die. Examples are PPS documents
              # 2-TD0346-003, -004, -006, -012, -013, -015, -016, and -017
              row_index = -1
              column_index = -1
            }
            else if (row_index < 3){
              #this prevents a match occuring with the name of the part. Such as if the part name has
              #tip or mandrel in it.
              row_index = -1
              column_index = -1
            }
            else{
              #if the match does not meet any of the above conditions, it breaks out of the  
              #placeholder match while loop
              break
            }
          } #end if to check if the placeholder is a note
          
          
          placeholder_count <- placeholder_count + 1
        } #end while
        
        if (row_index != -1){
          # breaks out of the placeholder while loop to stop searching the other matches if
          # an approprioate match was already found
          break
        }# end if to break out of the name match while loop
        
      } # end if of the note length check
      
      name_count <- name_count + 1 #update name_count
    } # end name find while
    
    if (row_index != -1){
      # break out of the outer while loop if a match was found
      break                                                                       
    } # end if
    
    data_count <- data_count + 1 # update data_count
  } # end while that searches through the columns
  
  tooling_name_index <- paste(names_to_match[1], "row index")
  assign(tooling_name_index, row_index, envir = se.debug.e)
  
  if (row_index != -1){
    #this gets the actual value of the tooling
    
    if (length(data[row_index, column_index + 1]) == 0){
      # makes sure the replacement length is not zero
      desired_data <- "Replacement Length Was Zero"
      
      if (nchar(data[row_index, column_index]) > 50){
        # ensure that a note is not picked up instead. A note will be longer than 50 characters
        desired_data <- "Replacement Length Was Zero and A note was read"
      }
    }
    else{
      desired_data <- data[row_index, column_index + 1]
    }
    
    
    #the next two ensure that if there is a space between the spec name and value then it checks
    #the next columns
    
    
    desired_count <- 2
    while (desired_count < (ncol(data) + 1 - column_index)){
      #desired_count is the number of columns after thr original desired data
      
      #this split the desired data into its characters and removes spaces
      split_characters <- strsplit(as.character(desired_data), "")
      #as.character was added to prevent strsplitting on a non-character argument, such as NA
      split_characters_nospace <- split_characters[[1]][split_characters[[1]] != ""]
      #removes spaces from the string split
      
      if (is.na(desired_data) || desired_data == ""){
        #sometimes there will be a space between the spec and the output
        desired_data <- data[row_index, column_index + desired_count]
      }
      else if (split_characters_nospace[1] == "9" && !grepl("e", desired_data, ignore.case = TRUE)){
        #if the first character of the index value is a 9, it is a print, and it will not pass
        #the value of the print
        #the grepl ensure that a match is not found for the scientific notation of a tooling size
        # for example "9.4E-2" should not be matched as a part number
        desired_data <- data[row_index, column_index + desired_count]
      }
      else if (split_characters_nospace[1] == "-"){
        #prevents things associated with the tab of a print, for example -01
        desired_data <- data[row_index, column_index + desired_count]
      }
      else{
        #breaks out of the checking the columns if one cell contained the appropraite data
        break
      }
      
      desired_count <- desired_count + 1
    }
    
    tooling_spec <- splitToolingString(desired_data)
    
    return (tooling_spec)
  }#enf if to check if the row_index != -1
  else{
    return ("Tooling was not found")
  }
  
} #end getTooling

getScrew <- function(data){
  #this function will get the print numbers of the screw
  #' it will find the column of the screw and the row, it will then find the column with 'p/n' or
  #' 'print number' and find the intersection between the two.
  #' if no intersection is found, it will check every column for a 9 (print match) and that the 
  #' associated number is 8 characters long (the length of a print)
  
  column_count <- 1
  data_columns <- ncol(data) #number of columns in the data frame
  screw_row <- -1 #row where screw is found
  screw_column <- -1 #row where screw is found
  print_column <- -1 #column where the print is located
  screw_print <- "" #initialize it to be empty
  
  
  #debuggin parameters
  se.debug.e$screw_found <- FALSE
  se.debug.e$screw_printcolumn_found <- FALSE
  se.debug.e$screw_open_find <- FALSE
  
  while (column_count < data_columns + 1){
    #this searches through the columns of the data frames to find matches of the screw
    
    placeholder <- grep("screw", data[, column_count], ignore.case = TRUE)
    mismatch <- grep("screw speed", data[,column_count], ignore.case = TRUE) #checks for a mismatch
    
    placeholder <- setdiff(placeholder, mismatch)
    
    if(length(placeholder) != 0){
      #break out of the while loop
      screw_column <- column_count
      screw_row <- placeholder[1]
      se.debug.e$screw_found <- TRUE
      break
    }#end if checking placeholder length
    
    column_count <- column_count + 1
  } #end while of searching through columns for screw
  
  part_strings <- c("print", "p/n", "part")
  column_count <- screw_column + 1 #check for the print in the columns after screw
  
  se.debug.e$screw_column <- screw_column
  se.debug.e$screw_row <- screw_row
  se.debug.e$column_count <- -1 #initialize to minus 1, will be updated if screw is found
  se.debug.e$screwprint_extracolumn <- -1
  se.debug.e$screwnote <- FALSE
  se.debug.e$screwnote_index <- -1 #note index for the screw print
  se.debug.e$screwnote_opensearch <- FALSE #whether there is an index for the note section
  se.debug.e$screw_note_opensearch_index <- -1 #where it was found
  
  
  
  if (screw_row != -1){
    #checks to make sure there is a screw row, otherwise the next check will not work
    
    while (column_count < data_columns + 1){
      #this will now search for 'p/n' or 'print number' or 'part number'
      #' it seaches for it after finding screw, becuase this column name will have to come after
      #' that column that contains the word 'screw'
      
      part_string_count <- 1
      while (part_string_count < 4){
        #checks the columns for the three string found in part_strings where the print should be 
        #located in the same column
        
        search_string <- part_strings[part_string_count]
        placeholder <- grep(search_string, data[,column_count], ignore.case = TRUE)
        
        if (length(placeholder) > 0 && placeholder[1] < screw_row){
          #the column header should come before the row that screw is located in
          #this prevents picking up a note
          
          screw_print_temp <- data[screw_row, column_count]
          
          #check overall cell
          if (length(screw_print_temp) == 0 || is.na(screw_print_temp) || nchar(screw_print_temp) < 8){
            #in all these conditions nothing should be done because the screw print grabbed is not
            #a proper print (either it is empty, is NA, or is less than the required print length)
            
          } #end if checking cell of screw print
          else if (grepl("note", screw_print_temp, ignore.case = TRUE) == TRUE &&
                   se.e$use_note == TRUE){
            #'if there is not screw print in the cell but instead it says to see the notes section
            #'the code will look at the note section.
            #'It also ensure that a note section was found by splitPPS function.
            #'If there is an integer index in the cell, the code will match the integer index to that
            #'in the notes, otherwise it will search the notes for Screw Print and grab all the
            #'print numbers in that cell and in the cell below.
            
            se.debug.e$screwnote <- TRUE #screw note was found
            screwnote_index <- -1 #initialize the index for the screw note if it is found
            
            first_sub <- gsub("\\(", " ", screw_print_temp) #remove a parenthese if present
            second_sub <- gsub(")", " ", first_sub) #remove a parenthese if present
            third_sub <- gsub(",", " ", second_sub) #remove commas
            fourth_sub <- gsub("\\.", " ", third_sub) #remove periods
            split_by_space <- strsplit(fourth_sub, " ") #split by spaces
            split_no_space <- split_by_space[[1]][split_by_space[[1]] != ""] #remove spaces
            
            words <- length(split_no_space) #number of words in the vector
            
            #'the next while loop will be searching every element in split_no_space to see if it is
            #'a single integer
            
            word_count <- 1 #the count for the next while loop
            
            while (word_count < words + 1){
              #loops through split_no_space to see if an element is an appropriate print
              
              current_word <- split_no_space[word_count]
              
              if(nchar(current_word) == 1){
                #checks to make sure the length is that of a single integer
                
                #it suppresses the warning to prevent NAs introduced by coercion
                current_word_numeric <- suppressWarnings(!is.na(as.numeric(current_word)))
                
                if (current_word_numeric == TRUE){
                  #if it is numeric it is a single integer index for the notes section
                  
                  screwnote_index <- current_word
                  se.debug.e$screwnote_index <- screwnote_index
                  break #breaks out of the while loop since an index was found
                  
                }#end if checking numeric
                
                
              }#end if checking word length
              
              word_count <- word_count + 1
            }#end while going through the words in the vector split_no_space
            
            
            if (screwnote_index != -1){
              #if the screw note index was found, the code will try to find that index in the notes
              #section. Otherwise it will search the notes for screwprint in the else statement
              
              note_section <- se.e$note_section
              note_section_column_length <- ncol(note_section)
              note_section_count <- 1
              
              while (note_section_count < note_section_column_length + 1){
                #goes through the columns of the note section search for the index
                
                #ensure that the cell starts with the number
                screw_placeholder <- grep(paste0("^", screwnote_index), 
                                          note_section[,note_section_count], ignore.case = TRUE)
                
                if (length(screw_placeholder) != 0){
                  #if a match was found
                  
                  note_cell_index <- screw_placeholder[1] #get the first match
                  screw_print_temp <- note_section[note_cell_index, note_section_count]
                  
                  first_sub <- gsub("\\(", " ", screw_print_temp) #remove a parenthese if present
                  second_sub <- gsub(")", " ", first_sub) #remove a parenthese if present
                  third_sub <- gsub(",", " ", second_sub) #remove commas
                  fourth_sub <- gsub("\\.", " ", third_sub) #remove periods
                  split_by_space <- strsplit(fourth_sub, " ") #split by spaces
                  split_no_space <- split_by_space[[1]][split_by_space[[1]] != ""] #remove spaces
                  
                  words <- length(split_no_space) #number of words in the vector
                  
                  #'the next while loop will be searching every element in split_no_space to see if 
                  #'it is a print. There may be multiple prints found in a cell
                  
                  word_count <- 1 #the count for the next while loop
                  
                  while (word_count < words + 1){
                    #loops through split_no_space to see if an element is an appropriate print
                    
                    current_word <- split_no_space[word_count]
                    
                    if(nchar(current_word) == 8){
                      #checks to make sure the length is that of a print
                      
                      first_char <- strsplit(current_word, "")[[1]][1]
                      last_char <- strsplit(current_word, "")[[1]][8]
                      
                      #the next two variable are booleans checking if the first and last char 
                      #are numeric it suppresses the warning to prevent NAs introduced by coercion
                      first_char_numeric <- suppressWarnings(!is.na(as.numeric(first_char)))
                      last_char_numeric <- suppressWarnings(!is.na(as.numeric(last_char)))
                      
                      if (first_char_numeric == TRUE && last_char_numeric == TRUE){
                        #if both are numeric and it has already met the length requirement, 
                        # it should bea print
                        
                        if (screw_print == ""){
                          screw_print <- current_word #overwrite because it is empty
                        }
                        else{
                          screw_print <- paste0(screw_print, "; ", current_word) 
                          #add to existing string
                        }
                        
                      }#end if checking first and last char are numeric
                      
                    }#end if checking word length
                    
                    word_count <- word_count + 1
                  }#end while going through the words in the vector split_no_space
                  
                  if (screw_print != ""){
                    #if screw_print was filled, return
                    se.debug.e$screwprint_column <- column_count
                    return(screw_print)
                  }
                  
                } #end if checking the length of placeholder
                
                note_section_count <- note_section_count + 1
              } #end while searching through the columns of the note section
              
            } #end else if for matching a note in a cell
            else{
              #the index was note found and will search for 'screw print' instead. It will search
              #for it and get all the print numbers in that cell and the one below it.
              
              se.debug.e$screwnote_opensearch <- TRUE
              
              note_section <- se.e$note_section
              note_section_column_length <- ncol(note_section)
              note_section_count <- 1
              
              while (note_section_count < note_section_column_length + 1){
                #goes through the columns of the note section search for the index
                
                #searches the column for screw print
                screw_placeholder <- grep("screw print", 
                                          note_section[,note_section_count], ignore.case = TRUE)
                
                if (length(screw_placeholder) != 0){
                  #if a match was found
                  
                  note_cell_index <- screw_placeholder[1] #get the first match
                  
                  se.debug.e$screwnote_opensearch_index <- note_cell_index
                  
                  screw_print_temp <- note_section[note_cell_index, note_section_count]
                  
                  first_sub <- gsub("\\(", " ", screw_print_temp) #remove a parenthese if present
                  second_sub <- gsub(")", " ", first_sub) #remove a parenthese if present
                  third_sub <- gsub(",", " ", second_sub) #remove commas
                  fourth_sub <- gsub("\\.", " ", third_sub) #remove periods
                  split_by_space <- strsplit(fourth_sub, " ") #split by spaces
                  split_no_space <- split_by_space[[1]][split_by_space[[1]] != ""] #remove spaces
                  
                  words <- length(split_no_space) #number of words in the vector
                  
                  #'the next while loop will be searching every element in split_no_space to see if 
                  #'it is a print. There may be multiple prints found in a cell
                  
                  word_count <- 1 #the count for the next while loop
                  
                  while (word_count < words + 1){
                    #loops through split_no_space to see if an element is an appropriate print
                    
                    current_word <- split_no_space[word_count]
                    
                    if(nchar(current_word) == 8){
                      #checks to make sure the length is that of a print
                      
                      first_char <- strsplit(current_word, "")[[1]][1]
                      last_char <- strsplit(current_word, "")[[1]][8]
                      
                      #the next two variable are booleans checking if the first and last char 
                      #are numeric it suppresses the warning to prevent NAs introduced by coercion
                      first_char_numeric <- suppressWarnings(!is.na(as.numeric(first_char)))
                      last_char_numeric <- suppressWarnings(!is.na(as.numeric(last_char)))
                      
                      if (first_char_numeric == TRUE && last_char_numeric == TRUE){
                        #if both are numeric and it has already met the length requirement, 
                        # it should bea print
                        
                        if (screw_print == ""){
                          screw_print <- current_word #overwrite because it is empty
                        }
                        else{
                          screw_print <- paste0(screw_print, "; ", current_word) 
                          #add to existing string
                        }
                        
                      }#end if checking first and last char are numeric
                      
                    }#end if checking word length
                    
                    word_count <- word_count + 1
                  }#end while going through the words in the vector split_no_space
                  
                  
                  #this now checks the cell below the previous one. It does this because
                  #sometimes the prints are found in two cells (one about that matched with screw
                  #print) and one below where the person finished filling in the information
                  
                  screw_print_temp_after <- note_section[note_cell_index + 1, note_section_count]
                  
                  if (!is.null(screw_print_temp_after) &&
                      !is.na(screw_print_temp_after)){
                    #ensure that the cell is not null (outside the bounds) or NA (empty)
                    
                    first_sub <- gsub("\\(", " ", screw_print_temp_after) #remove a parenthese
                    second_sub <- gsub(")", " ", first_sub) #remove a parenthese if present
                    third_sub <- gsub(",", " ", second_sub) #remove commas
                    fourth_sub <- gsub("\\.", " ", third_sub) #remove periods
                    split_by_space <- strsplit(fourth_sub, " ") #split by spaces
                    split_no_space <- split_by_space[[1]][split_by_space[[1]] != ""] #remove spaces
                    
                    words <- length(split_no_space) #number of words in the vector
                    
                    
                    #'the next while loop will be searching every element in split_no_space to see if 
                    #'it is a print. There may be multiple prints found in a cell
                    
                    word_count <- 1 #the count for the next while loop
                    
                    while (word_count < words + 1){
                      #loops through split_no_space to see if an element is an appropriate print
                      
                      current_word <- split_no_space[word_count]
                      
                      if(nchar(current_word) == 8){
                        #checks to make sure the length is that of a print
                        
                        first_char <- strsplit(current_word, "")[[1]][1]
                        last_char <- strsplit(current_word, "")[[1]][8]
                        
                        #the next two variable are booleans checking if the first and last char 
                        #are numeric it suppresses the warning to prevent NAs introduced by coercion
                        first_char_numeric <- suppressWarnings(!is.na(as.numeric(first_char)))
                        last_char_numeric <- suppressWarnings(!is.na(as.numeric(last_char)))
                        
                        if (first_char_numeric == TRUE && last_char_numeric == TRUE){
                          #if both are numeric and it has already met the length requirement, 
                          # it should bea print
                          
                          if (screw_print == ""){
                            screw_print <- current_word #overwrite because it is empty
                          }
                          else{
                            screw_print <- paste0(screw_print, "; ", current_word) 
                            #add to existing string
                          }
                          
                        }#end if checking first and last char are numeric
                        
                      }#end if checking word length
                      
                      word_count <- word_count + 1
                    }#end while going through the words in the vector split_no_space
                    
                  } #end if checking if cell si null or na
                  
                  
                  
                  if (screw_print != ""){
                    #if screw_print was filled, return
                    return(screw_print)
                  }
                  
                } #end if checking the length of placeholder
                
                note_section_count <- note_section_count + 1
              } #end while searching through the columns of the note section
              
            }#end if else for if the screwnote_index was found
            
          }# end if checking note
          else{
            #if the above conditions were not met, the screw print may be appropriate and will be 
            #analyzed
            
            first_sub <- gsub("\\(", " ", screw_print_temp) #remove a parenthese if present
            second_sub <- gsub(")", " ", first_sub) #remove a parenthese if present
            third_sub <- gsub(",", " ", second_sub) #remove commas
            fourth_sub <- gsub("\\.", " ", third_sub) #remove periods
            split_by_space <- strsplit(fourth_sub, " ") #split by spaces
            split_no_space <- split_by_space[[1]][split_by_space[[1]] != ""] #remove spaces
            
            words <- length(split_no_space) #number of words in the vector
            
            
            #'the next while loop will be searching every element in split_no_space to see if it is a
            #'print. There may be multiple prints found in a cell
            
            word_count <- 1 #the count for the next while loop
            
            while (word_count < words + 1){
              #loops through split_no_space to see if an element is an appropriate print
              
              current_word <- split_no_space[word_count]
              
              if(nchar(current_word) == 8){
                #checks to make sure the length is that of a print
                
                first_char <- strsplit(current_word, "")[[1]][1]
                last_char <- strsplit(current_word, "")[[1]][8]
                
                #the next two variable are booleans checking if the first and last char are numeric
                #it suppresses the warning to prevent NAs introduced by coercion
                first_char_numeric <- suppressWarnings(!is.na(as.numeric(first_char)))
                last_char_numeric <- suppressWarnings(!is.na(as.numeric(last_char)))
                
                if (first_char_numeric == TRUE && last_char_numeric == TRUE){
                  #if both are numeric and it has already met the length requirement, it should be
                  #a print
                  
                  if (screw_print == ""){
                    screw_print <- current_word #overwrite because it is empty
                  }
                  else{
                    screw_print <- paste0(screw_print, "; ", current_word) #add to existing string
                  }
                  
                  
                }#end if checking first and last char are numeric
                
                
              }#end if checking word length
              
              word_count <- word_count + 1
            }#end while going through the words in the vector split_no_space
            
            if (screw_print != ""){
              #if screw_print was filled, return
              se.debug.e$screwprint_column <- column_count
              return(screw_print)
            }
            
          }#end else checking the cell information
          
        }#end if for checking placeholder length and its first match
        
        
        part_string_count <- part_string_count + 1
      } #end while looping through part_string
      
      column_count <- column_count + 1
    } #end while checking columns
    
    #### Note Match Section ####
    ### if there was no column that matched print, p/n, or part, or the print number was not found
    # under such a column, it will search every column after the screw column for a print
    
    se.debug.e$screw_open_find <- TRUE
    extra_check_count <- screw_column + 1
    max_column_check <- min((extra_check_count + 5), data_columns) #tells the while loop when to stop
    #it checks five column after the screw column or to the end of the data frame, whichever comes
    #first
    
    while (extra_check_count < max_column_check){
      #goes through at most the next five columns to search if there is a print in the screw row
      
      screw_print_temp <- data[screw_row, extra_check_count]
      
      #check overall cell
      if (length(screw_print_temp) == 0 || is.na(screw_print_temp) || nchar(screw_print_temp) < 8){
        #in all these conditions nothing should be done because the screw print grabbed is not
        #a proper print (either it is empty, is NA, or is less than the required print length)
        
      } #end if checking cell of screw print
      else{
        #if the above conditions were not met, the screw print may be appropriate and will be 
        #analyzed
        
        first_sub <- gsub("\\(", " ", screw_print_temp) #remove a parenthese if present
        second_sub <- gsub(")", " ", first_sub) #remove a parenthese if present
        third_sub <- gsub(",", " ", second_sub) #remove commas
        fourth_sub <- gsub("\\.", " ", third_sub) #remove periods
        split_by_space <- strsplit(fourth_sub, " ") #split by spaces
        split_no_space <- split_by_space[[1]][split_by_space[[1]] != ""] #remove spaces
        
        words <- length(split_no_space) #number of words in the vector
        
        #'the next while loop will be searching every element in split_no_space to see if it is a
        #'print. There may be multiple prints found in a cell
        
        word_count <- 1 #the count for the next while loop
        
        while (word_count < words + 1){
          #loops through split_no_space to see if an element is an appropriate print
          
          current_word <- split_no_space[word_count]
          
          if(nchar(current_word) == 8){
            #checks to make sure the length is that of a print
            
            first_char <- strsplit(current_word, "")[[1]][1]
            last_char <- strsplit(current_word, "")[[1]][8]
            
            #the next two variable are booleans checking if the first and last char are numeric
            #it suppresses the warning to prevent NAs introduced by coercion
            first_char_numeric <- suppressWarnings(!is.na(as.numeric(first_char)))
            last_char_numeric <- suppressWarnings(!is.na(as.numeric(last_char)))
            
            if (first_char_numeric == TRUE && last_char_numeric == TRUE){
              #if both are numeric and it has already met the length requirement, it should be
              #a print
              
              if (screw_print == ""){
                screw_print <- current_word #overwrite because it is empty
              }
              else{
                screw_print <- paste0(screw_print, "; ", current_word) #add to existing string
              }
              
              
            }#end if checking first and last char are numeric
            
            
          }#end if checking word length
          
          word_count <- word_count + 1
        }#end while going through the words in the vector split_no_space
        
        if (screw_print != ""){
          #if screw_print was filled, return
          se.debug.e$screwprint_extracolumn <- extra_check_count
          return(screw_print)
        }
        
      }#end else checking the cell information
      
      extra_check_count <- extra_check_count + 1
    }#end the extra while to check at most the next five column
    
    
  } #end if checking the screw row
  
  
  
  
  return("Screw print not found") #if none of the methods worked
  
}#end getScrew

getFCATemperature <- function(data, names_to_match){
  #Gets the temperature data of the feedthroat, clamp, or adapter
  
  data_count <- 1 #will run through the while loop but it will also be the index of the column that
  #that contains the desired element since the while loop breaks 
  #if true
  
  
  row_index <- -1 #initialize the index of the row
  column_index <- -1 #initialize the index of the column
  names_length <- length(names_to_match)
  column_length <- ncol(data)
  
  while(data_count < column_length + 1){
    # this while loop runs through the second section and searches for the column that contains
    #either string in 'names_to_match'. If it finds the element, is saves the indices and then
    #breaks out of the while loop
    
    name_count <- 1 #runs through the vector of names_to_match
    
    while (name_count < names_length + 1){
      # this while loop checks if any of the names match anywhere in the column
      # if it does, it assigns the index to that name in second column of df and assigns TRUE to 
      # the third column
      placeholder <- grep(names_to_match[name_count], data[,data_count], ignore.case = TRUE)
      
      if (length(placeholder) != 0 && nchar(data[placeholder[1],data_count]) < 50){
        #in case of multiple matches, take the first
        #nchar ensures a note was not selected
        row_index <- placeholder[1]
        column_index <- data_count
        break
      } # end if
      else if(length(placeholder) == 1){
        row_index <- placeholder
        column_index <- data_count
        break
      } #end else if
      
      name_count <- name_count + 1 #update name_count
    } # end while
    
    data_count <- data_count + 1 # update data_count
  } # end while
  
  
  if (row_index != -1){
    
    if (length(data[row_index, column_index + 1]) == 0){
      # makes sure the replacement length is not zero
      desired_data <- "Replacement Length Was Zero"
      
      if (nchar(data[row_index, column_index]) > 50){
        # ensure that a note is not picked up instead. A note will be longer than 50 characters
        temps[intersect_count] <- "Replacement Length Was Zero and A note was read"
      }
    }
    else{
      desired_data <- data[row_index, column_index + 1]
    }
    
    if (nchar(data[row_index, column_index]) > 50){
      # ensure that a note is not picked up instead. A note will be longer than 50 characters
      desired_data <- "A note was read"
    }
    #the next two ensure that if there is a space between the spec name and value then it checks
    #the next columns
    if (is.na(desired_data) || desired_data == ""){
      #sometimes there will be a space between the spec and the output
      desired_data <- data[row_index, column_index + 2]
    }
    if (is.na(desired_data) || desired_data == ""){
      #sometimes there will be a space between the spec and the output
      desired_data <- data[row_index, column_index + 3]
    }
    
    temperature <- splitTemperatureString(desired_data)
    
    return (temperature)
  }
  else{
    return ("Temperature was not found")
  }
  
} #end getFCATemperature

getBarrelTemperatures <- function(data, names_to_match){
  #Gets the temperature data of the barrel
  
  temps <- c(-999, -999, -999) # store the three barrel temperatures
  intersect_indices <- c(-999, -999, -999) # stores indices of intersect of the names
  #and corresponding numbers
  
  data_count <- 1 #will run through the while loop but it will also be the index of the column that
  #that contains the desired element since the while loop breaks if true
  
  temp_strings <- matrix(c("1", "2", "3", "one", "two", "three"), nrow = 3, ncol = 2)
  row_index <- -1 #initialize the index of the row
  column_index <- -1 #initialize the index of the column
  column_length <- ncol(data)
  names_length <- length(names_to_match)
  temps_filled <- FALSE #boolean check if temps vectors has been filled
  
  while(data_count < column_length + 1){
    # this while loop runs through the second section and searches for the column that contains
    #either string in names_to_match'. If it finds the element, is saves the indices and then
    #breaks out of the while loop
    
    name_count <- 1 #runs through the vector of names_to_match
    
    while (name_count < names_length + 1){
      # this while loop checks if any of the names match anywhere in the column
      placeholder <- grep(names_to_match[name_count], data[,data_count], ignore.case = TRUE)
      
      
      if (length(placeholder) != 0 && nchar(data[placeholder[1],data_count]) < 50){
        # have to check to make sure grep does not return integer(0)
        #the nchar ensures that a note was not picked up
        
        barrelnumber_count <- 1
        #two iterations to check for 1 and one, 2 and two, 3 and three
        while (barrelnumber_count < 2){
          #check if 1, 2, 3 appears in the columns
          barrel_one <- grep(temp_strings[1,barrelnumber_count], data[,data_count], ignore.case = TRUE)
          barrel_two <- grep(temp_strings[2,barrelnumber_count], data[,data_count], ignore.case = TRUE)
          barrel_three <- grep(temp_strings[3,barrelnumber_count], data[,data_count], ignore.case = TRUE)
          
          if (length(barrel_one) != 0 || length(barrel_two) != 0 || length(barrel_three) != 0){
            #maybe there a renot three zones.
            break # breaks out of the while loop if one is found
          } # end if
          barrelnumber_count <- barrelnumber_count + 1 # update counter
        } # end while
        

        #the the first element
        if (length(barrel_one) != 0){
          intersect_indices[1] <- intersect(placeholder, barrel_one)[1]
        } #end if
        if (length(barrel_two) != 0){
          intersect_indices[2] <- intersect(placeholder, barrel_two)[1]
        }#end if
        if (length(barrel_three) != 0){
          intersect_indices[3] <- intersect(placeholder, barrel_three)[1]
        }#end if

        break # breaks out of the while loop since a match was found
      } # end if
      name_count <- name_count + 1 #update name_count
    } # end while
    
    intersect_count <- 1
    while (intersect_count < 4){
      
      
      if (length(data[intersect_indices[intersect_count], data_count + 1]) == 0){
        # makes sure the replacement length is not zero. Prevents intersect indices form being NA
        temps[intersect_count] <- "Replacement Length Was Zero"
        if (!is.na(intersect_indices[intersect_count]) && 
            !is.na(data[intersect_indices[intersect_count], data_count]) &&
            nchar(data[intersect_indices[intersect_count], data_count]) > 50){
          # ensure that a note is not picked up instead. A note will be longer than 50 characters
          temps[intersect_count] <- "Replacement Length Was Zero and A note was read"
        }
      }
      else if (!is.na(intersect_indices[intersect_count]) && intersect_indices[intersect_count] != -999){
        #checks to see if the matrix was filled and breaks out of the while loop. Checks if intersect
        #had a match, essentially.
        
        temps[intersect_count] <- data[intersect_indices[intersect_count], data_count + 1]
        temps[intersect_count] <- splitTemperatureString(temps[intersect_count])
        
        if (!is.na(intersect_indices[intersect_count]) && 
          !is.na(data[intersect_indices[intersect_count], data_count]) &&
          nchar(data[intersect_indices[intersect_count], data_count]) > 50){
          # ensure that a note is not picked up instead. A note will be longer than 50 characters
          temps[intersect_count] <- "A note was read"
        }
        if (is.na(temps[intersect_count]) || temps[intersect_count] == ""){
          #sometimes there will be a space between the spec and the output
          temps[intersect_count] <- data[intersect_indices[intersect_count], data_count + 2]
          temps[intersect_count] <- splitTemperatureString(temps[intersect_count])
        }
        if (is.na(temps[intersect_count]) || temps[intersect_count] == ""){
          #sometimes there will be a space between the spec and the output
          temps[intersect_count] <- data[intersect_indices[intersect_count], data_count + 3]
          temps[intersect_count] <- splitTemperatureString(temps[intersect_count])
        }
        
        
        temps_filled = TRUE # a barrel temperature has been found
      } # end if
      else {
        temps[intersect_count] <- "Barrel temperature is NA or ''"
      }
      intersect_count <- intersect_count + 1
    } #end while
    
    if (temps_filled == TRUE){
      break
    } # end if
    
    data_count <- data_count + 1 # update data_count
  } # end while
  
  return(temps)
  
} #end getBarrelTemepratures

getDieTemperatures <- function(data, names_to_match){
  #Gets the temperature data of the barrel
  
  #debuggin parameters
  se.debug.getDieTemperatures$die_found <- FALSE #whether grep found the dies
  se.debug.getDieTemperatures$die_found_matches <- NA #stores all the grep indices of the dies
  se.debug.getDieTemperatures$number_matches <- NA #stores the matches of the numbers  se.debug.getDieTemperatures$intersect_indices <- c(-1,-1)
  
  
  
  temps <- c(-999, -999) # store the two die temperatures
  intersect_indices <- c(-999, -999) # stores indices of intersect of the names
  #and corresponding numbers

  
  data_count <- 1 #will run through the while loop but it will also be the index of the column that
  #that contains the desired element since the while loop breaks if true
  
  temp_strings <- matrix(c("1", "2", "one", "two"), nrow = 2, ncol = 2)
  row_index <- -1 #initialize the index of the row
  column_index <- -1 #initialize the index of the column
  names_length <- length(names_to_match)
  column_length <- ncol(data)
  temps_filled <- FALSE
  
  while(data_count < column_length + 1){
    # this while loop runs through the second section and searches for the column that contains
    #either string in names_to_match'. If it finds the element, is saves the indices and then
    #breaks out of the while loop
    
    name_count <- 1 #runs through the vector of names_to_match
    
    while (name_count < names_length + 1){
      # this while loop checks if any of the names match anywhere in the column
      placeholder <- grep(names_to_match[name_count], data[,data_count], ignore.case = TRUE)
      
      if (length(placeholder) != 0 && nchar(data[placeholder[1],data_count]) < 50){
        # have to check to make sure grep does not return integer(0)
        #nchar ensures a note is not read
        
        dienumber_count <- 1
        #two iterations to check for 1 and one, 2 and two
        while (dienumber_count < 3){
          #check if 1, 2 appears in the columns
          die_one <- grep(temp_strings[1,dienumber_count], data[,data_count], ignore.case = TRUE)
          die_two <- grep(temp_strings[2,dienumber_count], data[,data_count], ignore.case = TRUE)
          if (length(die_one) != 0 || length(die_two) != 0){
            #break if a match was found for one.
            #it will find both if both exist because they will be labelled the same
            break
          } # end if
          dienumber_count <- dienumber_count + 1 # update counter
        } # end while
        
        
        if (length(intersect(placeholder, die_one)) > 0){
          #finds the intersect
          intersect_indices[1] <- intersect(placeholder, die_one)[1]
        }
        else{
          intersect_indices[1] <- NA
        }
        
        if (length(intersect(placeholder, die_two)) > 0){
          intersect_indices[2] <- intersect(placeholder, die_two)[1]
        }
        else{
          intersect_indices[2] <- NA
        }
        
        intersect_count <- 1 # intialize intersect_count
        
        while (intersect_count < 3){
          # goes through the intersect of the indices and see gets the data in the correct cell
          # of the PPS by using the indices in the inersect_indices
          
          intersect_index <- intersect_indices[intersect_count]
          
          if (!is.na(intersect_index) && intersect_index != -999){
            #checks to see if the matrix was filled and breaks out of the while loop. Checks if 
            #intersect had a match, essentially.
            
            #it gets the data in the next column after the temp column labels
            temperature_temp <- data[intersect_index, data_count + 1]
            
            if (is.na(temperature_temp) == TRUE || temperature_temp == ""){
              #sometimes there will be a space between the label and the temp
              temperature_temp <- data[intersect_index, data_count + 2]
            }
            if (is.na(temperature_temp) == TRUE || temperature_temp == ""){
              #sometimes there will be a space between the label and the temp
              temperature_temp <- data[intersect_index, data_count + 3]
            }
            
            temps[intersect_count] <- splitTemperatureString(temperature_temp)
            temps_filled <- TRUE
          } # end if
          else{
            temps[intersect_count] <- NA
          } #end else
          
          intersect_count <- intersect_count + 1
        } # end while
        
        return(temps) #if it was in the if loop, it found the die labels. If no data was found
        #that will be reflected in temps, but nevertheless, the code picked up the labels
        #so there is no use in checking further columns
        
      } # end if
      
      name_count <- name_count + 1 #update name_count
    } # end while
    
    data_count <- data_count + 1 # update data_count
  } # end while
  
  return(temps)
  
} #end getDieTemperatures

getAttribute <- function(data, names_to_match, desired_tolerance){
  #Gets the attribute data of the OD, ID, wall thickness, OOR, concentricity, length, and 
  #perpendicularity (also includes special dimensions for tapered and multi-layered extrusion)
  
  desired_data <- c(-999, -999, -999, -999, -999)
  
  desired_value <- -999 #the value of the attribute that is wanted (either USL, UCL, Target, LCL, or LSL)
  
  data_count <- 1 #will run through the while loop but it will also be the index of the column that
  #that contains the desired element since the while loop breaks 
  #if true
  
  
  row_index <- -1 #initialize the index of the row
  column_index <- -1 #initialize the index of the column
  
  while(data_count < ncol(data) + 1){
    # this while loop runs through the second section and searches for the column that contains
    #either string in names_to_match'. If it finds the element, is saves the indices and then
    #breaks out of the while loop
    
    name_count <- 1 #runs through the vector of names_to_match
    
    while (name_count < length(names_to_match) + 1){
      # this while loop checks if any of the names match anywhere in the column

      placeholder <- grep(paste0("^",names_to_match[name_count]), data[,data_count], ignore.case = TRUE)
      #the paste0 of '^' says that the string must start with the name. It prevents words like
      #'product' from appearing when we search 'OD'
      
      #Preventing matches to pp or ppk
      pp_match <- grep("pp", data[,data_count], ignore.case = TRUE)
      placeholder <- setdiff(placeholder, pp_match)
      
      
      #Preventing a match for wall uniformith when checking wall thickness
      if (names_to_match[name_count] == "wall"){
        wall_uniformity_match <- grep("wall uniformity", data[,data_count], ignore.case = TRUE)
        placeholder <- setdiff(placeholder, wall_uniformity_match)
      }
      
      
      #Trying to prevent any matches to 'Scrap Proximal Length'
      scrap_match <- grep("scrap", data[,data_count], ignore.case = TRUE)
      placeholder <- setdiff(placeholder, scrap_match)
      
      
      #Trying to prevent people who mistype scrap
      scap_match <- grep("scap", data[,data_count], ignore.case = TRUE)
      placeholder <- setdiff(placeholder, scap_match)
      
      
      if (length(placeholder) > 1){
        placeholder <- placeholder[1]
      }
      else if (length(placeholder) == 0){
        #in case it did not match '^OD' because there is a space
        placeholder <- grep(paste0(" ",names_to_match[name_count]), data[,data_count], ignore.case = TRUE)
        
        #Preventing matches to pp or ppk
        pp_match <- grep("pp", data[,data_count], ignore.case = TRUE)
        placeholder <- setdiff(placeholder, pp_match)
        
        
        #Preventing a match for wall uniformith when checking wall thickness
        if (names_to_match[name_count] == "wall"){
          wall_uniformity_match <- grep("wall uniformity", data[,data_count], ignore.case = TRUE)
          placeholder <- setdiff(placeholder, wall_uniformity_match)
        }
        
        
        #Trying to prevent any matches to 'Scrap Proximal Length'
        scrap_match <- grep("scrap", data[,data_count], ignore.case = TRUE)
        placeholder <- setdiff(placeholder, scrap_match)
        
        
        #Trying to prevent people who mistype scrap
        scap_match <- grep("scap", data[,data_count], ignore.case = TRUE)
        placeholder <- setdiff(placeholder, scap_match)
        
        if (length(placeholder) > 1){
          #this is just a heuristic of searching through the names
          placeholder <- placeholder[1]
        }
        
      }#end else if
      
      
      # have to check to make sure grep does not return integer(0)
      if (length(placeholder) != 0){
        break # breaks out of the while loop since a match was found
      } # end if
      name_count <- name_count + 1 #update name_count
    } # end while
    
    
    if (name_count < length(names_to_match) + 1){
      #if the previous while loop went inside the if statement, then name_count must be less than 
      # names_to match as it would have never had the final update
      row_index <- placeholder #gets index from previous while loop
      column_index <- data_count
      
      break;
    } # end if
    
    data_count <- data_count + 1 # update data_count
  } # end while
  
  if (name_count == length(names_to_match) + 1){
    #this means that no name was matched
    #we will check the other sections
    
    if (se.e$use_preanneal == TRUE && se.e$used_pre_anneal_section == FALSE){
      #checks pre anneal section
      se.e$used_pre_anneal_section <- TRUE #updates because it is used in the next line
      return(getAttribute(se.e$pre_anneal_section, names_to_match, desired_tolerance))
    }
    else if(se.e$use_attribute == TRUE && se.e$used_attribute_section == FALSE){
      #checks attribute section
      se.e$used_attribute_section <- TRUE
      return(getAttribute(se.e$attribute_section, names_to_match, desired_tolerance))
    }
    else{
      #if the other sections do not find a match it returns NA
      return(NA)
    }
    
  }
  
  
  spec_count <- 1 #initializes a spec count
  
  spec_names <- c("LSL", "LCL", "Target", "UCL", "USL")
  spec_indices <- matrix (nrow = 5, ncol = 1) #stores incidices for LSL, LCL, Target, UCL, and USL
  spec_values <- matrix(nrow = 5, ncol = 1) # stores the values of the attributes
  
  while(spec_count < length(spec_names) + 1){
    # this while loop runs through the data and searches for the column that contains
    #either spec_names. If it finds the elements, is saves the indices and then
    #breaks out of the while loop. It runs through the names first, and then through the column
    
    spec <- spec_names[spec_count]
    attributename_index <- -998 # the column index at which the attribute name is found
    #data_count is not used instead so -998 can be passed if the attribute name was never found
    
    data_count <- 1 #reinitialize data
    
    while (data_count < ncol(data) + 1){
      # this while loop checks if the attribute is found in the matrix
      placeholder <- grep(paste0("^",spec), data[,data_count], ignore.case = TRUE)
      #paste0 makes sure the string starts with the name. Prevents substring finds
      
      if (length(placeholder) == 0){# if it did not match
        placeholder <- grep(spec, data[,data_count], ignore.case = TRUE)
        if (length(placeholder) != 0){
          #removes space from the match and checks if it is a spec
          possible_match <- gsub(" ", "", data[placeholder[1],data_count])
          possible_match_verify <- grepl(paste0("^",spec), possible_match, ignore.case = TRUE)
          if (possible_match_verify == FALSE){
            placeholder <- integer(0)
          }#end if
        }#end if
      } #end if
      
      
      # have to check to make sure grep does not return integer(0)
      if (length(placeholder) != 0){
        attributename_index <- data_count

        break # breaks out of the while loop since a match was found
      } # end if
      
      data_count <- data_count + 1 # update data_count
    } # end while
    
    spec_indices[spec_count,1] <- attributename_index
    
    spec_count <- spec_count + 1 #update name_count
  } # end while
  
  if (row_index != -1){
    #checks for non-negative spec indice
    
    if (length(spec_indices[spec_indices == -998]) > 0){
      #checks for any missing specs (assigned -998)
      replace_count <- 1
      while (replace_count < 6){
        #runs through each index of desired_data and spec_indices
        if (spec_indices[replace_count] != -998){
          #replace if it does not equal -998. If it does, the value for the desired_data remains -999
          desired_data[replace_count] <- data[row_index, spec_indices[replace_count]]
        }
        replace_count <- replace_count + 1
      }
    }
    else {
      #if all the specs were matched, it assigned desired_data directly
      desired_data <- data[row_index, spec_indices]
    }
    
    
    #cleans up the values
    no_ft <- gsub("ft", "", desired_data, ignore.case = TRUE)
    no_quotes <- gsub("\"", "", no_ft, ignore.case = TRUE)
    desired_data <- gsub(" ", "", no_quotes, ignore.case = TRUE)
    
    names(desired_data) <- spec_names
  } #end if

  #### The next lines of code decide which value to pass #####
  ####
  
  desired_value <- desired_data[desired_tolerance]
  
  if (is.na(desired_value) || desired_value == "N/A" || desired_value == "n/a"){
    #checking if the desired value is NA. If yes, then it removes it
    non_na <- desired_data[!is.na(desired_data)] #removes NA values
    non_na <- non_na[non_na != "N/A"]
    non_na <- non_na[non_na != "n/a"]
    
    if (length(non_na) > 0){
      desired_value <- max(non_na) #gets the max value of the limits
    }#end if for length of non_na
    else{
      #if nothing then it searches the other sections
      if (se.e$use_preanneal == TRUE && se.e$used_pre_anneal_section == FALSE){
        #checks pre anneal section
        se.e$used_pre_anneal_section <- TRUE
        return(getAttribute(se.e$pre_anneal_section, names_to_match, desired_tolerance))
      }
      else if(se.e$use_attribute == TRUE && se.e$used_attribute_section == FALSE){
        #checks attribute section
        se.e$used_attribute_section <- TRUE
        return(getAttribute(se.e$attribute_section, names_to_match, desired_tolerance))
      }
      else{
        #if the other sections do not find a match it returns NA
        return(NA)
      }
    }
    
  }#end if checking if desired_data is na
  
  #cleaning
  desired_value <- gsub("<", "", desired_value)
  desired_value <- gsub("\\(ref\\)", "", desired_value, ignore.case = TRUE)
  
  first_char <- strsplit(desired_value, "")[[1]][1]
  
  if (!is.na(first_char) && first_char == "."){
    #adds a leading zero if there is none in front of a decimal
    desired_value <- paste0("0",desired_value)
  } #end if
  

  
  if (!is.na(desired_value)){
    #checks to make sure the value is not NA
    #check if it is an integer
    value_list <- strsplit(desired_value, "")[[1]]
    first_char_numeric <- !is.na(as.numeric(value_list[1])) #checks if it is a number
    last_char_numeric <-!is.na(as.numeric(value_list[length(value_list)]))
    
    if (first_char_numeric == TRUE &&
        last_char_numeric == TRUE && 
        desired_value > 0){
      # if the value is zero or negative (-999), does not round
      desired_value <- as.double(desired_value)
      desired_value <- round(desired_value, digits = 4)
      desired_value <- format(desired_value, scientific = FALSE) #removes scientific notation
    }
  }
  
  #if it gets to this section, then the code worked properly and I will reset the sections used
  #for the next attribute
  
  
  se.e$used_post_anneal_section <- FALSE
  se.e$used_pre_anneal_section <- FALSE
  se.e$used_attribute_section <- FALSE
  return(desired_value)
  
} #end getAttribute

splitTemperatureString <- function(temperature_string){
  #gets the temperature from the number in the temperature
  
  if (length(temperature_string) == 0){
    return (temperature_string)
  }
  else if(is.na(temperature_string)){
    return (temperature_string)
  }
  else if(temperature_string == ""){
    return (temperature_string)
  }
  else if(grepl("NA", temperature_string, ignore.case = TRUE) == TRUE){
    return (temperature_string)
  }
  
  remove_target <- gsub(pattern = "target", replacement = "", temperature_string, ignore.case = TRUE)
  remove_equals <- gsub(pattern = "=", replacement = "", remove_target, ignore.case = TRUE)
  split_space <- strsplit(remove_equals, " ")
  no_blanks <- unlist(split_space)[!split_space[[1]] == ""]
  first_temp <- no_blanks[1]
  split_error <- strsplit(first_temp, "±")[[1]][1]
  split_degree <- strsplit(split_error, "°")[[1]][1]
  split_farenheit <- strsplit(split_degree, "F")[[1]][1]
  split_celcius <- strsplit(split_farenheit, "C")[[1]][1]
  split_broken <- strsplit(split_celcius, "Â")[[1]][1]
  split_degree_second <- strsplit(split_broken, "°")[[1]][1]
  split_underscore <- strsplit(split_degree_second, "_")[[1]][1]
  split_minus <- strsplit(split_underscore, "-")[[1]][1]
  before_final <- gsub(pattern = "?", replacement = "", split_minus, ignore.case = TRUE)
  before_final2 <- gsub(pattern = "\\(ref\\)", replacement = "", before_final, ignore.case = TRUE)
  final <- gsub(pattern = "*", replacement = "", before_final2, ignore.case = TRUE)
  
  
  if (!is.na(final)){
    #checks to make sure the value is not NA
    #check if it is an integer
    temperature_list <- strsplit(final, "")[[1]]
    first_char_numeric <- !is.na(as.numeric(temperature_list[1])) #checks if it is a number
    last_char_numeric <-!is.na(as.numeric(temperature_list[length(temperature_list)]))
    
    if (first_char_numeric == TRUE &&
        last_char_numeric == TRUE && 
        final > 0){
      # if the value is zero or negative (-999), does not round
      final <- as.double(final)
      final <- round(final, digits = 0)
      final <- format(final, scientific = FALSE) #removes scientific notation
    }
  }
  
  return(final)
}#end splitTemperatureString

splitToolingString <- function(tooling_string){
  #splits string found from the tooling
  first_sub <- gsub("\"", " ", tooling_string)
  second_sub <- gsub("\\(", " ", first_sub)
  third_sub <- gsub("*", "", second_sub)# no space because this will split every character
  fourth_sub <- gsub(",", " ", third_sub)
  if (grepl("e-", fourth_sub, ignore.case = TRUE) == TRUE){
    #this prevents scientific notation (such as 5E-2) from being split incorrectly
    fifth_sub <- fourth_sub
  }
  else{
    fifth_sub <- gsub("-", " ", fourth_sub)
  }
  
  
  se.debug.e$tooling_after_sub <- fifth_sub
  
  if(grepl("x", fifth_sub, ignore.case = TRUE) == TRUE){
    #checks if there is an x for the mandrel because a core is buth through it (0.46 x 0.100)
    sixth_sub <- gsub(" ", "", fifth_sub)
    split_x <- strsplit(sixth_sub, "x") #lowercase x
    if(length(split_x[[1]]) == 1){
      #seperated by an uppercase X
      split_x <- strsplit(sixth_sub, "X")
    }#end if for uppercase X
    split_spaces <- strsplit(split_x[[1]][2], " ") #gets what follow x
  }#end if for checking x
  else{
    split_spaces <- strsplit(fifth_sub, " ")
  }
  
  no_spaces <- split_spaces[[1]][split_spaces[[1]] != ""] #gets ride of blanks in the list
  desired_value <- no_spaces[1]
  
  se.debug.e$tooling_after_nospaces <- desired_value
  
  first_char <- strsplit(desired_value, "")[[1]] #gets the first charcaters of the string
  
  if (!is.na(desired_value) && first_char == "."){
    #adds a leading zero if there is none in front of a decimal
    desired_value <- paste0("0",desired_value)
  } #end if
  
  
  
  if (!is.na(desired_value)){
    #checks to make sure the value is not NA
    #check if it is an integer. If it is, then it rounds the values.
    value_list <- strsplit(desired_value, "")[[1]]
    first_char_numeric <- !is.na(as.numeric(value_list[1])) #checks if it is a number
    last_char_numeric <-!is.na(as.numeric(value_list[length(value_list)]))
    
    if (first_char_numeric == TRUE &&
        last_char_numeric == TRUE && 
        desired_value > 0){
      # if the value is zero or negative (-999), does not round
      desired_value <- as.double(desired_value)
      desired_value <- round(desired_value, digits = 4)
      desired_value <- format(desired_value, scientific = FALSE) #removes scientific notation
    }
  }
  se.debug.e$tooling_after_cleaning <- desired_value
  
  return(desired_value)
  
}#end splitToolingString

getExtrusionType <- function(data){
  #'this function determines whether the PPS is for an extruded component that is a single extrusion,
  #'multi-extrusion, or tapered extrusion. It cannot determine ILC extrusions.
  #'The code does this by parsing through the PPS data and searching for keywords. It returns "single"
  #'for single extrusion, "multi" for multi-extrusion, and "tapered" for tapered extrusion.
  
  ############### Multi-section  ###############
  
  matches <- 0 #reassign 0 to matches to check for multi-extrusion
  numberofrows <- nrow(data)
  row_count <- 1
  
  while (row_count < numberofrows + 1){
    #this while loops goes through each row in data to search for 'Extruder 1', 'Extruder 2',
    #'Extruder A', 'Extruder B', etc.
    
    
    ####### Regular Extruder Match ####### 
    
    extruder_match <- grep("Extruder", data[row_count,], ignore.case = TRUE)
    
    if (length(extruder_match) > 0){
      ##Checking for #1, #2, #3
      one_match <- grep("#1", data[row_count,], ignore.case = TRUE)
      two_match <- grep("#2", data[row_count,], ignore.case = TRUE)
      three_match <- grep("#3", data[row_count,], ignore.case = TRUE)
      
      one_intersect <- intersect(extruder_match, one_match)
      two_intersect <- intersect(extruder_match, two_match)
      three_intersect <- intersect(extruder_match, three_match)
      
      ##Dealing with overlaps (incase something like "extruder #1#2" is picked up)
      onetwo_intersect <- intersect(one_intersect, two_intersect)
      onethree_intersect <- intersect(one_intersect, three_intersect)
      twothree_intersect <- intersect(two_intersect, three_intersect)
      if (length(onetwo_intersect) != 0){
        two_intersect <- setdiff(two_intersect, onetwo_intersect)
      }
      if (length(onethree_intersect) != 0){
        three_intersect <- setdiff(three_intersect,onethree_intersect)
      }
      if (length(twothree_intersect) != 0){
        three_intersect <- setdiff(three_intersect,twothree_intersect)
      }
      
      
      first_total_matches <- length(one_intersect) + length(two_intersect) + length(three_intersect)
      
      if (first_total_matches > 2){
        return ("multi")
      }
      
      
      ##Checking for A, B
      A_match <- grep("A", data[row_count,], ignore.case = FALSE)
      B_match <- grep("B", data[row_count,], ignore.case = FALSE)
      
      A_intersect <- intersect(extruder_match, A_match)
      B_intersect <- intersect(extruder_match, B_match)
      
      ##Dealing with overlaps
      AB_intersect <- intersect(A_intersect, B_intersect)
      if (length(AB_intersect) != 0){
        B_intersect <- setdiff(B_intersect, AB_intersect)
      }
      
      second_total_matches <- length(A_intersect) + length(B_intersect)
      
      if (second_total_matches == 2){
        return("multi")
      }
      
      
      
      ##Checking for 1, 2, 3
      oneblank_match <- grep("1", data[row_count,], ignore.case = TRUE)
      twoblank_match <- grep("2", data[row_count,], ignore.case = TRUE)
      threeblank_match <- grep("3", data[row_count,], ignore.case = TRUE)
      
      oneblank_intersect <- intersect(extruder_match, oneblank_match)
      twoblank_intersect <- intersect(extruder_match, twoblank_match)
      threeblank_intersect <- intersect(extruder_match, threeblank_match)
      
      ##Dealing with overlaps (incase something like "extruder 12" is picked up)
      onetwoblank_intersect <- intersect(oneblank_intersect, twoblank_intersect)
      onethreeblank_intersect <- intersect(oneblank_intersect, threeblank_intersect)
      twothreeblank_intersect <- intersect(twoblank_intersect, threeblank_intersect)
      
      if (length(onetwoblank_intersect) != 0){
        twoblank_intersect <- setdiff(twoblank_intersect,onetwoblank_intersect)
      }
      if (length(onethreeblank_intersect) != 0){
        threeblank_intersect <- setdiff(threeblank_intersect,onethreeblank_intersect)
      }
      if (length(twothreeblank_intersect) != 0){
        threeblank_intersect <- setdiff(threeblank_intersect, twothreeblank_intersect)
      }

      third_total_matches <- length(oneblank_intersect) + length(twoblank_intersect) + 
        length(threeblank_intersect)
      
      if (third_total_matches == 2){
        return ("multi")
      }
      else if (third_total_matches == 3){
        return ("multi")
      }
      
    }
    
    row_count <- row_count + 1
    
    
    ####### Special Extruder Match #######
    
    extA_indices <- grep("ext A", data[row_count,], ignore.case = TRUE)
    extB_indices <- grep("ext B", data[row_count,], ignore.case = TRUE)
    
    extAandB_match <- length(extA_indices) + length(extB_indices)
    if (length(extA_indices) > 0 &&
        length(extB_indices) > 0 &&
        extAandB_match > 1
        ){
      return("multi")
    }#end if
    
    
    oneinch_indices <- grep("1\" DS", data[row_count,], ignore.case = TRUE)
    oneandhalfinch_indices <- grep("1.5\" DS", data[row_count,], ignore.case = TRUE)
    
    oneandoneandhalfinch_match <- length(oneinch_indices) + length(oneandhalfinch_indices)
    if (length(oneinch_indices) > 0 &&
        length(oneandhalfinch_indices) > 0 &&
        oneandoneandhalfinch_match > 1
        ){
      return("multi")
    }#end if
      
    
    alayer_indices <- grep("A \\(inner layer\\)", data[row_count,], ignore.case = TRUE)
    blayer_indices <- grep("B \\(outer layer\\)", data[row_count,], ignore.case = TRUE)
    
    aandblayer_match <- length(alayer_indices) + length(blayer_indices)
    if (length(alayer_indices) > 0 &&
        length(blayer_indices) > 0 &&
        aandblayer_match > 1
        ){
      return("multi")
    }#end if
    
    
  }#end while
  
  
  ############### Tapered Section ###############
  
  numberofcolumns <- ncol(data)
  column_count <- 1
  tapered_strings <- c("Prox", "Dist") #grep will match Prox to Proximal and Dist to Distal
  prox_matches <- 0
  dist_matches <- 0
  bumppuller_matches <- 0
  
  while (column_count < numberofcolumns + 1){
    #this while loop goes through each column in data and searches for matches for 'Prox' and 'Dist'
    prox_indices <- grep(tapered_strings[1], data[,column_count], ignore.case = TRUE)
    dist_indices <- grep(tapered_strings[2], data[,column_count], ignore.case = TRUE)
    bumppuller_indicies <- grep("Bump Puller", data[,column_count], ignore.case = TRUE)
    
    #checks if prox and dist matched because of the words approx (approximately) and distance
    approx_indices <- grep("approx", data[,column_count], ignore.case = TRUE)
    distance_indices <- grep("distance", data[,column_count], ignore.case = TRUE)
    
    #if there were false matches, it finds which indices were false
    prox_intersect <- intersect(prox_indices, approx_indices)
    dist_intersect <- intersect(dist_indices, distance_indices)
    
    #gets the value that did not have intersection with false positives
    if (length(prox_intersect) != 0){
      #if the length is zero, the code below will return a length zero even if prox_indices is filled
      actual_prox <- prox_indices[prox_indices != prox_intersect]
    }
    else{
      actual_prox <- prox_indices
    }
    if (length(dist_intersect) != 0){
      actual_dist <- dist_indices[dist_indices != dist_intersect]
    }
    else{
      actual_dist <- dist_indices
    }
    
    prox_matches <- prox_matches + length(actual_prox)
    dist_matches <- dist_matches + length(actual_dist)
    bumppuller_matches <- bumppuller_matches + length(bumppuller_indicies)
    
    column_count <- column_count + 1
  }#end while
  
  if (prox_matches > 0 &&
      dist_matches > 0 &&
      (prox_matches + dist_matches) > 2){
    #if there are at least three matches and one of each
    return("tapered")
  }
  if (bumppuller_matches > 0){
    return("tapered")
  }
  
  
  
  ############### Single Section ###############
  
  return("single") #returns single if non of the others were found
  
}#end getExtrusionType






