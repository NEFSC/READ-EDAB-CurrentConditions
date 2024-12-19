# create_subraster_region
# 
# Function for subsetting a smaller (square) region from a directory of RAST files.
# EXample:
# From ncdf_2_full_raster a directory of "full" raster files (full world extent) is created.
# The user may wish to analyze a smaller spatial scale. The function extracts the pixel elements
# from the larger (in spatial extent) RAST files the user defined geographic extent.
# 
# Function arguments are: custom.extent the vector of c(xmin, xmax, ymin, ymax).
#                         custom.name the region name.
# 
# A new directory will be created containing RAST files based on the custom.name.
#
# 01.13.2014 Robert Leaf and Kevin Friedland



create_subraster_region <- function(custom.extent = c(-80,-60,32,48),
                                    custom.name = "NESREG",
                                    raster.directory = "C:\\3_rs_data")     { 

#create_subraster_region <- function(custom.extent = c(1,21,55,73),
#                                      custom.name = "NORWAY",
#                                      raster.directory = "C:\\3_rs_data")     { 
  
#create_subraster_region <- function(custom.extent = c(3,8,57,63),
#                                      custom.name = "SNORWAY",
#                                      raster.directory = "C:\\3_rs_data")     { 


#create_subraster_region <- function(custom.extent = c(-69,-56,45,58),
#                                      custom.name = "GSLREG",
#                                      raster.directory = "C:\\3_rs_data")     { 
    
#create_subraster_region <- function(custom.extent = c(-83,-73,24,40),
#                                      custom.name = "SHARK",
#                                      raster.directory = "C:\\3_rs_data")     { 
    
#create_subraster_region <- function(custom.extent = c(-82,-74,26,37),
#                                      custom.name = "SESREG",
#                                      raster.directory = "C:\\3_rs_data")     { 
    
#create_subraster_region <- function(custom.extent = c(-97.2,-81.6,24.5,31),
#                                    custom.name = "Northern_GOM",
#                                    raster.directory = "C:\\3_rs_data")     { 

#create_subraster_region <- function(custom.extent = c(-180,-150,16,30),
#                                      custom.name = "PACISLME",
#                                      raster.directory = "C:\\3_rs_data")     { 
    
#create_subraster_region <- function(custom.extent = c(-101,31,19,71),
#                                      custom.name = "NATLREG",
#                                      raster.directory = "C:\\3_rs_data")     { 
    
  
  
  # x.limits = c(-97.2,-81.6),
  # y.limits = c(26,30.95),
  
  require(maptools)
  require(raster)
  require(chron)
  
  input.dir <- choose.dir(default = raster.directory, caption = "Where are the raster files?")
  
  # Import raster and meta data derived from netCDF.crop
  input.stack.name <- dir(path = input.dir,pattern = "RAST")
  input.stack <- paste(input.dir,input.stack.name,sep="\\")
  
  # Create sub-directory if it does not exist
  extract.sub <- paste(input.dir,custom.name, sep = "\\")
  suppressWarnings(dir.create(extract.sub))
  
    for (j in 1:length(input.stack)) {                
      load(input.stack[j])
      masked.raster <- raster::crop(masked.raster,extent(custom.extent))       
      print(paste("Processing",(j),"of",length(input.stack)))
      rast.file.name <- paste("RAST",custom.name,strsplit(input.stack[j], split = "_")[[1]][length(strsplit(input.stack[j], split = "_")[[1]])],sep = "_")       
      rast.file.name <- paste(extract.sub,"\\",rast.file.name,sep="")
        
      save(masked.raster,file = rast.file.name)
    }  
}
