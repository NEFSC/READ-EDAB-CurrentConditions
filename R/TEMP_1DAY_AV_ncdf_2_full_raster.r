# TEMP_1DAY_AV_ncdf_2_full_raster
#
# 09.08.2015 Robert Leaf and Kevin Friedland 
#
# For each netCDF file in the specified directory a single "RAST" .RData file with 
# a variable named "masked.raster". 

TEMP_1DAY_AV_ncdf_2_full_raster <- function() { # start function
  
  require(chron)
  require(ncdf4)
  require(raster)
  
  index.name.1 <- "TEMP"
  index.name.2 <- "1DAY"
  index.name.3 <- "AV"
  
  # Determine the location of the .nc files
  netCDF.wd <- choose.dir(caption = "Where are the .nc files to analyze?")
  NCDF.vect <- dir(netCDF.wd,pattern = ".nc") 
  
  #######  NEED TO FIX  should be yr mo day  <><><><><M!!!!!!!!!!!!!!!!!!!!!!!!!
  #######    #######    #######    #######    #######    #######    #######  
  #######  
  #######  
  #######  
  #######  
  #######  
  #######  
  #######  
  #######  
  #######  
  #######  
  
  # Extract data from the file name
  year.label   <- as.numeric(substr(as.character(NCDF.vect),20,23))
#  day.label    <- as.numeric(substr(as.character(NCDF.vect),24,25))
#  month.label  <- as.numeric(substr(as.character(NCDF.vect),26,27))
  month.label    <- as.numeric(substr(as.character(NCDF.vect),24,25))
  day.label  <- as.numeric(substr(as.character(NCDF.vect),26,27))
  jul.code  <- julian(month.label,day.label,year.label, c(1,0,1800))
  date.code <- paste(sprintf("%04.0f",year.label),
                     sprintf("%02.0f",month.label),
                     sprintf("%02.0f",day.label),sep = ".")
  
  # Insert leading zeros in julian code
  jul.code <- sprintf("%09i", jul.code) 
  
  # Create sub-directory                                                                                                                # Set and create new directories
  netCDF.wd.sub <- paste(netCDF.wd,paste("FULL",paste(index.name.3,index.name.1,index.name.2,sep = "."),sep = "_"), sep = "\\")        # Name a new directory to place the saved netCDF files
  suppressWarnings(dir.create(netCDF.wd.sub))
  NC.RAST <- paste("RAST","FULL",paste(date.code,index.name.3,index.name.1,index.name.2,jul.code,"RData",sep = "."),sep = "_")
  
  if (length(NCDF.vect) > 0)      {
    for (j in 1:length(NCDF.vect))  {
      
      print(paste("Converting .nc to raster... ",NCDF.vect[j],sep = " "))
#      raster.ncdf <- open.ncdf(paste(netCDF.wd,NCDF.vect[j],sep = "\\"))
      raster.ncdf <- nc_open(paste(netCDF.wd,NCDF.vect[j],sep = "\\"))
      
      # Initialize values
      miss.val <- -9.99
      netCDF.extent <- c(0,360,-90,90)
      netCDF.dim <- c(720,1440) 
      
#      raster.full <- as.matrix(t(get.var.ncdf(raster.ncdf,varid = "sst")))   # Transpose matrix and pull out "sst" variable
      raster.full <- as.matrix(t(ncvar_get(raster.ncdf,varid = "sst")))   # Transpose matrix and pull out "sst" variable
      raster.full <- raster(raster.full)
      raster.full <- flip(raster.full, 'y')                        # These data need to be inverted on the y-axis
      
      xmin(raster.full) <- netCDF.extent[1]                         # Assign the extent of the full raster
      xmax(raster.full) <- netCDF.extent[2]
      ymin(raster.full) <- netCDF.extent[3]
      ymax(raster.full) <- netCDF.extent[4]
      
      # Need to change raster x-dimension: Split the raster into two halves and rearrange them
      hemi.1 <- as.matrix(raster::crop(raster.full,c(180,360,-90,90)))
      hemi.2 <- as.matrix(raster::crop(raster.full,c(0,180,-90,90)))
      raster.full <- raster(cbind(hemi.1,hemi.2))
      
      # Re-assign the extent of the raster
      raster.extent.new <- c(-180,180,-90,90)   # Set the extent of the raster and projection
      xmin(raster.full) <- raster.extent.new[1]
      xmax(raster.full) <- raster.extent.new[2]
      ymin(raster.full) <- raster.extent.new[3]
      ymax(raster.full) <- raster.extent.new[4]
      projection(raster.full) <- NA                          # Set the coordinate reference system to "NA"
      
      miss.val.ind.1 <- which(raster.full[] > miss.val - 10e-5)
      miss.val.ind.2 <- which(raster.full[] < miss.val + 10e-5)
      miss.val.ind <- intersect(miss.val.ind.1,miss.val.ind.2)
      raster.full[miss.val.ind] <- NA  
      masked.raster <- raster.full
    
      file.name.rast <- paste(netCDF.wd.sub,paste("RAST","FULL",
                                                  paste(date.code[j],
                                                        index.name.3,
                                                        index.name.1,
                                                        index.name.2,
                                                        jul.code[j],"RData",sep = "."),sep = "_"), sep = "\\")
       
      save(masked.raster,file = file.name.rast)
      
#      close.ncdf(raster.ncdf)
      nc_close(raster.ncdf)
      closeAllConnections()
      
    }
    
    if (length(NCDF.vect) == 0) {print("You have rasterized all .nc files")}    
  }
}  