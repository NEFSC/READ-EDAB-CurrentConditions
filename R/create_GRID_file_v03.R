create_GRID_file <- function()  {
  
  # Fields for hardcoding the location of the rasters (the directory) and the shapefile (file)
  # These are character vectors.
  RAST.dir <- c()
  Shape.file <- c()
  
  require(raster)
  require(maptools)
  require(matlab) 
  require(fields)
  
  # Establish raster directory
  if (is.null(RAST.dir)) {
  RAST.dir = choose.dir(,caption = "Where are the target rasters located.")
  rast.list <- dir(path=RAST.dir,pattern = "RAST")
  load(paste(RAST.dir,rast.list[1],sep = "\\")) }
  index.raster <- masked.raster
  index.raster[] <- 1:ncell(index.raster)
  
  # Establish shape file
  if (is.null(Shape.file)) {
  ShapeStem   <- choose.files(default = "", caption = "Choose the shapefile for region extraction.") }
  OverlayFile <- readShapePoly(ShapeStem)
  OverlayFile.name <- unlist(strsplit(chartr(old = "\\",new = "/",ShapeStem),"/"))[length(unlist(strsplit(chartr(old = "\\",new = "/",ShapeStem),"/")))]
  shape.name <- substr(OverlayFile.name,1,c(nchar(OverlayFile.name) - 4))
  field.values <- as.data.frame(OverlayFile) 
  
  # Rename individual polygons in shape file
  # For multiple polygons in the .SHP file
  center.coords <- matrix(NA,dim(field.values)[1],2)
  tmp <- slot(OverlayFile, 'polygons')
  for (j in 1:length(tmp)) {
    sub.tmp <- slot(tmp[[j]],'Polygons')
    coords.mat <- c()
    for (k in 1:length(sub.tmp)) {
    coords.mat <- rbind(coords.mat,sub.tmp[[k]]@coords)
    }
    center.coords[j,1] <- mean(coords.mat[,1])
    center.coords[j,2] <- mean(coords.mat[,2])  }
  
  if (dim(field.values)[1] > 1)    {
    use.poly <- winDialog(type = c("yesno"),
                          message = "Will the GRID file use unique polygon names from the .shp file?             No means the coordinates of the polygon centroids will be used for naming.")
    
    if (use.poly == "YES") {
      print(head(field.values))
      detected.fields <- names(field.values)
      Polygon.headers <- winDialogString("Which of the fields refers to the unique name?",paste(detected.fields, collapse = " ", sep = ","))
      Polygon.headers.name   <- strsplit(Polygon.headers, split = " ")[[1]][1]
      Polygon.headers.number <- seq(1,dim(field.values)[1])                        
      Polygon.name   <- as.character(field.values[,which(names(field.values) == Polygon.headers.name)])
      Polygon.names  <- Polygon.name
      Polygon.number <- seq(1,dim(field.values)[1])
      rename.vect <- which(is.na(Polygon.name))
      Polygon.name[rename.vect] <- paste("UI",seq(1,length(rename.vect)),sep=".")   }
    
    if (use.poly == "NO") {
      Polygon.name <- paste(sprintf("%.3f", center.coords[,1]),"_",sprintf("%.3f", center.coords[,2]),sep = "")  
    }
  }
    
  # For single polygons in the .SHP file
  if (dim(field.values)[1] == 1)   {                        
    Polygon.name <- shape.name
    Polygon.names  <- Polygon.name 
    center.coords <- rbind(center.coords, center.coords)
  }
  
  # Initialize GRID.output function
  ##########################################################################################################################
  GRID.output <-  function(OverlayFile = OverlayFile,
                           index.raster = index.raster,
                           col.index = c(1,3))  {
    print("Extracting indices from input raster files")
    grid.indices <- extract(index.raster,OverlayFile,weights=TRUE,cellnumbers=TRUE) 
    l.gi <- length(grid.indices)
    
    grid.vals <- list()
    for (j in 1:l.gi) {
      grid.ind <- as.data.frame(grid.indices[[j]])
      grid.vals[[j]] <- cbind(grid.ind$cell,grid.ind$weight)  } 
    
    return(grid.vals)  } 
  ##########################################################################################################################
  grid.out <- GRID.output(OverlayFile = OverlayFile,
                          index.raster = index.raster)  
  
# xyFromCell(index.raster,grid.out[[1]][,1]))
# range(xyFromCell(index.raster,grid.out[[216]][,1])[,1])
# range(xyFromCell(index.raster,grid.out[[216]][,1])[,2])
# index.p <- index.raster
# plot(crop(index.p,extent(-88.9,-84.4,28.5,29.7)))
# v <- cbind(xyFromCell(index.raster,grid.out[[216]][,1]),grid.out[[216]][,2])
# plot(index.raster,xlim = c(-88.9,-84.4),ylim = c(28.5,29.7))
# plot(rasterToPolygons(index.p),add = T)
# text(v[,c(1,2)],label = v[,3],cex = 0.5)

  blank.raster <- raster(index.raster) 
  cell.numbers <- grid.out
  spatialPDF <- OverlayFile
  grid.dim <-  "shp"
  
  file.name.save <- paste("GRID_SHP_",shape.name,".RData",sep="")
  file.name.save <- paste(RAST.dir,file.name.save,sep="\\")
  
  save(index.raster,
       blank.raster,
       cell.numbers,
       center.coords,
       spatialPDF,
       grid.dim,
       Polygon.name,
       file = file.name.save)
  
  print(paste("Analyzing and saving file ",file.name.save))
 
}
  