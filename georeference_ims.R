georeference_IMS_snow_data <- function(filename="",geotiff=F){
  
  # you need the raster library to georeference
  # the image matrix
  require(raster)
  
  # Scan for the line on which "Dim Units"
  # occurs for the second time, I'll scan for the
  # first 200 lines if necessary, assuming no 
  # header will be longer than this even given
  # some changes in the header info through time
  # I also assume that the basic naming of the 
  # header info remains the same (file size etc)
  
  # the projection as used (polar stereographic (north))
  proj = CRS("+proj=stere +lat_0=90 +lat_ts=60 +lon_0=10 +k=1 +x_0=0 +y_0=0 +a=6371200.0 +b=6371200.0 +units=m +no_defs")
  
  # set extent based upon documentation info x / y resolution top left coordinate
  e = extent(-12126597.0,12126840,-12126597,12126840.0)
  
  # set string occurence to 0
  str_occurence = 0
  
  # attache and open file
  con <- file(filename)
  open(con)
  for(i in 1:200){
    
    # read a line of the ascii file
    l = readLines(con, n=1)
    
    # find the occurence of "Dim Units"
    occurence = grep("Dim Units",l,value=F)
    
    # only process valid results (skip empty strings)
    if(length(occurence)!=0){
      str_occurence = str_occurence + occurence 
    }
    
    # when two occurences are found return the
    # line of the second one
    if (str_occurence == 2){
      skip_lines <<- i # set the global variable skip_lines to i
      break
    }
  }
  # close file
  close(con)
  
  # read in the data after the header data assumes a 6144 x 6144 matrix
  data_list = scan(filename,skip=skip_lines,nlines=6144,what=character())
  
  # split every element (a long character string) in the list into it's
  # individual characters and return to the original list object
  data_list = lapply(data_list,function(x)substring(x,seq(1,nchar(x),1),seq(1,nchar(x),1)))
  
  # unlist the list and wrap it into a matrix
  data_matrix = matrix(as.numeric(unlist(data_list)),6144,6144)
  
  # convert to a raster object
  r = raster(data_matrix)
  
  # assign the raster a proper extent and projection
  extent(r) = e
  projection(r) = proj
  
  # return the raster
  if (geotiff == F){
    return(r)
  }else{
    no_extension = unlist(strsplit(basename(filename),split='\\.'))[1]
    writeRaster(r,paste(no_extension,".tif",sep=""),overwrite=T,options=c("COMPRESS=DEFLATE"))
  }
}

georeference_IMS_snow_data(filename="ims2014335.asc", geotiff=TRUE)
