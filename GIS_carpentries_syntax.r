# Geospatial Data Carpentry for Urbanism
# Skipping Episodes 1-5
# Starting Below with Episode 6

# Open and Plot Vector Layers

# load the required packages
library(tidyverse)
library(sf)

# Import shapefiles
boundary_Delft <- st_read("data/delft-boundary.shp", quiet = FALSE) 

# gives us information about the geometry type: polygon
# sf package supports these geometries: point, linestring, polygon, multipoint, multilinestring, etc. 
st_geometry_type(boundary_Delft)

st_crs(boundary_Delft) # returns the coordinate reference system (CRS) used by the shapefile

st_crs(boundary_Delft$Name) # Not pulling the right output!!!!
