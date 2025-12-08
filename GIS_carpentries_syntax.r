# Geospatial Data Carpentry for Urbanism
# https://carpentries-incubator.github.io/r-geospatial-urban/index.html
# Day 1 AM: Episodes 1, 2, 3 (Harley)
# Day 1 PM: Episodes 4 (Savannah), 5 + GIS (Abbie)
# Day 2 AM: Episodes 6 (Abbie), 7 (Shelby)
# Day 2 PM: Episodes 8 (Shelby), 9 (Savannah), 15 (Shelby)

# Skipping Episodes 1-5 For Now

################################
# Episode 6: Open and Plot Vector Layers

# load the required packages
library(tidyverse)
library(sf)

# Import shapefiles
boundary_Delft <- st_read("data/delft-boundary.shp", quiet = FALSE) 

# gives us information about the geometry type: polygon
# sf package supports these geometries: point, linestring, polygon, multipoint, multilinestring, etc. 
st_geometry_type(boundary_Delft)

st_crs(boundary_Delft) # returns the coordinate reference system (CRS) used by the shapefile

st_crs(boundary_Delft)$Name # be careful this $ is outside of the parentheses
st_crs(boundary_Delft)$epsg

st_bbox() # function shows the extent of the layer
st_bbox(boundary_Delft)

#####As WGS 84 is a geographic CRS, the extent of the shapefile is displayed in degrees. We need a projected CRS, which in the case of the Netherlands is the Amersfoort / RD New projection. To reproject our shapefile, we will use the st_transform() function. For the crs argument we can use the EPSG code of the CRS we want to use, which is 28992 for the Amersfort / RD New projection. To check the EPSG code of any CRS, we can check this website: https://epsg.io/#####

boundary_Delft <- st_transform(boundary_Delft, crs = 28992)
st_crs(boundary_Delft)$Name
# now metric units
st_bbox(boundary_Delft)
st_crs(boundary_Delft)$units_gdal

# Plot a vector layer
# Now, let's plot this shapefile
geom_sf() # for the sf data
coord_sf() # to ensure the coordinates shown on the two axes are displayed in meters

ggplot(data = boundary_Delft) + geom_sf(size = 1, color = "black", fill = "cyan1") + 
  labs(title = "Delft Administrative Boundary") + 
  coord_sf(datum = st_crs(28992)) #displays the axes in meters


