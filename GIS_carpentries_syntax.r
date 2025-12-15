# Geospatial Data Carpentry for Urbanism
# https://carpentries-incubator.github.io/r-geospatial-urban/index.html
# Day 1 AM: Episodes 1, 2, 3 (Harley)
# Day 1 PM: Episodes 4 (Savannah), 5 + GIS (Abbie)
# Day 2 AM: Episodes 6 (Abbie), 7 (Shelby)
# Day 2 PM: Episodes 8 (Shelby), 9 (Savannah), 15 (Shelby)

# Skipping Episodes 1-5 For Now

###############################
# Episode 4 (I'm teaching)
# Introduction to Visualization
# gg stands for grammar of graphs
# three components: data, aesthetics (coordinate system), geometries

library(tidyverse)
# ?? Was gapminder installed and loaded in a previous lesson ??
library(gapminder)

# plot distribution of life expetancy
ggplot(data = gapminder, 
       aes(x = lifeExp)) + geom_histogram()

# plot with subset of observations
gapminder |> 
  filter(year == 2007 & continent == "Americas") |> 
  ggplot(aes(x = country, y = gdpPercap)) + 
  geom_col()
  
# flip that plot
gapminder |> 
  filter(year == 2007 & continent == "Americas") |> 
  ggplot(aes(x = country, y = gdpPercap)) + 
  geom_col() + 
  coord_flip()
# ordered alphabetically but might want to compare gdpPercap
# need to reorder factor levels (similar to earlier episode...check)

# order levels will depend on another variable - gdpPercap
gapminder |> 
  filter(year == 2007 & continent == "Americas") |> 
  mutate(country = fct_reorder(country, gdpPercap)) |> # note the mutate (not perm)
  ggplot(aes(x = country, y = gdpPercap)) + 
  geom_col() + 
  coord_flip()

str(gapminder$country)

################################
# Episode 6: Open and Plot Vector Layers

# load the required packages
library(tidyverse)
library(sf)

# Import shapefiles
boundary_Delft <- st_read("data/delft-boundary.shp", quiet = FALSE) 
st_read("data/delft-boundary.shp")

# gives us information about the geometry type: polygon
# sf package supports these geometries: point, linestring, polygon, multipoint, multilinestring, etc. 
st_geometry_type(boundary_Delft) # polygon

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

################################
# Episode 7: Explore and Plot by Vector Layer Attributes

lines_Delft # ??? This doesn't pull up anything
## Ah ha! it's a challenge activity from the previous episode
## Give FEEDBACK to creators

# Challenge Activity from Episode 6 (need objects for ep 7)
# read in delft-streets.shp and delft-leisure
lines_Delft <- st_read("data/delft-streets.shp") 
points_Delft <- st_read("data/delft-leisure.shp")
lines_Delft
points_Delft

# Back to Episode 7
lines_Delft # we can examine and manipulate like a df
ncol(lines_Delft)
names(lines_Delft)
head(lines_Delft)

head(lines_Delft$highway, 10)
unique(lines_Delft$highway)

# this is showing another way for unique values by treating it as a factor
factor(lines_Delft$highway) |> levels() # note alphabetical ordering and NAs gone

# Challenge: Explore point_Delft
points_Delft
names(points_Delft)
unique(points_Delft$leisure)
factor(points_Delft$leisure) |> levels()

# Subset Features
# Note: previous exercise used different df
names(lines_Delft) # refreshing what I looked at before
# Select only cycleways from the lines data
cycleway_Delft <- lines_Delft |> 
  filter(highway == "cycleway")
dim(cycleway_Delft)

# Calculate the total length of cycleways
# First we need to calculate the length of each segment with st_length()
cycleway_Delft <- cycleway_Delft |> 
  mutate(length = st_length(geometry)) # wow, that's cool

cycleway_Delft |> 
  summarize(total_length = sum(length)) 
# Feedback to creators: give more context here about the total length

# Plot only the cycleways
ggplot(cycleway_Delft) + geom_sf() +
  labs(title = "Slow mobility network in Delft", 
       subtitle = "Cycleways") +
  coord_sf(datum = st_crs(28992))
