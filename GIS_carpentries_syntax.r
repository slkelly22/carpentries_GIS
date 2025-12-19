# Geospatial Data Carpentry for Urbanism
# https://carpentries-incubator.github.io/r-geospatial-urban/index.html
# Day 1 AM: Episodes 1, 2, 3 (Harley)
# Day 1 PM: Episodes 4 (Savannah), 5 + GIS (Abbie)
# Day 2 AM: Episodes 6 (Abbie), 7 (Shelby)
# Day 2 PM: Episodes 8 (Shelby), 9 (Savannah), 15 (Shelby)

# Skipping Episodes 1-3 For Now

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
  
# flip the plot
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
  mutate(country = fct_reorder(country, gdpPercap)) |> # note the mutate (not permanent)
  ggplot(aes(x = country, y = gdpPercap)) + 
  geom_col() + 
  coord_flip()

# representing lifeExp with color
gapminder |> 
  filter(year == 2007 & continent == "Americas") |> 
  mutate(country = fct_reorder(country, gdpPercap)) |> 
  ggplot(aes(x = country, y = gdpPercap, fill = lifeExp)) + 
  # explain fill versus color
  geom_col() + coord_flip()

# changing color; popular choice for readability and colorblind is viridis package
# NOTE: don't they need to install viridis package? I have viridis package but scale_fill_viridis_c is listed as a function within ggplot so maybe they don't need the viridis package?

gapminder |> 
  filter(year == 2007 & continent == "Americas") |> 
  mutate(country = fct_reorder(country, gdpPercap)) |> 
  ggplot(aes(x = country, y = gdpPercap, fill = lifeExp)) + 
  geom_col() + 
  coord_flip() + 
  scale_fill_viridis_c() # c stands for continuous scale

# we want to know above or below average life expectancy
# use if_else within mutate to create new variable lifeExpCat
# if_else(condition, value if true, value if false)
# note: they are also adding a new function: scale_fill_manual
# SK Change: They save plot as an object, but I'm going to create it (so they can see it), and then save the plot

# step 1: build the plot so we can see it
gapminder |> 
  filter(year == 2007 & continent == "Americas") |> 
  mutate(
    country = fct_reorder(country, gdpPercap), 
    lifeExpCat = if_else(
      lifeExp >= mean(lifeExp), 
      "high", 
      'low')) |> 
  ggplot(aes(x = country, y = gdpPercap, fill = lifeExpCat)) + 
  geom_col() + 
  coord_flip() + 
  scale_fill_manual(values = c("lightblue", "orange")) #report error to creators: there was a space in blue color

# step 2: same thing, but save as an object
# content calls plot only "p" but I'm calling "avg_life_plot"
avg_life_plot <- gapminder |> 
  filter(year == 2007 & continent == "Americas") |> 
  mutate(
    country = fct_reorder(country, gdpPercap), 
    lifeExpCat = if_else(
      lifeExp >= mean(lifeExp), 
      "high", 
      'low')) |> 
  ggplot(aes(x = country, y = gdpPercap, fill = lifeExpCat)) + 
  geom_col() + 
  coord_flip() + 
  scale_fill_manual(values = c("lightblue", "orange"))

# adding title, legend, axes
avg_life_plot <- avg_life_plot + labs(
  title = "GPD per capita in Americas", 
  subtitle = "Year 2007", 
  x = "Country", 
  y = "GDP per capita", 
  fill = "Life Expectancy categories"
)

# view plot
avg_life_plot

# saving the plot
## NOTE: did we set up this file structure in the earlier lessons and there "here"?
# I'm not running the following code b/c I know it won't work without the file structure
ggsave(plot = avg_life_plot, filename = here("fig_output", "plot_americas_2007.pdf"))

# I'll save it like this for now
ggsave("americas_plot.pdf")
# that looks bad as a pdf and the Carp content said it would look bad and to run ??ggsave; I think they should REVISE that

# we'll just save as a png
ggsave("americas_plot.png")
?ggsave

# Saving the americas dataset and writing out as a csv
gapminder_amr_2007 <- gapminder |> 
  filter(year == 2007 & continent == "Americas") |> 
  mutate(
    country_reordered = fct_reorder(country, gdpPercap), # sk: now they're using a new column name
    lifeExpCat = if_else(lifeExp >= mean(lifeExp), "high", "low")
  )

# UGH, SWITCH THIS: saving with Base R and here 
write.csv(gapminder_amr_2007, 
          here("data_output", "gapminder_americas_2007.csv"), 
          row.names = FALSE)

# CHANGE  - Use Tidy and not the here function
# this way is more straightforward (but we might need to use here if we don't set up an R Project in the beginning of the lessons...check)
write_csv(gapminder_amr_2007, "gapminder_americas_2007.csv")

################################
# Episode 5: Introduction to Geospatial Concepts (Abbie)
# No R Code in this episode

################################
# Episode 6: Open and Plot Vector Layers (Abbie)

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
# Episode 7: Explore and Plot by Vector Layer Attributes (Shelby)

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
# 115550.1[m] in console (hard to see)
# Feedback to creators: give more context here about the total length

# Plot only the cycleways
ggplot(data = cycleway_Delft) + 
  geom_sf() +
  labs(title = "Slow mobility network in Delft", 
       subtitle = "Cycleways") +
  coord_sf(datum = st_crs(28992))

## Challenge: Now with motorways
# 1. Create anew object that only contains the motorways in Delft
motorways_Delft <- lines_Delft |> 
  filter(highway == "motorway")

# 2. How many features does the new object have? 
nrow(motorways_Delft) # 48

# 3. What is the total length of motorways? 
motorways_Delft <- motorways_Delft |> 
  mutate(length = st_length(geometry))

motorways_Delft |> 
  summarize(total_length = sum(length)) # 14877.44 [m]

# in the answer, they did this in one step like below: 
motorway_Delft_length <- motorways_Delft |> 
  mutate(length = st_length(geometry)) |> 
  select(everything(), geometry) |> # this line is unnecessary
  summarize(total_length = sum(length)) # same number I got above

# 4. Plot the motorways
ggplot(motorways_Delft) + 
  geom_sf() +
  labs(title = "Fast mobility network in Delft", 
       subtitle = "Motorways") +
  coord_sf(datum = st_crs(28992))

## Customize Plots
unique(lines_Delft$highway) # we're going to add distinct colors to four types

# watch how they use road_types
road_types <- c("motorway", "primary", "secondary", "cycleway")

lines_Delft_selection <- lines_Delft |> # note this is new object
  filter(highway %in% road_types) |> # see how they used the object; they could also just list the 4 types
  mutate(highway = factor(highway, levels = road_types)) # also used road_types as the fct levels

# they are adding colors like this?
road_colors <- c("blue", "green", "navy", "purple")

# now creating the plot with the different colors
ggplot(lines_Delft_selection) + 
  geom_sf(aes(color = highway)) + # technically this is all you need for colors
  scale_color_manual(values = road_colors) + # the blue and navy are hard to differentiate
  labs(
    color = "Road Type", 
    title = "Mobility Network of Delft", 
    subtitle = "Main Roads & Cycleways"
  ) +
  coord_sf(datum = st_crs(28992))

## Challenge Activity
# Again, why this way? 
line_widths <- c(1, 0.75, 0.5, 0.25)

ggplot(lines_Delft_selection) + 
  geom_sf(aes(color = highway, linewidth = highway)) + # add linewidth and below
  scale_color_manual(values = road_colors) + 
  scale_linewidth_manual(values = line_widths) + # see this function***
  labs(
    color = "Road Type", 
    linewidth = "Road Type",  # interestingly, merges the two legends
    title = "Mobility Network of Delft", 
    subtitle = "Main Roads & Cycleways"
  ) +
  coord_sf(datum = st_crs(28992))

## Challenge Activity
# first create a df where only roads where bicycles are allowed
lines_Delft_bicycle <- lines_Delft |> 
  filter(highway == "cycleway")

# plot - note the two dfs being called in different sections!
ggplot(lines_Delft) + # that's the main df we've used
  geom_sf() + # this is empty
  geom_sf(
    data = lines_Delft_bicycle, # that's the smaller df we just created
    aes(color = highway), 
    linewidth = 1
  ) + 
  scale_color_manual(values = "magenta") + 
  labs(
    title = "Mobility network in Delft", 
    subtitle = "Roads dedicated to Bikes") + 
  coord_sf(datum = st_crs(28992))


################################
# Episode 8: Plot Multiple Shapefiles (Shelby)

# We've been plotting with a single shapefile, but what if we wanted to use multiple? 
# We will create a plot that combines our leisure locations, municipal boundary, and street objects. Also build a custom legend

# To begin, we'll create a plot with the site boundary as the first layer. Then layer the leisure locations and street data on top in consecutive calls to geom_sf

ggplot() + 
  geom_sf(data = boundary_Delft, 
          fill = "lightgrey", 
          color = "lightgrey") + 
  geom_sf(
    data = lines_Delft_selection, 
    aes(color = highway), 
    size = 1
  ) + 
  geom_sf(
    data = points_Delft) + # I named by oject with an s on points, lesson has point
  labs(title = "Mobility network of Delft") + 
  coord_sf(datum = st_crs(28992))
  
# building a custom legend

points_Delft$leisure <-factor(points_Delft$leisure)
levels(points_Delft$leisure)
levels(points_Delft$leisure) |> length() # 15
# rainbow function....Oh, okay! 
?rainbow
leisure_colors <- rainbow(15)

# plotting
ggplot() + 
  geom_sf(
    data = boundary_Delft, 
    fill = "lightgrey", 
    color = "lightgrey") + 
  geom_sf(
    data = lines_Delft_selection, 
    aes(color = highway), 
    size = .5) + # I changed the size from 1 to .5 b/c that looks better (and more like the example)
  geom_sf(
    data = points_Delft, 
    aes(fill = leisure), 
    shape = 21) + 
  scale_color_manual(
    values = road_colors, 
    name = "Road Type") + 
  scale_fill_manual(
    values = leisure_colors, 
    name = "Leisure Location") + 
  labs(title = "Mobility network and leisure in Delft") + 
  coord_sf(datum = st_crs(28992))

## Challenge
# What value of shape will disply points as squares with custom fills? 
# shape 22; how do you know? grab the ggplot cheat sheet from the Help menu
ggplot() + 
  geom_sf(
    data = boundary_Delft, 
    fill = "lightgrey", 
    color = "lightgrey") + 
  geom_sf(
    data = lines_Delft_selection, 
    aes(color = highway), 
    size = .5) + # I changed the size from 1 to .5 b/c that looks better (and more like the example)
  geom_sf(
    data = points_Delft, 
    aes(fill = leisure), 
    shape = 22) + 
  scale_color_manual(
    values = road_colors, 
    name = "Road Type") + 
  scale_fill_manual(
    values = leisure_colors, 
    name = "Leisure Location") + 
  labs(title = "Mobility network and leisure in Delft") + 
  coord_sf(datum = st_crs(28992))

## Challenge
# Create a map of leisure locations only including playground and picnic table, etc. 
# There's more than one way to do this, but this is how they did it: 

# NOTE: how they filtered as they imported...
leisure_locations_selection <- st_read("data/delft-leisure.shp") |> 
  filter(leisure %in% c("playground", "picnic_table")) 

# checking the levels
factor(leisure_locations_selection$leisure) |> levels() # picnic_table, playground

# setting a color 
blue_orange <- c("cornflowerblue", "darkorange")

# plotting
ggplot() + 
  geom_sf(
    data = lines_Delft_selection, 
    aes(color = highway)) + 
  geom_sf(
    data = leisure_locations_selection, 
    aes(fill = leisure, shape = leisure)) + 
  scale_shape_manual( # NOTE this function scale_shape_manual!
    name = "Leisure Type", 
    values = c(21, 22)) + 
  scale_color_manual(
    name = "Line Type", 
    values = road_colors) + 
  scale_fill_manual(
    name = "Leisure Type", 
    values = blue_orange) + 
  labs(title = "Road network and leisure") + 
  coord_sf(datum = st_crs(28992))
  
################################
# Episode 9: Handling Spatial Projections and CRS (Savannah)