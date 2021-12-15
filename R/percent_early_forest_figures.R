## This script reads in polygon data and shows relationship between % early seral forests
## and body condition index of locations densities. The final objects are directly used
## in the script "density_plots". Also shows relationship digestible energy map

# Function to load and install packages
getPackages <- function(...) {
  
  pcks <- unlist(list(...))
  req <- lapply(pcks, require, character.only = TRUE)
  need <- pcks[req != TRUE]
  if(length(need) > 0) {
    install.packages(need)
    lapply(need, require, character.only = TRUE)
  }
  
}

# List packages needed for script
getPackages("ggplot2",   # Plotting
            "tidyverse", # Piping
            "sf",        # Spatial data in R, will superseded sp
            "raster",    # Reading in asc files
            "cowplot"
)

## Percent Early Seral Forests

# Load early seral forest polygon (forest harvest and wildfire) and watershed boundary polygon
forests <- sf::st_read("R/Shapefiles/wts_corsec4MP_HarvFireComplete1934_2017P.shp") 
watershed_boundaries <- sf::st_read("R/Shapefiles/wts_corsec4MP.shp")

# Using a zero-width buffer cleans up many topology problems in R
forests <- sf::st_buffer(forests, dist = 0)
watershed_boundaries <- sf::st_buffer(watershed_boundaries, dist = 0)

# Check coordinate reference system of both layers are the same
sf::st_crs(forests)
sf::st_crs(watershed_boundaries)

# Add age column to manipulate layer dataframe more easily
forests$age <- with(forests, abs(Correct_Ye - 2017))

# Add polygon id column for disturbed areas
forests$distID <- 1:nrow(forests)

# Create dataframe of forests according to age
forests_0_20 <- forests[with(forests, which(age >= 0 & age <= 20)), ]
forests_21_40 <- forests[with(forests, which(age >= 21 & age <= 40)), ]
forests_41_60plus <- forests[with(forests, which(age >= 41)), ]
forests_0_30 <- forests[with(forests, which(age >= 0 & age <= 30)), ]

# Intersect watershed boundaries with forest by age
int_0_20 <- sf::st_intersection(watershed_boundaries, forests_0_20)
int_21_40 <- sf::st_intersection(watershed_boundaries, forests_21_40)
int_41_60plus <- sf::st_intersection(watershed_boundaries, forests_41_60plus)
int_0_30 <- sf::st_intersection(watershed_boundaries, forests_0_30)

# Function to calculate overlapping areas
AreaPercent <- function(x, y) {
  
  attArea <- x %>%
    mutate(area = st_area(.) %>% as.numeric())
  
  attArea <- attArea %>% 
    as_tibble() %>%
    group_by(ID) %>%
    summarize(area = sum(area))
  
  watShedArea <- y %>%
    mutate(area = st_area(.) %>% as.numeric())
  
  AreaCalc <- merge(attArea, watShedArea, by = "ID")
  AreaCalc$perc <- with(AreaCalc, (area.x / area.y) * 100)
  return(AreaCalc)
  
}

# Apply function for different forests categories
# These objects will be used in the script "density_plots"
AreaPercent(int_0_20, watershed_boundaries)
AreaPercent(int_21_40, watershed_boundaries)
AreaPercent(int_41_60plus, watershed_boundaries)
AreaPercent_0_30 <- AreaPercent(int_0_30, watershed_boundaries)


## Digestible Energy Map

# Load digestible energy datasets, stack, project, and sum (min and max should be 0 and 114003, respectively)
grids <- list.files("R/Rasters" , pattern = "*.asc$")
eng <- raster::stack(paste0("R/Rasters/", grids))
raster::crs(eng) <- "+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
eng_full <- sum(eng)
plot(eng_full)

# Prepare raster data as dataframe 
engdf <- as.data.frame(eng_full, xy = TRUE)
names(engdf)[3] <- 'DigestibleEnergy' # Name value column
engdf[is.na(engdf[3]), "DigestibleEnergy"] <- 0

# Plot with ggplot2
digE <- ggplot() +
  geom_raster(data = engdf, aes(x = x, y = y, fill = DigestibleEnergy)) +
  coord_sf(datum = st_crs(26911)) +
  scale_fill_gradient(na.value = "white", low = "grey95", high = "black") +
  labs(x = "Longitude", y = "Latitude", fill = "Digestible Energy (kcal)") +
  theme_bw() +
  theme(panel.background = element_rect(colour = "black", size = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_blank())


## Standard Deviation in Digestible Energy Map

# Standard deviation in digestible energy per watershed
digEngVar <- raster::extract(eng_full, watershed_boundaries, 
                             fun = sd, na.rm = TRUE, df = TRUE, sp = TRUE) 
# Covert st object to sf
digEngVar <- sf::st_as_sf(digEngVar)

# Plot standard deviation in digestible energy per watershed
SDdigE <- ggplot(data = digEngVar) +
  geom_sf(aes(fill = layer)) +
  coord_sf(datum = st_crs(26911)) +
  scale_fill_continuous(name = "Standard Deviation in\nDigestible Energy (kcal)") + 
  labs(x = "Longitude", y = "Latitude") + 
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12)) 


# Multi-panel plot with cowplot, adjust aspect ratio for PDF
cowplot::plot_grid(digE, SDdigE,
                   rel_heights = c(1, 1),
                   align = "hv",
                   ncol = 1,
                   labels = c("A","B"),
                   label_size = 18,
                   label_fontfamily = "sans")


#digEngVar <- raster::extract(eng_full, watershed_boundaries, 
#                             fun = function(x, ...) (sd(x)/mean(x)) * 100, na.rm = TRUE,
#                             df = TRUE, sp = TRUE) # ellipsis for na.rm
