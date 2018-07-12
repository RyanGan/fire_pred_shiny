# creating shapefile from raster
library(raster)
library(sf)

# extract pm extent for point coords
e <- extent( -125, -67, 24, 49)

# create empty raster
# create empty raster
r <- raster(e, nrow=26, ncol=59)

# assign wgs84 crs to empty raster
crs(r) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

# read csv
coords <- read_csv('./grid_coords.csv')
head(coords)
summary(coords)
# grid raster
grid_r <- rasterize(coords[,3:4], r, coords[,2], fun=min)
# fire risk value
fire_risk <- read_csv('./current_fire_prob.csv')
# create fire r
fire_r <- rasterize(fire_risk[,3:4], r, fire_risk$pr_fire, fun=mean)
# fire size
fire_size <- read.csv('./current_fire_size.csv') 
fire_size_r <- rasterize(fire_size[,3:4], r, fire_size$size_val, fun=mean)

# raster brick of values
fire_brick <- brick(fire_r, fire_size_r)
names(fire_brick@data) <- c('fire_risk', 'fire_size')

# raster to polygon
grid_poly <- rasterToPolygons(fire_brick)

# write grid poly as simple feature
 writeOGR(obj = grid_poly, dsn = "./grid_poly", layer = "grid_poly",
          driver = "ESRI Shapefile")
