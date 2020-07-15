
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rio)
library(readxl)
library(tidyverse)
library(lubridate)

# Directories
inputdir <- "data/da_sampling/2020_request/raw"
outputdir <- "data/da_sampling/2020_request/processed"
plotdir <- "data/da_sampling/figures"

# Read data
data_orig <- read_excel(file.path(inputdir, "DA_CFree_0520.xls.xlsx"))


# Data notes
################################################################################

# Vanessa Zubkousky, 6/12/19 email
# 1. The “Mod” data field before the biotoxin value field is a modifier field that 
#    should be added in front of the biotoxin value (IE. <2.5 ppm domoic acid).
# 2. Sample station coordinates are approximate. These have not been QCd or ground-truthed 
#    and may be incorrect.  If there are no coordinates, there are none in our database.
# 3. Only a portion of the commercial growing areas are tested for domoic acid routinely. 
#    For other areas it is usually event driven based on presence of Pseudo-nitzschia.
# 4. The shellfish growing area samples are generally collected weekly. There may be 
#    some breaks in data due to an area being temporarily closed for a variety of reasons.
# 5. The non-commercial sample sites are not sampled on a routine basis. Volunteer sampling 
#    is sporadic, based on the schedules of volunteers and plankton events. This data is not good for time series analysis. 
# 6. Sentinel mussels are mussels that have been placed in bags at a specific location 
#    (usually on a pier or buoy) for the purpose of collecting samples.
# 7. The razor clam samples are generally a single samples of meat from one 
#    individual clam. There may be some instances where there was a pooled sample of a few individuals. 
#    If you need more information for these samples, please let me know and I can see what we can find out.
# 8. Citations: Please cite the California Department of Public Health, Environmental Management Branch.

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  rename(sampleid="SRL #",
         date="Date -Sampled-",
         location="Sample Site", 
         da_ppm_prefix="Mod-ASP", 
         da_ppm="ASP -ug/g-",
         type_orig="Sample Type",
         county="County",
         lat_dd="Latitude",
         long_dd="Longitude",
         notes="Notes") %>% 
  # Add time columns
  mutate(date=ymd(date),
         year=year(date)) %>% 
  # Add species and type columns
  # 1. Add commas where missing so you can split based on commmas
  # 2. Split species and type based on comma
  # 3. Species to sentence case
  # 4. Replace cultured species
  mutate(type_orig=recode(type_orig, 
                          "Clam, razor"="Razor Clam",
                          "Gaper Clam meat"="Gaper Clam, meat",
                          "Gaper Clam viscera"="Gaper Clam, viscera",
                          "Razor Clam meat"="Razor Clam, meat",
                          "Razor Clam viscera"="Razor Clam, viscera",
                          "Rock Scallop adductor"="Rock Scallop, adductor",
                          "Rock Scallop viscera"="Rock Scallop, viscera"),
         comm_name=sub('\\s*,.*','', type_orig),
         type_new=sub('.*,\\s*','\\1', type_orig),
         type_new=tolower(ifelse(comm_name==type_new, NA, type_new)),
         tissue=ifelse(type_new%in%c("wild", "cultured", "sentinel") | is.na(type_new), "whole", type_new),
         type=ifelse(type_new%in%c("wild", "cultured", "sentinel"), type_new, "wild"),
         type=ifelse(comm_name=="Cultured Rock Scallop", "cultured", type),
         comm_name=freeR::sentcase(comm_name),
         comm_name=recode(comm_name, "Cultured rock scallop"="Rock scallop")) %>% 
  # Add scientific name
  mutate(species=recode(comm_name, 
                        "Basket cockle"="Clinocardium nuttallii",
                        "Bay mussel"="Mytilus galloprovincialis", # Mediterranean mussel
                        "Butter clam"="Saxidomus giganteus",
                        "Gaper clam"="Tresus nuttallii", 
                        "Kumamoto oyster"="Crassostrea sikamea", 
                        "Littleneck clam"="Leukoma staminea", # Pacific littleneck clam
                        "Manila clam"="Venerupis philippinarum",
                        "Pacific oyster"="Crassostrea gigas", 
                        "Pismo clam"="Tivela stultorum", 
                        "Razor clam"="Siliqua patula", # Pacific razor clam 
                        "Rock scallop"="Crassadoma gigantea", 
                        "Sea mussel"="Mytilus californianus", 
                        "Washington clam"="Saxidomus spp.")) %>% # Saxidomus giganteus or Saxidomus nuttalli
  # Fill in missing lat/longs
  # Humboldt Bay, Gunther Is. 1-1 = Indian Island, Humboldt Bay, Eureka, CA : 40.809702, -124.169595
  mutate(lat_dd=ifelse(location=="Humboldt Bay, Gunther Is. 1-1", 40.809702, lat_dd),
         long_dd=ifelse(location=="Humboldt Bay, Gunther Is. 1-1", -124.169595, long_dd)) %>% 
  # Arrange columns
  dplyr::select(sampleid,
         county, location, long_dd, lat_dd,
         year, date,
         type_orig, comm_name, species, type, tissue, da_ppm, da_ppm_prefix, notes) %>% 
  arrange(species, date)
  
# Are sample ids unique?
anyDuplicated(data$sampleid)

# Inspect data
str(data)
freeR::complete(data)
table(data$comm_name)
table(data$species)
table(data$tissue)
table(data$type)

# Check species names
freeR::suggest_names(sort(unique(data$species)))

# Plot points
ggplot(data, aes(x=long_dd, y=lat_dd, color=comm_name, size=da_ppm)) +
  geom_point()

# Export data
saveRDS(data, file=file.path(outputdir, "CDPH_mollusc_viscera_da_data.rds"))

# Summarize
stats <- data %>% 
  group_by(comm_name, species) %>% 
  summarize(n_samples=n()) %>% 
  arrange(desc(n_samples))

# Export data
write.csv(stats, file=file.path(outputdir, "CDPH_mollusc_da_data_summary.csv"), row.names=F)

