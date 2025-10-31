library(tidyverse)
library(terra)
library(sf)


#### 1. read and harmonize species occurrence observations ---- 

# read macrophyte species list ----

macrophytes = read.csv2(file  = "d:/Protect_BALTIC/WP3/data/occurrence_data/macrophyte_species_list_24_07_2024.csv")

# read and harmonize national data sets (Ant's & Javier's code) ----

EE <- readxl::read_xlsx("D:/Protect_BALTIC/WP3/data/occurrence_data/raw_occurrence_data/EE_benthic_species_obs_20250805.xlsx", 
                        guess_max = 20000) %>%
  select(c("origin_id", "station_id", "sample_id", "date", "quantity", "q_type",
           "q_unit", "samp_metho", "start_lat", "start_long", "scien_name", "depth", "sampling_equipment")) %>%
  mutate(country = "EE", date = as.character(date)) %>% 
  rename(method_cat = sampling_equipment)
unique(EE$date)
str(EE)

# EE <- read.csv2("d:/Protect_BALTIC/WP3/data/occurrence_data/raw_occurrence_data/EE_benthic_species_observations_19062024.csv", 
#                 sep=",", dec=".") %>%
#   select(c("origin_id", "station_id", "sample_id", "date", "quantity", "q_type",
#            "q_unit", "samp_metho", "start_lat", "start_long", "scien_name", "depth")) %>%
#   mutate(country = "EE")
# unique(EE$date)



DE <- read.csv2("D:/Protect_BALTIC/WP3/data/occurrence_data/raw_occurrence_data/DE_benthic_species_observations_24032025.csv", 
                sep="|", dec=".") %>% 
  select(c("original_id", "station_id", "sample_id", "date", "quantity", "quantity_type", 
           "quantity_unit", "sampling_method", "start_decimal_latitude", 
           "start_decimal_longitude", "scientific_name","water_depth")) %>%
  rename(origin_id = original_id,
         start_lat = start_decimal_latitude, 
         start_long = start_decimal_longitude, 
         samp_metho = sampling_method,
         scien_name = scientific_name,
         q_type = quantity_type,
         q_unit = quantity_unit,
         depth = water_depth) %>%
  mutate(country = "DE")
str(DE)

DK <- read.csv2("D:/Protect_BALTIC/WP3/data/occurrence_data/raw_occurrence_data/DK_benthic_species_observations_20250619.csv", 
                sep="|", dec=".") %>%
  select(c("origin_id","station_id", "sample_id", "date", "quantity", "q_type", "q_unit", 
           "sampl_meth", "start_lat", "start_long", "scien_name", "depth")) %>%
  rename(samp_metho = sampl_meth) %>%
  mutate(country = "DK", depth = as.numeric(depth))

str(DK)

FI <- read.csv2("D:/Protect_BALTIC/WP3/data/occurrence_data/raw_occurrence_data/FI_benthic_species_observations_20250610.csv", sep="|", dec=".")  %>% 
  select(c("unique_id","station_id", "sample_id", "date", "quantity", "q_type", "q_unit", 
           "sampl_meth", "start_lat", "start_long", "scien_name", "depth", "method_cat")) %>%
  rename(samp_metho = sampl_meth,
         origin_id = unique_id) %>%
  mutate(country = "FI",
         depth = as.numeric(depth))
str(FI)

ICES <- read.table("D:/Protect_BALTIC/WP3/data/occurrence_data/raw_occurrence_data/ICES_benthic_species_phytobenthos_23082024.csv", 
                   sep = "|", header = T) %>%
  select(c("origin_id","station_id", "sample_id", "date", "quantity", "q_type", "q_unit", 
           "s_method", "start_lat", "start_long", "scien_name", "CNTRY")) %>%
  mutate(s_method = recode(s_method,
                           "VV" = "Grab sampler", 
                           "SM" = "Grab sampler",
                           "BC" = "Corer",
                           "EK" = "Grab sampler",
                           "HOS" = "Hose",
                           "HP" = "Corer",
                           "HC" = "Corer",
                           "GUG" = "Grab sampler",
                           "PT" = "Grab sampler",
                           "KK" = "Dredge")) %>%
  mutate(CNTRY = recode(CNTRY,
                        "Finland" = "FI", 
                        "Sweden" = "SE",
                        "Lithuania" = "LT",
                        "Latvia" = "LV",
                        "Estonia" = "EE",
                        "Poland" = "PL",
                        "Denmark" = "DK",
                        "Germany" = "DE")) %>%
  rename(samp_metho = s_method,
         country = CNTRY)
str(ICES)

# Ensure origin_id, station_id, and sample_id are treated as characters
EE$origin_id <- as.character(EE$origin_id)
EE$station_id <- as.character(EE$station_id)
EE$sample_id <- as.character(EE$sample_id)
DK$origin_id <- as.character(DK$origin_id)
DK$station_id <- as.character(DK$station_id)
DE$origin_id <- as.character(DE$origin_id)
DE$station_id <- as.character(DE$station_id)
DE$sample_id <- as.character(DE$sample_id)
FI$origin_id <- as.character(FI$origin_id)
FI$station_id <- as.character(FI$station_id)
FI$sample_id <- as.character(FI$sample_id)
ICES$origin_id <- as.character(ICES$origin_id)
ICES$station_id <- as.character(ICES$station_id)
ICES$sample_id <- as.character(ICES$sample_id)


# check that EE and SE macrophyte obs from ICEs are not within the national datasets 

# Estonia 

# EE %>% 
#   st_as_sf(coords = c("start_long", "start_lat"), crs = st_crs(4326)) %>% 
#   st_write(dsn = "D:/temp/EE_data_points.geojson")
# 
# ICES %>% 
#   filter(country == "EE") %>% 
#   st_as_sf(coords = c("start_long", "start_lat"), crs = st_crs(4326)) %>% 
#   st_write(dsn = "D:/temp/ICES_EE_data_points.geojson")

# mostly not the same but some overlap 

ices.ee.sf = ICES %>% 
  filter(country == "EE") %>%
  st_as_sf(coords = c("start_long", "start_lat"), crs = st_crs(4326)) 

ee.sf = EE %>% 
  st_as_sf(coords = c("start_long", "start_lat"), crs = st_crs(4326)) 

int.res = st_intersects(ices.ee.sf, ee.sf)

# write the intersecting points 
# ices.ee.sf[lengths(int.res) > 0,] %>% 
#   select(station_id, date) %>% 
#   st_write(dsn = "D:/temp/ICES_EE_intersections.geojson")

# we will remove these from the ices data 

ices.ee.intersections = ices.ee.sf[lengths(int.res) > 0,]

ICES = ICES %>% 
  filter(!origin_id %in% ices.ee.intersections$origin_id)

# as there is lot of swedish data we will remove these as well from the data 
# we cannot make spatial check bcause swedish data doesnt have coordinate info


ICES = ICES %>% 
  filter(country != "SE")
# only 16k obs remain

## take out empty obs from national datasets 

# 793 rows 
ee.empty = EE %>% 
  filter(country == "EE" & is.na(q_type) & is.na(scien_name))

# 20493 rows 
fi.empty = FI %>% 
  filter(country == "FI" & scien_name == "No species recorded")

# remove these from national datasets 

EE = EE %>% 
  filter(!is.na(q_type))

FI = FI %>% 
  filter(scien_name != "No species recorded")

# merge 

empty.obs = rbind(ee.empty, fi.empty) %>% 
  mutate(
    date = as.Date(date), 
    year = year(date)
  )

# we need to extract env vars for the empty data before saving 

# Merge all datasets into a single table
BalticSea <- bind_rows(EE, DK, DE, FI, ICES) %>% 
  mutate(
    date = as.Date(date), 
    year = year(date)
    )

# BalticSea$date %>% range()
# BalticSea$year %>% range()

# dim(BalticSea)
# str(BalticSea)

### 2. check taxonomic nomenclature ----

# manually 

BalticSea = BalticSea %>% 
  mutate(
    scien_name = recode(scien_name, 
                        "Batrachospermum atrum" = "Batrachospermum",
                        "Enteromorpha intestinalis" = "Ulva intestinalis",
                        "Enteromorpha prolifera" =  "Ulva prolifera", 
                        "Furcellaria" = "Furcellaria lumbricalis", 
                        "Pylaiella/Ectocarpus" = "Ectocarpus_Pylaiella", # Aggregation
                        "Ranunculus circinatus" = "Ranunculus aquatilis var. diffusus",
                        "Zostera subg. Zostera marina"  = "Zostera marina", 
                        "Zostera (Zostera) marina" = "Zostera marina", 
                        # species that have their name changed along the way 
                        "Aglaothamnion roseum" = "Gaillona rosea",
                        "Cladophora pygmaea" = "Lychaete pygmaea",
                        "Petalonia zosterifolia" = "Planosiphon zosterifolius",
                        "Potamogeton lucens" = "Potamogeton illinoensis",
                        "Potamogeton compressus" = "Potamogeton zosteriformis",
                        "Potamogeton pectinatus" = "Stuckenia pectinata",
                        "Ranunculus circinatus" = "Ranunculus aquatilis var. diffusus",
                        "Spirodela polyrhiza" = "Spirodela polyrrhiza",
                        "Batrachospermum atrum" = "Torularia atra",
                        "Brongniartella byssoides" = "Vertebrata byssoides",
                        "Polysiphonia fucoides" = "Vertebrata fucoides"
    )
  )


sp.names.indata = unique(BalticSea$scien_name)
sp.names.indata = sort(sp.names.indata)

# species which are not in macrophyte list
sp.out = sp.names.indata[!sp.names.indata %in% macrophytes$Valid.scientifc.name..2019.]

# species, which have old (2012) species names  

old.species = sp.out[sp.out %in% macrophytes$Scientific.name.in.previous.Checklist..2012.]
old.table = macrophytes %>% 
  dplyr::filter(Scientific.name.in.previous.Checklist..2012. %in% old.species)

### species aggregations 

BalticSea = BalticSea %>% 
  mutate(
    scien_name = recode(scien_name, 
                        "Fucus vesiculosus" = "F_vesiculosus_F_radicans",
                        "Fucus radicans" = "F_vesiculosus_F_radicans",
                        "Coccotylus truncatus" = "Coccotylus_Phyllophora",
                        "Phyllophora pseudoceranoïdes" = "Coccotylus_Phyllophora",
                        "Ectocarpus siliculosus" = "Ectocarpus_Pylaiella ",
                        "Pylaiella littoralis" = "Ectocarpus_Pylaiella",
                        "Pilayella littoralis" = "Ectocarpus_Pylaiella"
          
    )
  )

# scientific synonym column lists Elatine Orthosperma as synonym (!) for Ranunculus baudotii 
# we will make these in to Ranunculus peltatus subsp. baudotii

BalticSea = BalticSea %>% 
  mutate(
    scien_name = case_when(
      scien_name == "Phyllophora pseudoceranoides" ~ "Coccotylus_Phyllophora", 
      scien_name == "Ranunculus baudotii" ~ "Ranunculus peltatus subsp. baudotii", 
      TRUE ~ scien_name
    )
  )


# BalticSea %>%
#   filter(scien_name %in% macrophytes$Valid.scientifc.name..2019.) %>%
#   nrow()
# 407342 obs 


### match with code with EC components list 

# edit - matching names . 2019 names have duplicates 
# create new id for unique species names 

mphytes2019 = tibble(
  scien_name = unique(macrophytes$Valid.scientifc.name..2019.), 
  new_id = paste0(1:length(unique(macrophytes$Valid.scientifc.name..2019.)), "N")
)

# join new id 
phytobenthos = left_join(
  BalticSea %>% 
    mutate(check_id = 1:nrow(BalticSea)),  # just to be sure  
  mphytes2019
)

# species which are listed in 2019 names 
phytobenthos.in.2019 = phytobenthos %>% filter(!is.na(new_id))
phytobenthos.out.2019 = phytobenthos %>% filter(is.na(new_id))

# match 2019 and 2012 names 
mphytes2012 = tibble(
  scien_name = macrophytes$Valid.scientifc.name..2019., 
  name12 = macrophytes$Scientific.name.in.previous.Checklist..2012.
)

# link 2012 names to new ids  
mphytes2012 = left_join(mphytes2012, mphytes2019) %>% 
  select(name12, new_id) %>% 
  rename(scien_name = name12)

# fetch new id for species which were non matched in 2019 but are found in 2012 names 
unique(phytobenthos.out.2019$scien_name)[unique(phytobenthos.out.2019$scien_name) %in% mphytes2012$scien_name] 
# 0 

# temp = left_join(phytobenthos.out.2019 %>% select(-new_id), mphytes2012)
# temp %>% filter(!is.na(new_id)) %>% nrow()
# 
# phytobenthos.in = bind_rows(phytobenthos.in.2019, temp)
# phytobenthos.out = temp %>% filter(is.na(new_id)) %>% select(-new_id)

phytobenthos.in = phytobenthos.in.2019
phytobenthos.out = phytobenthos.out.2019 %>% select(-new_id)


# match with scientific synonym 

mphytes.synonyms = tibble(
  scien_name = macrophytes$Valid.scientifc.name..2019., 
  name12 = macrophytes$Scientific.name.in.previous.Checklist..2012., 
  synonyms = macrophytes$Scientific.synonyms
)
# fetch new id 

mphytes.synonyms = left_join(mphytes.synonyms, mphytes2019)
# keep new id and synonyms 
mphytes.synonyms = mphytes.synonyms %>% select(synonyms, new_id) %>% 
  rename(scien_name = synonyms)

# semicolns to long 
mphytes.synonyms = mphytes.synonyms %>% 
  mutate(
    scien_name = strsplit(scien_name, ";")
  ) %>% 
  unnest(scien_name) %>% 
  mutate(scien_name = str_trim(scien_name))

# 
# mphytes.synonyms$scien_name[duplicated(mphytes.synonyms$scien_name)]

# we will leave out synonyms which link to multiple species entitities 

mphytes.synonyms = mphytes.synonyms %>% 
  filter(!duplicated(scien_name))

temp = left_join(phytobenthos.out, mphytes.synonyms)

# which out rows have found names in synonyms 

temp.in = temp %>% filter(!is.na(new_id)) # 2869 rows 
temp.out = temp %>% filter(is.na(new_id))

# adjust sci names 
# temp.in %>% 
#   select(scien_name, new_id) %>% 
#   unique() %>% 
#   rename(synonym = scien_name) %>% 
#   left_join(mphytes2019)

temp.in = temp.in %>% 
  mutate(
    scien_name= case_when(
      scien_name == "Chara aspera var. subinermis" ~ "Chara aspera", 
      scien_name == "Ranunculus baudotii" ~ "Ranunculus peltatus subsp. baudotii", # should these be combined
      scien_name == "Enteromorpha clathrata" ~ "Ulva clathrata", 
      scien_name == "Ceramium nodulosum" ~ "Ceramium virgatum", 
      TRUE ~ scien_name
    )
  )

# colnames(phytobenthos.in)
# colnames(temp.in)


phytobenthos.in = rbind(phytobenthos.in, temp.in)

# link to 2019 names with the new id 

phytobenthos.in = phytobenthos.in %>% 
  rename(old.name = scien_name) %>% 
  left_join(mphytes2019)

# filter out all species which are not macrophytes and those which have not matched 
phytobenthos.in %>% 
  filter(!scien_name %in% macrophytes$Valid.scientifc.name..2019.) %>% 
  pull(scien_name) %>% unique()


# final check do the names make sense 

phytobenthos.in %>%
  select(old.name, scien_name) %>%
  unique() %>%
  print()

# how may obs by country 

# phytobenthos.in %>% 
#   group_by(country) %>% 
#   tally()

# there are no obs from DE or DK 

# DE %>% 
#   filter(
#     scien_name %in% macrophytes$Valid.scientifc.name..2019. |
#       scien_name %in% macrophytes$Scientific.name.in.previous.Checklist..2012. | 
#       scien_name %in% mphytes.synonyms
#   )
# 
# DE$scien_name %>% unique()


# DK %>% 
#   filter(
#     scien_name %in% macrophytes$Valid.scientifc.name..2019. |
#       scien_name %in% macrophytes$Scientific.name.in.previous.Checklist..2012. | 
#       scien_name %in% mphytes.synonyms
#   )
# 
# DK$scien_name %>% unique()


# extract the environmental data

env.stack = terra::rast("data/predictors_250m/predictor_stack_02_07_2025/predictor_stack_02_07_2025.tif")

phyto.sf = st_as_sf(
  phytobenthos.in, coords = c("start_long", "start_lat"), crs = st_crs(4326)
) %>% 
  st_transform(crs = st_crs(env.stack))

#check 
# 
# ggplot(data = phyto.sf[sample(1:nrow(phyto.sf), size = 1000),]) + 
#   geom_sf()
# hmm the data is mostly from fin and est 

env.data = terra::extract(
  env.stack, 
  phyto.sf
)

# empty obs 
empty.sf = empty.obs %>% 
  st_as_sf(coords = c("start_long", "start_lat"), crs = st_crs(4326)) %>% 
  st_transform(crs = st_crs(env.stack))

env.data.empty = terra::extract(
  env.stack, 
  empty.sf
)

empty.obs = bind_cols(
  empty.obs, 
  env.data.empty %>% 
    select(-ID)
) %>% 
  filter(!is.na(phosphate_surface_cv_resample_250m)) %>% 
dplyr::rename(
  lat = start_lat, 
  lon = start_long
  ) %>% 
  dplyr::mutate(
    quantity = 0
  ) 

saveRDS(empty.obs, file = "data/occurrence_data/empty_sites.rds")

# are there many NA values 
# head(env.data)

# env.data %>% 
#   filter(is.na(phosphate_surface_cv_resample_250m)) %>% 
#   nrow()
# 3334

# let's write these points 

# bind_cols(env.data, st_coordinates(phyto.sf)) %>% 
#   filter(is.na(phosphate_surface_cv_resample_250m)) %>% 
#   select(ID, X, Y) %>% 
#   st_as_sf(coords = c("X", "Y"), crs = st_crs(3035)) %>% 
#   st_write(
#     dsn = "D:/temp/PB_NA_points_check.geojson", 
#     overwrite = T
#   )

# these are in finland, we will discard them 

phytobenthos.in = bind_cols(
  phytobenthos.in, 
  env.data %>% 
    select(-ID)
) %>% 
  filter(!is.na(phosphate_surface_cv_resample_250m))

# remove extra columns 

phytobenthos.in = phytobenthos.in %>% 
  rename(sp_id = new_id) %>% 
  select(
    -old.name, -check_id
  ) %>% 
  rename(
    lat = start_lat, lon = start_long
  )




## swedish data. 
# species names matching has been already done by Oscar 

swe = readRDS("D:/Protect_BALTIC/WP3/data/occurrence_data/raw_occurrence_data/Sw_species_aggregates_predictors_v4.rds")

swe.macrophytes = swe %>% 
  filter(Species.type == "Macrophyte" & year >= 2000)

## plot pres abs 
# there is no coordinates 

ggplot(
  data = swe.macrophytes %>% filter(taxon_name == "Fucus vesiculosus"), 
  aes(x = salinity_surface_mean_resample_250m)
) + 
  geom_density() + 
  facet_wrap(facets = vars(presence))

# need to ask oscar what are presence == 0 observations 

swe.macrophytes = swe.macrophytes %>%
  mutate(
    taxon_name = recode(taxon_name,
                        "F_vesiculosus/F_radicans" = "F_vesiculosus_F_radicans",
                        "Coccotylus/Phyllophora" = "Coccotylus_Phyllophora",
                        "Ectocarpus/Pylaiella" = "Ectocarpus_Pylaiella "
    )
  )

# we will filter out the species obs in species complexes to keep these out 
# from the modelling template. Oscar has already aggregated these, but the 
# species level obs are there as well 

swe.macrophytes = swe.macrophytes %>% 
  filter(
    !taxon_name %in% c(
      "Fucus vesisulosus", 
      "Fucus radicans", 
      "Coccotylus truncatus", 
      "Phyllophora pseudoceranoides", 
      "Pylaiella littoralis",
      "Ectocarpus siliculosus" 
    )
  )

# oscar has distinguished the species based on valid scientific name (2019) 
# and No. so the data might need to be reprocessed because numbers in EC species list 
# are not unique species identifiers.
# edit: thsi seems to have resolved by O so that the No of non-unique "species" is always 
# the first 

# noo = macrophytes %>% 
#   select(No, Valid.scientifc.name..2019.) %>% 
#   filter(
#     macrophytes$Valid.scientifc.name..2019. %>% duplicated()  
#   ) 
# swe.macrophytes %>% 
#   filter(taxon_name %in% noo$Valid.scientifc.name..2019.) %>% 
#   nrow()
# # 101615 

# swe.macrophytes %>% 
#     filter(taxon_name %in% noo$Valid.scientifc.name..2019.) %>%
#     group_by(No) %>% 
#     tally()

# are there species in swedish data which are not in 2019 species names but are in 2012? 
swe.macrophytes$taxon_name[
  swe.macrophytes$taxon_name %in% macrophytes$Scientific.name.in.previous.Checklist..2012. & !swe.macrophytes$taxon_name %in% macrophytes$Valid.scientifc.name..2019.] %>% 
  unique()

# "Gracilaria vermiculophylla"

# are there species in swe data which are not in 2019 spe names but which are 
# in the synonyms? 

# those species which dont match 2019 names but do match 2012 
swe12  = swe.macrophytes %>% 
  filter(
    !taxon_name %in% macrophytes$scien_name_2019
  ) %>% 
  filter(
    taxon_name %in% macrophytes$Scientific.name.in.previous.Checklist..2012.
  ) %>% 
  pull(taxon_name) %>% 
  unique()

"Gracilaria vermiculophylla"

# those species which are not in 2019 or 2012 names but are found in sci synonyms list 
swe.synonyms = swe.macrophytes %>% 
  filter(
    !taxon_name %in% macrophytes$scien_name_2019
  ) %>% 
  filter(
    !taxon_name %in% macrophytes$Scientific.name.in.previous.Checklist..2012.
  ) %>% 
  filter(
    taxon_name %in% mphytes.synonyms$scien_name
  ) %>% 
  pull(taxon_name) %>% 
  unique()
# ranunculus baudotii, zostera angustifolia 
  

swe.macrophytes %>% 
  filter(
    !taxon_name %in% macrophytes$scien_name_2019 & taxon_name %in% macrophytes$Scientific.name.in.previous.Checklist..2012.
  ) %>% 
  pull(taxon_name) %>% 
  unique()

swe.macrophytes$taxon_name %>% unique() %>% sort()

# "Ranunculus baudotii"        "Gracilaria vermiculophylla" "Zostera angustifolia"  

swe.notfound = mphytes.synonyms %>% 
  filter(scien_name %in% c("Ranunculus baudotii", "Gracilaria vermiculophylla", "Zostera angustifolia")) 

swe.notfound = left_join(swe.notfound %>% rename(swe.name = scien_name), mphytes2019)

# change the species column name to scien_name
swe.macrophytes = swe.macrophytes %>% 
  mutate(
    scien_name = case_when(
      taxon_name == "Gracilaria vermiculophylla" ~ "Agarophyton vermiculophyllum",
      taxon_name == "Ranunculus baudotii" ~ "Ranunculus peltatus subsp. baudotii", # this links w synonym also to Elatine orthosperma?????
      taxon_name == "Zostera angustifolia" ~ "Zostera marina",
      TRUE ~ taxon_name
    )
  )


# we'll combine all the data to get the counts of species obs to see what we want to model 

swe.count = swe.macrophytes %>% 
  filter(presence == 1) %>% 
  select(scien_name) %>% 
  group_by(scien_name) %>% 
  tally() %>% 
  rename(n_sweden = n)

# count without swedish data 
count.no.swe = phytobenthos.in %>% 
  group_by(scien_name) %>% 
  tally() %>% 
  rename(n_no_sweden = n)

macrophytes = left_join(
  macrophytes %>% rename(scien_name_2019 = Valid.scientifc.name..2019.), 
  count.no.swe %>% rename(scien_name_2019 = scien_name)
  )

macrophytes = left_join(
  macrophytes, 
  swe.count %>% rename(scien_name_2019 = scien_name)
)

macrophytes$n_no_sweden[is.na(macrophytes$n_no_sweden)] <- 0
macrophytes$n_sweden[is.na(macrophytes$n_sweden)] <- 0

macrophytes = macrophytes %>% 
  mutate(
    n_total = n_no_sweden + n_sweden
  )

macrophytes = left_join(
  macrophytes, 
  mphytes2019 %>% 
    rename(scien_name_2019 = scien_name)
  ) %>% 
  rename(sp_id = new_id)


# - there is about 800k observations in swedish data, about 400k in all other data together 

# temp = macrophytes %>% 
#   select(
#     scien_name_2019, n_no_sweden, n_sweden, n_total
#   ) %>% 
#   arrange(desc(n_total))
# 
# ggplot(data = temp, aes(y = n_total, x = scien_name_2019)) +
#   geom_col()

# are there duplicate species 




#   

# create a modelling template 

macrophyte_template = macrophytes %>% 
  select(scien_name_2019, n_total, Phylum) %>% 
  unique() %>% 
  mutate(methods_for_abs = "") # sampling methods considered for pseudo-absence generation
# creation 

# pick the methods combinations from the velmu modelling template 

# velmu.temp = readxl::read_xlsx("V:/Mallit_2025/scripts/Automation_2025_ed.xlsx")
# 
# velmu.temp = velmu.temp %>% 
#   filter(Group %in% c("Vascular plants ", "Algae", "Bryophyta")) %>% 
#   slice(-1)
# 
# velmu.temp$method %>% unique()
# velmu.temp %>% 
#   filter(method == "DV") %>% 
#   select(species)

macrophyte_template = macrophyte_template %>% 
  mutate(
    batchname = paste0(
      "sbatch ", 
      gsub(pattern = " ", replacement = "_", x = macrophyte_template$scien_name_2019), 
      ".sh"
    )
  ) %>% 
  mutate(
    methods_for_abs = case_when(
      scien_name_2019 %in% c(
        "Ceratophyllum demersum", 
        "Chara tomentosa", 
        "Chorda filum", 
        "Cladophora glomerata", 
        "Cladophora rupestris", 
        "Coccotylus_Phyllophora", 
        "F_vesiculosus_F_radicans",
        "Furcellaria lumbricalis", 
        "Tolypella nidifica", 
        "Vaucheria"
      ) ~ "DV",  # observable from dive and video data 
      TRUE ~ "D" # species are always recorded on species level by diving 
    )
  ) %>% 
  filter(n_total > 100) # we will only model species with more than 200 obs total 
    
# save the data files 
saveRDS(phytobenthos.in, file = "data/occurrence_data/data_call_data_for_modelling_04_08_2025.rds")
saveRDS(swe.macrophytes, file = "data/occurrence_data/swedish_macrophytes_2000_2024_ver4.rds")

# write the datafile names to template 

macrophyte_template$datafile1_name = "data_call_data_for_modelling_04_08_2025.rds"
macrophyte_template$datafile2_name = "swedish_macrophytes_2000_2024_ver4.rds"

macrophyte_template$sp_workname = gsub(pattern = " ", replacement = "_", x = macrophyte_template$scien_name_2019)

# write the macrophyte spcecies list

write_csv2(
  macrophyte_template, 
  file = "d:/Protect_BALTIC/WP3/data/occurrence_data/macrophyte_modelling_template_04_08_2025.csv"
)


write_csv2(
  macrophytes, 
  file = "d:/Protect_BALTIC/WP3/data/occurrence_data/macrophyte_species_list_04_08_2024_with_obs_count.csv"
)

