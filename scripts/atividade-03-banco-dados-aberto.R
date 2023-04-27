## Atividade 3

## Pacotes necessários

library(tidyverse)
library(rgbif)
library(dplyr)
library(bdc)
library(CoordinateCleaner)
# checar funcoes
?occ_data

# baixar ocorrencias
dori_gbif <- occ_data(scientificName = "Pterois volitans", 
                      hasCoordinate = TRUE,
                      hasGeospatialIssue=FALSE)

# dimensoes
dim(dori_gbif)

dim(dori_gbif$data)

# checar campos
dori_gbif$data %>% names


# checar problemas reportados
issues_gbif <- dori_gbif$data$issues %>% 
  unique() %>% 
  strsplit(., "[,]") %>% 
  unlist()

gbif_issues() %>% 
  data.frame() %>% 
  filter(code %in% issues_gbif)

dori_gbif1 <- dori_gbif$data %>%
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
                issues, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat)


dori_gbif1 <- dori_gbif1 %>% 
  distinct()

# checar niveis dos fatores
lapply(dori_gbif1, unique)

# checar coordenadas válidas
check_pf <- 
  bdc::bdc_coordinates_outOfRange(
    data = dori_gbif1,
    lat = "decimalLatitude",
    lon = "decimalLongitude")

# checar coordenadas válidas e próximas a capitais (muitas vezes as coordenadas são erroneamente associadas a capitais dos países)

cl <- dori_gbif1 %>%
  select(acceptedScientificName, decimalLatitude, decimalLongitude) %>%
  rename(decimallongitude = decimalLongitude,
         decimallatitude = decimalLatitude,
         scientificName = acceptedScientificName) %>% 
  as_tibble() %>% 
  mutate(val = cc_val(., value = "flagged"),
         sea = cc_sea(., value = "flagged"),
         capital = cc_cap(., value = "flagged"))

# verificar coordenadas com flags

# capitais (padrão é um raio de 10km)
cl %>% 
  rename(decimalLongitude = decimallongitude,
         decimalLatitude = decimallatitude) %>% 
  bdc::bdc_quickmap(., col_to_map = "capital")  


cl %>% 
  rename(decimalLongitude = decimallongitude,
         decimalLatitude = decimallatitude) %>% 
  bdc::bdc_quickmap(., col_to_map = "sea")  


# investigar niveis suspeitos
dori_gbif1 %>% 
  distinct(waterBody) %>% 
  pull()

# waterBody
dori_gbif1 %>%
  group_by(waterBody) %>% 
  summarise(occ = length(scientificName)) %>% 
  ggplot(aes(occ, y=waterBody)) +
  geom_bar(stat = 'identity') 

# fonte das regioes erradas
dori_gbif1 %>% 
  filter(waterBody %in% c("Northwest Atlantic")) %>% 
  distinct(datasetName)

# 25 ocorrencias
dori_gbif1 %>% 
  filter(datasetName %in% c("Puerto Rico Coral Reef Monitoring Program Database Compilation"))

# filtrar todas do dataset suspeito
dori_gbif_ok <- dori_gbif1 %>% 
  filter(!datasetName %in% c("Puerto Rico Coral Reef Monitoring Program Database Compilation"))


library(ggmap)
library(maps)
library(mapdata)
library(ggplot2)
library(robis)
world <- map_data('world')

# checar pontos

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = dori_gbif_ok, aes(x = decimalLongitude, y = decimalLatitude), color = "pink") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Pterois volitans")))



## OBIS
dori_obis <- robis::occurrence("Pterois volitans")

# checar dados
names(dori_obis)

dori_obis1 <- dori_obis %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude, bathymetry,
                flags, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat) %>% 
  distinct()

# check problemas reportados (flags)
dori_obis1 %>% 
  distinct(flags)

# check NA em datasetName
dori_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         is.na(datasetName)) %>% 
  distinct(waterBody)

# depth ok
dori_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North Pacific Ocean", "Pacific Ocean", "Indian Ocean")) %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 

# checar niveis
dori_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North Pacific Ocean", "Pacific Ocean", "Indian Ocean")) %>% 
  lapply(., unique)


# ok
dori_obis_ok <- dori_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North Pacific Ocean", "Pacific Ocean", "Indian Ocean", NA)) 

# check
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = dori_obis_ok, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Pterois volitans")))

# unir GBIF e OBIS

# ver diferencas
setdiff(names(dori_gbif_ok), names(dori_obis_ok))

setdiff(names(dori_obis_ok), names(dori_gbif_ok))

all_data <- bind_rows(dori_gbif_ok %>% 
                        mutate(repo = paste0("gbif", row.names(.))), 
                      dori_obis_ok %>% 
                        mutate(repo = paste0("obis", row.names(.)))) %>%
  column_to_rownames("repo") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, depth) %>% 
  distinct() %>% 
  rownames_to_column("occ") %>% 
  separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%
  mutate(scientificName = "Pterois volitans") %>% 
  dplyr::select(-rn)


# mapear ocorrencias
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = all_data, aes(x = decimalLongitude, y = decimalLatitude, color = datasetName)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Pterois volitans")))


