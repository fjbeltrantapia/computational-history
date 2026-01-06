# Curating the data set extracted from the 1860 census for class assignments
# Using the version stored for Demography
# Keeping only a few variables
# Extracting only a random sample (25%).

library(tidyverse)

setwd("~/Library/CloudStorage/OneDrive-NTNU/course-websites/computational-history")

data <- read_csv("~/Library/CloudStorage/OneDrive-NTNU/papers-quarto/names-censo-1860/demography/data/censo_zgz_1860_clean.csv")

data <- data %>% 
  mutate(settlement = recode(settlement, 
                             "BELMONTE" = "BELMONTE DE GRACIAN", 
                             "OSERA" = "OSERA DE EBRO")) %>%
  mutate(fam_occ_cat_ext = recode(fam_occ_cat_ext, 
                                  "Artisan (skilled)" = "Artisan",
                                  "Land-owner (farmer)" = "Farmer",
                                  "Liberal occ./admin/Elites" = "Elite",
                                  "Merchant/Trader" = "Merchant",
                                  "Semi-skilled occupation" = "Semi-skilled")) %>%
  mutate(fam_occ_cat_ext = replace_na(fam_occ_cat_ext, "Missing")) #####????

data <- data %>% # transform to factor
  mutate(fam_occ_cat_ext_rank = factor(fam_occ_cat_ext, 
                                       levels = c("Labourer", "Farmer", "Tenant", "Semi-skilled", 
                                                  "Artisan", "Merchant", "Army", "Elite", "Other"), 
                                       ordered = TRUE))

data <- data %>%
  mutate(RELACION = str_trim(RELACION)) %>%
  mutate(fam_type = case_when(
    RELACION=="FAM; SEGUNDO MATRIMONIO?" ~ "OTROS",
    RELACION=="FAM; SEGUNDO MATRIMONIO" ~ "OTROS",
    RELACION=="FAM Y SEGUNDO MATRIMONIO?" ~ "OTROS",   
    RELACION=="HIJA CABEZA; HIJA ANTERIOR MATRIMONIO?" ~ "DESCENDIENTES", 
    RELACION=="HIJO ANTERIOR MATRIMONIO Y SEGUNDO MATRIMONIO?" ~ "DESCENDIENTES", 
    RELACION=="HIJO ANTERIOR MATRIMONIO? Y SEGUNDO MATRIMONIO?" ~ "DESCENDIENTES",     
    RELACION=="HIJA ANTERIOR MATRIMONIO? Y SEGUNDO MATRIMONIO?" ~ "DESCENDIENTES",      
    RELACION=="FAM DEL CABEZA; SEGUNDO MATRIMONIO" ~ "OTROS",    
    RELACION=="FAMILIAR DEL CABEZA; SEGUNDO MATRIMONIO?" ~ "OTROS",
    RELACION=="SOBRINA DEL CABEZA Y SEGUNDO MATRIMONIO?" ~ "OTROS",  
    RELACION=="HIJO DEL ANTERIOR MATRIMONIO" ~ NA,  
    RELACION=="HIJO DEL ANTERIOR" ~ NA,    
    RELACION=="NO ES LA MADRE" ~ NA,  
    RELACION=="HIJA ANTERIOR LATRIMONIO MADRE?" ~ NA,
    RELACION=="HIJA ANTERIOR MATRIVIDIO PADRE?" ~ NA,
    RELACION=="HIJO ANTERIOR MATIRMONIO PADRE?" ~ NA,
    RELACION=="HIJO ANTERIOR MATRIVIDIO PADRE?" ~ NA,  
    RELACION=="HIJA ANTERIOR MATRIMONIO MADRE" ~ NA,
    RELACION=="HIJA ANTERIOR MATRIMONIO? MADRE" ~ NA,
    RELACION=="HIJO ANTERIOR LATRIMONIO MADRE?" ~ NA,
    RELACION=="HIJO ANTERIOR MATRIMONIO MADRE" ~ NA,      
    str_detect(RELACION, "PADRE DEL") ~ "ASCENDIENTES", 
    str_detect(RELACION, "PADRE DE LA") ~ "ASCENDIENTES",     
    str_detect(RELACION, "MADRE DE LA") ~ "ASCENDIENTES",  
    str_detect(RELACION, "MADRE DEL") ~ "ASCENDIENTES", 
    str_detect(RELACION, "HIJO DEL") ~ "DESCENDIENTES",
    str_detect(RELACION, "HIJO CABEZA") ~ "DESCENDIENTES",    
    str_detect(RELACION, "HIJA DEL") ~ "DESCENDIENTES",    
    str_detect(RELACION, "NUER") ~ "DESCENDIENTES",  
    str_detect(RELACION, "HUERA DEL") ~ "DESCENDIENTES",  
    str_detect(RELACION, "NUEVA DEL") ~ "DESCENDIENTES",      
    str_detect(RELACION, "YERN") ~ "DESCENDIENTES", 
    str_detect(RELACION, "NIET") ~ "DESCENDIENTES",     
    str_detect(RELACION, "MADE DEL CAB") ~ "ASCENDIENTES",     
    str_detect(RELACION, "ABUEL") ~ "ASCENDIENTES",  
    str_detect(RELACION, "SUEGR") ~ "ASCENDIENTES", 
    RELACION=="MADRE" ~ "ASCENDIENTES",  
    RELACION=="MADRE  DEL CABEZA" ~ "ASCENDIENTES",  
    RELACION=="MADRE DE L" ~ "ASCENDIENTES",  
    RELACION=="MADRE LA ESPOSA?" ~ "ASCENDIENTES",  
    RELACION=="PADRE" ~ "ASCENDIENTES",      
    RELACION=="PADRE D LA ESPOSA" ~ "ASCENDIENTES",      
    str_detect(RELACION, "HERMAN") ~ "HORIZONTAL",
    str_detect(RELACION, "HEMAN") ~ "HORIZONTAL",   
    str_detect(RELACION, "HERNANA") ~ "HORIZONTAL",
    str_detect(RELACION, "FERMAN") ~ "HORIZONTAL",    
    str_detect(RELACION, "HEERMAN") ~ "HORIZONTAL", 
    str_detect(RELACION, "HER,MAN") ~ "HORIZONTAL",   
    str_detect(RELACION, "CUÑAD") ~ "HORIZONTAL",  
    str_detect(RELACION, "CUNAD") ~ "HORIZONTAL",  
    str_detect(RELACION, "HUESP") ~ NA, 
    str_detect(RELACION, "TRABAJA") ~ NA,     
    RELACION=="OTRO" ~ NA,   
    RELACION=="EXPOSITO" ~ NA,       
    RELACION=="SIR" ~ NA,  
    RELACION=="SIRV" ~ NA,       
    RELACION=="SIRVIENTA" ~ NA,  
    RELACION=="SIRVIENTE" ~ NA,
    str_detect(RELACION, "MATRIMONIO") ~ NA, 
    str_detect(RELACION, "MATIRMONIO") ~ NA, 
    str_detect(RELACION, "MATRIMOMIO") ~ NA, 
    str_detect(RELACION, "PATRIMONIO") ~ NA, 
    str_detect(RELACION, "FATRIFONIO") ~ NA,     
    RELACION=="HIJA DE LA OTRA SIRVIENTA?" ~ NA,   
    RELACION=="HIJA DE LA ESPOSA" ~ NA,   
    RELACION=="HIJO DE ESPOSA ANTERIOR?" ~ NA,  
    RELACION=="HIJO DE P2" ~ NA,  
    RELACION=="OTRO; HIJA DE P2" ~ NA,      
    RELACION=="HIJA DE P2V" ~ NA, 
    RELACION=="HIJA DE LA VIUDA" ~ NA, 
    RELACION=="HIJO DE ANTERIOR" ~ NA,   
    is.na(RELACION) ~ NA,
    TRUE ~ "OTROS")) %>%
  mutate(fam_type = replace(fam_type, is.na(fam_type) & POS=="FAM", "OTROS")) %>%
  # mutate(fam_type = replace(fam_type, is.na(fam_type) & POS=="FAM SIRV", "OTROS")) %>%  
  mutate(asc = if_else(fam_type=="ASCENDIENTES", 1, 0)) %>%
  mutate(desc = if_else(fam_type=="DESCENDIENTES", 1, 0)) %>% 
  mutate(hor = if_else(fam_type=="HORIZONTAL", 1, 0)) %>%
  mutate(otr = if_else(fam_type=="OTROS", 1, 0)) %>%
  group_by(hh_id) %>%
  mutate(asc = sum(asc, na.rm = TRUE),
         desc = sum(desc, na.rm = TRUE),
         hor = sum(hor, na.rm = TRUE),
         otr = sum(otr, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ext_fam = case_when( 
    asc==0 & desc==0 & hor==0 & otr==0 ~ 0,      
    TRUE ~ 1)) %>%
  mutate(mult_fam = case_when(
    asc==0 & desc==0 ~ 0,      
    TRUE ~ 1)) %>%  
  mutate(asc = if_else(asc>0, 1, 0)) %>%
  mutate(desc = if_else(desc>0, 1, 0)) %>%
  mutate(oth = if_else(hor>0 | otr>0, 1, 0))

data <- data |>
  select(settlement, munic, household, hous_size, indiv, 
         name, surname1, surname2, sex, age, marst, 
         fam_occ_cat_ext, POS, RELACION, occ, write, read, school,
         fam_id, parity, father, mother, munic_gis) |>
  rename(ses = fam_occ_cat_ext)


# Spatial data - Shapefiles
# So I can select the sample of settlements within 40 kms. of Zgz
library(sf)

pop <- data %>%
  group_by(settlement) %>%
  summarise(pop = n())

  # settlements shapefile
settl_pt_shp <- read_sf("~/Library/CloudStorage/OneDrive-NTNU/papers-quarto/names-censo-1860/demography/data/gis_settlements/Nucleos_Zgz_pt.shp") %>%
  mutate(d_nucleo_i = str_to_upper(d_nucleo_i)) %>%
  right_join(pop, by = join_by(d_nucleo_i == settlement)) %>%
  rename(settlement = d_nucleo_i) %>%
  select(settlement)

# Identify settlements within 40 kms. from Zaragoza.
zgz <- settl_pt_shp |>
  filter(settlement == "ZARAGOZA")

settl_pt_shp <- settl_pt_shp |>
  mutate(d_zgz = as.numeric(st_distance(geometry, zgz))) %>%
  mutate(d_zgz = d_zgz/1000)

sample <- settl_pt_shp |>
  filter(d_zgz<=40) # sample with 78 locations (within 40 kms. from Zgz)

library(tmap)
settl_pt_shp |>
  tm_shape() + tm_dots("grey80") +
  tm_shape(sample) + tm_dots() +
  tm_shape(zgz) + tm_dots("red")


# Extract the sample of settlements in the sample

data_40 <- data |>
  full_join(sample, by = join_by(settlement)) |>
  filter(!is.na(d_zgz))

data_40 |> count(settlement) |> print(n = Inf)
  # 132,500 obs.
  # 70 munic. (78 settlements)



# Store the data set (sample) 
write_csv(data_40, "data/zgz-1860/sample-zgz-1860.csv")

# Store the shapefiles
munic_sample <- data_40 |> count(munic_gis)

munic_sample_shp <- read_sf("~/Library/CloudStorage/OneDrive-NTNU/papers-quarto/names-censo-1860/demography/data/gis_munic_313/Munic_1860.shp") %>%
  left_join(munic_sample, by = join_by(NAMEUNIT == munic_gis)) %>%
  filter(!is.na(n))

esp_shp <- read_sf("~/Library/CloudStorage/OneDrive-NTNU/papers-quarto/names-censo-1860/demography/data/adm0/ESP_adm0_pr_peninsula.shp")
zgz_shp <- read_sf("~/Library/CloudStorage/OneDrive-NTNU/papers-quarto/names-censo-1860/demography/data/adm2/ESP_adm2.shp") %>% filter(NAME_2=="Zaragoza")

tm_shape(zgz_shp) + tm_polygons() +
  tm_shape(munic_sample_shp) + tm_polygons() +
  tm_shape(settl_pt_shp) + tm_dots("grey80") +
  tm_shape(sample) + tm_dots() +
  tm_shape(zgz) + tm_dots("red")

write_sf(sample, "data/zgz-1860/gis/sample_40.shp")
write_sf(zgz, "data/zgz-1860/gis/zgz_cap.shp")
write_sf(esp_shp, "data/zgz-1860/gis/spain.shp")
write_sf(zgz_shp, "data/zgz-1860/gis/zgz_prov.shp")




zaragoza <- settl_pt_shp |>
  filter(settlement == "ZARAGOZA")

munic_shp <- read_sf("~/Library/CloudStorage/OneDrive-NTNU/papers-quarto/names-censo-1860/demography/data/gis_munic_313/Munic_1860.shp")

munic <- data %>%
  group_by(munic_gis) %>%
  summarise(pop = n())

missing_shp <- read_sf("~/Library/CloudStorage/OneDrive-NTNU/papers-quarto/names-censo-1860/demography/data/gis_munic_313/Munic_1860.shp") %>%
  left_join(munic, by = join_by(NAMEUNIT == munic_gis)) %>% filter(is.na(pop))


m1 <- tm_shape(esp_shp) + tm_borders("grey90") +
  tm_shape(zgz_shp) + tm_borders("grey70")

m2 <- tm_shape(munic_shp) + tm_borders(col = "grey90") +
  tm_shape(missing_shp) + tm_fill(col = "grey90") +
  tm_shape(munic_pt_shp) +
  tm_bubbles(fill = "blue", fill_alpha = 0.5, col = "blue", 
             size = "pop",
             size.scale = tm_scale_continuous(
               ticks = c(250, 500, 1000, 5000, 10000, 20000, 50000, 100000),
               labels = c("0-250", "250-500", "500-1,000", "1,000-5,000", 
                          "5,000-10,000", "10,000-20,000", "20,000-50,000", 
                          "+50,000")),
             size.legend = tm_legend(title = "",
                                     frame = FALSE,
                                     orientation = "portrait",
                                     position = tm_pos_in("right", "top"))) +
  tmap_options(legend.text.fontfamily = "Times")

library(grid)
m2
print(m1, vp = grid::viewport(0.275, 0.86, width = 0.22, height = 0.22))