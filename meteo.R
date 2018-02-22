library(tidyverse)
library(SPEI)

# dla stacji Ostrów Mazowiecka (OMPS) posterunek opadowy położony najbliżej to:
# Basinów (251210270 - 18 km), Rybienko (252210290 - 20 km) i Wąsewo (252210030 - 20 km)
# pomiary temperatury odległość:
# Pułtusk (152219997 oraz 252210050 - 43 km)
# 
# dla lokalizacji Głęboki Bród (GBPS): 
# Suwałki (354220195 - 25 km)
  
# temperature ------------------------------------------------------------------------------------
k_m_t_temp <- list.files(path="IMGW/", pattern="k_m_t+.*csv", full.names = TRUE) %>% 
  map_df(~read_csv(., 
                   col_names = FALSE, 
                   col_types = cols(X1 = col_integer(), 
                                    X3 = col_integer(), 
                                    X4 = col_integer()),
                   locale = locale(encoding = "latin2")) %>% 
           select(id = X1, name = X2, year = X3, month = X4, temp = X5)) %>%
  spread(., key = month, value = temp)

k_m_d_temp <- list.files(path="IMGW/", pattern="k_m_d+.*csv", full.names = TRUE) %>% 
  map_df(~read_csv(., 
                   col_names = FALSE, 
                   col_types = cols(X1 = col_integer(), 
                                    X3 = col_integer(), 
                                    X4 = col_integer()),
                   locale = locale(encoding = "latin2")) %>% 
           select(id = X1, name = X2, year = X3, month = X4, temp = X13)) %>%
  spread(., key = month, value = temp)

s_m_d_temp <- list.files(path="IMGW/", pattern="s_m_d+.*csv", full.names = TRUE) %>% 
  map_df(~read_csv(., 
                   col_names = FALSE, 
                   col_types = cols(X1 = col_integer(), 
                                    X3 = col_integer(), 
                                    X4 = col_integer()),
                   locale = locale(encoding = "latin2")) %>% 
           select(id = X1, name = X2, year = X3, month = X4, temp = X13)) %>%
  spread(., key = month, value = temp)

# temp for Suwałki (Głęboki Bród):
k_m_t_temp_suwalki <- k_m_t_temp %>% filter(id == 354220195)
# k_m_d_temp_suwalki <- k_m_d_temp %>% filter(id == 354220195)
s_m_d_temp_suwalki <- s_m_d_temp %>% filter(id == 354220195)
temp_suwalki <- bind_rows(k_m_t_temp_suwalki[1:10, ], s_m_d_temp_suwalki)

# temp for Pułtusk (Ostrów Mazowiecka):
temp_pultusk <- k_m_t_temp %>% filter(id == 252210050)
# k_m_d_temp_pultusk <- k_m_d_temp %>% filter(id == 252210050)

# precipitation ----------------------------------------------------------------------------------
# load precipitation as one data frame
s_m_d_prcp <- list.files(path="IMGW/", pattern="s_m_d+.*csv", full.names = TRUE) %>% 
  map_df(~read_csv(., 
                   col_names = FALSE, 
                   col_types = cols(X1 = col_integer(), 
                                    X3 = col_integer(), 
                                    X4 = col_integer()),
                   locale = locale(encoding = "latin2")) %>% 
           select(id = X1, name = X2, year = X3, month = X4, prcp = X17)) %>%
  spread(., key = month, value = prcp)

k_m_d_prcp <- list.files(path="IMGW/", pattern="k_m_d+.*csv", full.names = TRUE) %>% 
  map_df(~read_csv(., 
                   col_names = FALSE, 
                   col_types = cols(X1 = col_integer(), 
                                    X3 = col_integer(), 
                                    X4 = col_integer()),
                   locale = locale(encoding = "latin2")) %>% 
           select(id = X1, name = X2, year = X3, month = X4, prcp = X17)) %>%
  spread(., key = month, value = prcp)

o_m <- list.files(path="IMGW/", pattern="o_m+.*csv", full.names = TRUE) %>% 
  map_df(~read_csv(., 
                   col_names = FALSE, 
                   col_types = cols(X1 = col_integer(), 
                                    X3 = col_integer(), 
                                    X4 = col_integer()),
                   locale = locale(encoding = "latin2")) %>% 
           select(id = X1, name = X2, year = X3, month = X4, prcp = X5)) %>%
  spread(., key = month, value = prcp)

# precipitation for Suwałki (Głęboki Bród):
k_m_d_prcp_suwalki <- k_m_d_prcp %>% filter(id == 354220195)
s_m_d_prcp_suwalki <- s_m_d_prcp %>% filter(id == 354220195)
prcp_suwalki <- bind_rows(k_m_d_prcp_suwalki[1:10, ], s_m_d_prcp_suwalki)

# precipitation for Wąsewo (Ostrów MAzowiecka)
prcp_wasewo <- o_m %>% filter(id == 252210030)

# Data Glueing -------------------------------------------------------------------------------
colnames(temp_suwalki) <- 
  c("id", "names", "year", "T.1", "T.2", "T.3", "T.4", "T.5", "T.6", "T.7", "T.8", "T.9", "T.10", "T.11", "T.12")
colnames(prcp_suwalki) <- 
  c("id", "names", "year", "P.1", "P.2", "P.3", "P.4", "P.5", "P.6", "P.7", "P.8", "P.9", "P.10", "P.11", "P.12")
# climate station Suwalki, 25 km to GBPS
GBPS_climate <- bind_cols(temp_suwalki[, 3:15], prcp_suwalki[, 4:15])
write_csv(GBPS_climate, file.path(getwd(), "/IMGW/GBPS_climate.csv"))

colnames(temp_pultusk) <- 
  c("id", "names", "year", "T.1", "T.2", "T.3", "T.4", "T.5", "T.6", "T.7", "T.8", "T.9", "T.10", "T.11", "T.12")
colnames(prcp_wasewo) <- 
  c("id", "names", "year", "P.1", "P.2", "P.3", "P.4", "P.5", "P.6", "P.7", "P.8", "P.9", "P.10", "P.11", "P.12")
# temp station Pułtusk (43 km) and precipitation station Wąsewo (20 km)
OMPS_climate <- bind_cols(temp_pultusk[, 3:15], prcp_wasewo[, 4:15])
write_csv(OMPS_climate, file.path(getwd(), "/IMGW/OMPS_climate.csv"))

###############################################################################################
### dla Marcina Klisza z IBLu - dane z Okęcia -------------------------------------------------
okecie <- read_delim("NOAA/NOAA_125950_OKECIE.csv", delim = ";")

okecie %>%
  replace_na(list(prcp_mm = 0)) %>%
  mutate(., month = substr(okecie$vdate, 4, 5),
         year = substr(okecie$vdate, 7, 10)) %>%
  group_by(year, month) %>%
  summarise(temp = mean(temp_c),
            prec = sum(prcp_mm)) -> okecie_monthly

okecie_monthly$PET <- thornthwaite(okecie_monthly$temp, 52.169756)

plot(okecie_monthly$prec ~ okecie_monthly$year)

spei3 <- spei(okecie_monthly$prec - okecie_monthly$PET, 3)

myts <- ts(spei3$fitted, start=1940-03, frequency=12)

### zbyt krótki okres danych z Okęcia (dziury)