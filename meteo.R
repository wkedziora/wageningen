library(tidyverse)
library(SPEI)

o_m_1950_1955 <- read_csv("IMGW/o_m_1950.csv", 
                          col_names = FALSE, 
                          col_types = cols(X1 = col_integer(), 
                                           X3 = col_integer(), 
                                           X4 = col_integer()),
                          locale = locale(encoding = "latin2")) %>% 
  select(id = X1, name = X2, year = X3, month = X4, precip = X5) # flag = X6, kod 8 lub 9 - brak opadu

summary(o_m_1950_1955)
hist(o_m_1950_1955$precip)

wide_o_m_1950_1955 <- spread(o_m_1950_1955, key = month, value = precip)

### dla MK dane z Okęcia
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
                          
# file.list <- list.files(path = "IMGW/", pattern="\\.csv$", full.names = TRUE)
# df.list <- lapply(file.list, read_csv)

##Read files named o_m_1950_1955.csv, etc.
filenames <- list.files(path="IMGW/", pattern="o_m+.*csv", full.names = TRUE)

##Create list of data frame names without the ".csv" part 
names <-substr(filenames,1,8)

###Load all files
for(i in names){
  filepath <- file.path("IMGW/", paste0(i, ".csv"))
  assign(i, read_csv(filepath,
                     col_names = FALSE, 
                     col_types = cols(X1 = col_integer(), 
                                      X3 = col_integer(), 
                                      X4 = col_integer()),
                     locale = locale(encoding = "latin2")) %>% 
           select(id = X1, name = X2, year = X3, month = X4, precip = X5)) 
}

# load temperature as one data frame
k_m_t_1950_1955 <- read_csv("IMGW/k_m_t_1951_1955.csv", 
                          col_names = FALSE, 
                          col_types = cols(X1 = col_integer(), 
                                           X3 = col_integer(), 
                                           X4 = col_integer()),
                          locale = locale(encoding = "latin2")) %>% 
  select(id = X1, name = X2, year = X3, month = X4, temp = X5) # flag = X6, kod 8 lub 9 - brak opadu

summary(o_m_1950_1955)
hist(o_m_1950_1955$precip)

wide_o_m_1950_1955 <- spread(o_m_1950_1955, key = month, value = precip)


temp <- list.files(path="IMGW/", pattern="o_m+.*csv", full.names = TRUE) %>% 
  map_df(~read_csv(., 
                   col_names = FALSE, 
                   col_types = cols(X1 = col_integer(), 
                                    X3 = col_integer(), 
                                    X4 = col_integer()),
                   locale = locale(encoding = "latin2")) %>% 
           select(id = X1, name = X2, year = X3, month = X4, precip = X5)) %>%
  spread(., key = month, value = precip)

a <- temp %>% filter(id == 125950)

# load precipitation as one data frame
precip <- list.files(path="IMGW/", pattern="k_m_t+.*csv", full.names = TRUE) %>% 
  map_df(~read_csv(., 
                   col_names = FALSE, 
                   col_types = cols(X1 = col_integer(), 
                                    X3 = col_integer(), 
                                    X4 = col_integer()),
                   locale = locale(encoding = "latin2")) %>% 
           select(id = X1, name = X2, year = X3, month = X4, precip = X5)) %>%
  spread(., key = month, value = precip)

a <- precip %>% filter(name == "WARSZOWICE")
