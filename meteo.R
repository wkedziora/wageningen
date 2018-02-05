library(tidyverse)

o_m_1950_1955 <- read_csv("IMGW/o_m_1950_1955.csv", 
                          col_names = FALSE, 
                          col_types = cols(X1 = col_integer(), 
                                           X3 = col_integer(), 
                                           X4 = col_integer()),
                          locale = locale(encoding = "latin2")) %>% 
  select(id = X1, name = X2, year = X3, month = X4, precip = X5) # flag = X6, kod 8 lub 9 - brak opadu

summary(o_m_1950_1955)
hist(o_m_1950_1955$precip)

wide_o_m_1950_1955 <- spread(o_m_1950_1955, key = month, value = precip)

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

a <- temp %>% filter(id == 354220195)

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
