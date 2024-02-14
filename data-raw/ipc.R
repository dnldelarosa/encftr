library(dplyr)
library(tidyr)

ipc_2010 <- readxl::read_excel("G:/My Drive/Databases/ENCFT/pobrezaMonetaria/2020/Pobreza/ipc_base_2010.xls", sheet = "IPCbase 2010", col_names = F, skip = 7) %>%
  drop_na(...2) %>%
  fill(...1, .direction = "down") %>%
  rename("ANO" = "...1", "MES" = "...2", "IPCbase2010" = "...3") %>%
  select(ANO, MES, IPCbase2010) %>%
  mutate (ANO = as.double(ANO),
          MES = recode(MES,
                       "Enero" = "01", "Febrero" = "02", "Marzo" = "03", "Abril" = "04", "Mayo" = "05", "Junio" = "06",
                       "Julio" = "07", "Agosto" = "08", "Septiembre" = "09", "Octubre" = "10", "Noviembre" = "11", "Diciembre" = "12"),
          IPCbase2010=as.double(IPCbase2010)) %>%
  mutate(IPCcentral = case_when(
    MES == "02" ~ IPCbase2010,
    MES == "05" ~ IPCbase2010,
    MES == "08" ~ IPCbase2010,
    MES == "11" ~ IPCbase2010
  ),
  IPCcentral = lead(IPCcentral),
  PERIODO = as.numeric(paste(ANO, MES, sep="")),
  IPCanterior = lag(IPCbase2010),
  IPCprom = (lag(IPCbase2010)+ lag(IPCbase2010,2)+ lag(IPCbase2010,3)+ lag(IPCbase2010,4)+ lag(IPCbase2010,5)+
               lag(IPCbase2010,6))/6
  ) %>%
  fill(IPCcentral) %>%
  filter(PERIODO >= 201601) %>%
  mutate (MES = as.double(MES)) %>%
  select(-c(ANO, MES))

usethis::use_data(ipc_2010, overwrite = TRUE)





ipc_2020 <- readxl::read_excel("G:/My Drive/Databases/ENCFT/pobrezaMonetaria/2022/ipc_base_2019-2020.xls", col_names = F, skip = 7) %>%
  drop_na(...2) %>%
  fill(...1, .direction = "down") %>%
  rename("ANO" = "...1", "MES" = "...2", "IPCbase2020" = "...3") %>%
  select(ANO, MES, IPCbase2020) %>%
  mutate (ANO = as.double(ANO),
          MES = recode(MES,
                       "Enero" = "01", "Febrero" = "02", "Marzo" = "03", "Abril" = "04", "Mayo" = "05", "Junio" = "06",
                       "Julio" = "07", "Agosto" = "08", "Septiembre" = "09", "Octubre" = "10", "Noviembre" = "11", "Diciembre" = "12"),
          IPCbase2020=as.double(IPCbase2020)) %>%
  mutate(IPCcentral = case_when(
    MES == "02" ~ IPCbase2020,
    MES == "05" ~ IPCbase2020,
    MES == "08" ~ IPCbase2020,
    MES == "11" ~ IPCbase2020
  ),
  IPCcentral = lead(IPCcentral),
  PERIODO = as.numeric(paste(ANO, MES, sep="")),
  IPCanterior = lag(IPCbase2020),
  IPCprom = (lag(IPCbase2020)+ lag(IPCbase2020,2)+ lag(IPCbase2020,3)+ lag(IPCbase2020,4)+ lag(IPCbase2020,5)+
               lag(IPCbase2020,6))/6
  ) %>%
  fill(IPCcentral) %>%
  filter(PERIODO >= 201601) %>%
  mutate (MES = as.double(MES)) %>%
  select(-c(ANO, MES))

usethis::use_data(ipc_2020, overwrite = TRUE)
