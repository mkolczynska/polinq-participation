
devtools::install_github("xmarquez/vdem")
library(vdem)
library(WDI)
library(countrycode) # converting country codes
library(xlsx)
library(tidyverse)
library("readxl")
library(readstata13)
library(data.table)

vdem.part <- extract_vdem(name_pattern = "v2x_partipdem", include_uncertainty = FALSE) %>%
  select(iso3 = vdem_country_text_id, year, v2x_partipdem) %>%
  mutate(year = as.numeric(year))

swiid <- read.csv("https://raw.githubusercontent.com/fsolt/swiid/master/data/swiid7_1_summary.csv",
                  stringsAsFactors = FALSE) %>%
  mutate(iso3 = countrycode(country, "country.name", "iso3c")) %>%
  mutate(iso3 = ifelse(country == "Kosovo", "XKX", iso3)) %>%
  select(iso3, year, gini_disp) %>%
  mutate(year = as.numeric(year))

# Czechoslovakia, Micronesia, SÃ£o TomÃ© and PrÃ�ncipe, Yugoslavia

polyarchy <- read.csv2("https://www.prio.org/Global/upload/CSCW/Data/Governance/file42534_polyarchy_v2.csv",
                       stringsAsFactors = FALSE) %>%
  mutate(iso3 = countrycode(Abbr, "cowc", "iso3c")) %>%
  select(iso3, year = Year, polyarch_part = Part) %>%
  mutate(year = as.numeric(year),
         polyarch_part = as.numeric(polyarch_part))

# fh <- read.xlsx("https://freedomhouse.org/sites/default/files/Aggregate%20Category%20and%20Subcategory%20Scores%20FIW2003-2018.xlsx",
#                 sheetIndex = 1)

fh <- read.xlsx("Freedom House.xlsx",
                sheetIndex = 3) %>%
  gather(year, fh_B_aggr, 2:14) %>%
  mutate(year = substr(year, 2, 5)) %>%
  mutate(iso3 = countrycode(Country.Territory, "country.name", "iso3c")) %>%
  mutate(iso3 = ifelse(Country.Territory %in% c("Kosovo", "Kosovo*"), "XKX", iso3),
         year = as.numeric(year)) %>%
  select(iso3, year, fh_B_aggr)

polity <- read_excel("p4v2017.xls", 
                   sheet= 1) %>%
  mutate(iso3 = countrycode(country, "country.name", "iso3c")) %>%
  mutate(iso3 = ifelse(country == "Kosovo", "XKX", iso3)) %>%
  select(iso3, year, p4_polcomp = polcomp) %>%
  mutate(year = as.numeric(year),
         p4_polcomp = ifelse(p4_polcomp %in% c(-66, -77, -88), NA, p4_polcomp))

db <- read.csv("DB_data_1990-2016_Standardized_x.csv",
                stringsAsFactors = FALSE) %>%
  mutate(iso3 = countrycode(Ccode.QOG, "iso3n", "iso3c")) %>%
  select(iso3, year = Year, db_PARTICIP = PARTICIP) %>%
  mutate(year = as.numeric(year),
         db_PARTICIP = as.numeric(db_PARTICIP))


table(polity$p4_polcomp)






# vdem.wgi <- extract_vdem(name_pattern = "wbgi", include_uncertainty = FALSE, include_external = TRUE) %>%
#   dplyr::select(1, 4, 13, 14, 16, 18, 20, 22, 24)
# fh <- download_fh(verbose = FALSE)



poverty <- WDIsearch(string = "poverty", 
                   field = "name", short = TRUE, cache = NULL) %>%
  as.data.frame()




poverty <- WDI(country="all", indicator=c("SI.POV.NAHC"),
    start=1900, end=2018, extra=TRUE, cache=NULL)

poverty1 <- poverty %>%
  filter(!is.na(SI.POV.NAHC)) %>%
  select(iso3 = iso3c, year, wb_poverty = SI.POV.NAHC)


merged <- full_join(db, fh) %>%
  full_join(polity) %>%
  full_join(polyarchy) %>%
  full_join(swiid) %>%
  full_join(vdem.part) %>%
  full_join(poverty1) %>%
  mutate(country = countrycode(iso3, "iso3c", "country.name"))


cor(merged[merged$year > 2014 , c("db_PARTICIP", "v2x_partipdem", "fh_B_aggr", "p4_polcomp", 
                                  "polyarch_part", "gini_disp", "wb_poverty")],
    use = "pairwise.complete.obs")

cor(merged[merged$year == 2014 , c("db_PARTICIP", "v2x_partipdem", "fh_B_aggr", "p4_polcomp", 
                                  "polyarch_part")],
    use = "pairwise.complete.obs")

merged %>%
  filter(year >= 2014 & year <= 2014) %>%
  ggplot(., aes(x = p4_polcomp, y = v2x_partipdem)) +
  geom_point() +
  geom_smooth(method='lm')

fwrite(merged, "merged-20190201.csv")
