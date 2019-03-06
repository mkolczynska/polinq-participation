
### PACKAGES ---------------

#devtools::install_github("xmarquez/vdem")
library(vdem)
#devtools::install_github("xmarquez/democracyData")
library(democracyData)
library(WDI)
library(countrycode) # converting country codes
library(xlsx)
library(tidyverse)
library(readxl)
library(data.table)


### V-DEM ---------------------

vdem.part <- extract_vdem(name_pattern = "v2x_partipdem", include_uncertainty = FALSE) %>%
  select(iso3 = vdem_country_text_id, year, vdem_par = v2x_partipdem) %>%
  mutate(year = as.numeric(year)) %>%
  drop_na(iso3, year, vdem_par)


### SWIID -------------------

swiid <- read.csv("https://raw.githubusercontent.com/fsolt/swiid/master/data/swiid7_1_summary.csv",
                  stringsAsFactors = FALSE, encoding = "UTF-8") %>%
  mutate(iso3 = countrycode(country, "country.name", "iso3c")) %>%
  mutate(iso3 = ifelse(country == "Kosovo", "XKX", iso3),
         iso3 = ifelse(country == "Czechoslovakia", "CSK", iso3),
         iso3 = ifelse(country == "Soviet Union", "SUN", iso3),
         iso3 = ifelse(country == "Yugoslavia", "YUG", iso3)) %>%
  select(iso3, year, gini_disp) %>%
  drop_na(iso3, year, gini_disp)


### POLYARCHY ------------------

polyarchy <- read.csv2("https://www.prio.org/Global/upload/CSCW/Data/Governance/file42534_polyarchy_v2.csv",
                       stringsAsFactors = FALSE) %>%
  mutate(iso3 = countrycode(Abbr, "cowc", "iso3c")) %>%
  select(iso3, year = Year, polyarch_part = Part) %>%
  mutate(year = as.numeric(year),
         polyarch_part = as.numeric(polyarch_part)) %>%
  drop_na(iso3, year, polyarch_part)


### FREEDOM HOUSE - PARTICIPATION -----------------------

myurl <- "https://freedomhouse.org/sites/default/files/Aggregate%20Category%20and%20Subcategory%20Scores%20FIW2003-2018.xlsx"
td = tempdir()
tmp <- tempfile(tmpdir = td, fileext = ".xlsx")
download.file(url = myurl, destfile = tmp, mode="wb")
excel_sheets(tmp)

names(read.xlsx(file = tmp, sheetIndex = 2))


fh.list <- list()
for (i in 1:13) {
  fh.list[[i]] <- read.xlsx(file = tmp, sheetIndex = i+1, colIndex = c(1,6))
  fh.list[[i]]$Country.Territory <- gsub("[*].*$","",fh.list[[i]]$Country.Territory)
  fh.list[[i]]$year <- 2018 - i + 1
}

fh <- do.call("rbind", fh.list) %>%
  mutate(iso3 = countrycode(Country.Territory, "country.name", "iso3c")) %>%
  mutate(iso3 = ifelse(Country.Territory %in% c("Kosovo", "Kosovo*"), "XKX", iso3)) %>%
  filter(!(Country.Territory %in% c("Israeli Occupied Territories",
                                    "Northern Cyprus",
                                    "Pakistani Kashmir",
                                    "Indian Kashmir",
                                    "Somaliland"))) %>%
  select(iso3, year, fh_B_aggr = B.Aggr) %>%
  filter(iso3 != "PSE") %>%
  drop_na(iso3, year, fh_B_aggr)


### FREEDOM HOUSE - STATUS --------------------

fh_status <- download_fh(verbose = FALSE) %>%
  filter(!(fh_country %in% c("Northern Cyprus", "South Vietnam",
                             "Vietnam, N."))) %>%
  mutate(iso3 = countrycode(fh_country, "country.name", "iso3c")) %>%
  mutate(iso3 = ifelse(fh_country %in% c("Kosovo", "Kosovo*"), "XKX", iso3),
         iso3 = ifelse(fh_country == "East Germany", "DDR", iso3),
         iso3 = ifelse(fh_country == "Czechoslovakia", "CSK", iso3),
         iso3 = ifelse(fh_country == "Soviet Union", "SUN", iso3),
         iso3 = ifelse(fh_country == "Yugoslavia", "YUG", iso3),
         iso3 = ifelse(fh_country == "Yugoslavia (Serbia & Montenegro)", "SCG", iso3)) %>%
  select(iso3, year, fh_status = status, fh_total, fh_cl = cl, fh_pr = pr) %>%
  drop_na(iso3, year, fh_status, fh_cl, fh_pr)


### POLITY IV ----------------------

# a <- haven::read_sav("http://www.systemicpeace.org/inscr/p4v2017.sav") %>%
#   mutate(iso3 = countrycode(country, "country.name", "iso3c")) %>%
#   mutate(iso3 = ifelse(country == "Kosovo", "XKX", iso3)) %>%
#   select(iso3, year, p4_polcomp = polcomp) %>%
#   mutate(p4_polcomp = ifelse(p4_polcomp %in% c(-66, -77, -88), NA, p4_polcomp))


myurl <- "http://www.systemicpeace.org/inscr/p4v2017.xls"
td = tempdir()
tmp <- tempfile(tmpdir = td, fileext = ".xls")
download.file(url = myurl, destfile = tmp, mode="wb")
excel_sheets(tmp)

polity <- readxl::read_excel(tmp) %>%
  filter(year >= 1945) %>%
  mutate(iso3 = countrycode(country, "country.name", "iso3c")) %>%
  mutate(iso3 = ifelse(country == "Kosovo", "XKX", iso3),
         iso3 = ifelse(country == "Czechoslovakia", "CSK", iso3),
         iso3 = ifelse(country == "Germany East", "DDR", iso3),
         iso3 = ifelse(country == "Yugoslavia", "YUG", iso3),
         iso3 = ifelse(country == "Soviet Union", "SUN", iso3)) %>%
  filter(!(country %in% c("Yemen South", "Yemen North", 
                          "Vietnam South", "Vietnam North",
                          "Sudan", "Sudan-North"))) %>%
  select(iso3, year, p4_polcomp = polcomp) %>%
  mutate(p4_polcomp = ifelse(p4_polcomp %in% c(-66, -77, -88), NA, p4_polcomp)) %>%
  group_by(iso3, year) %>%
  summarise(p4_polcomp = mean(p4_polcomp)) %>%
  drop_na(iso3, year, p4_polcomp)


### DEMOCRACY BAROMETER -------------

myurl <- "http://www.democracybarometer.org/Data/DB_data_1990-2016_Standardized.xls"
td = tempdir()
tmp <- tempfile(tmpdir = td, fileext = ".xls")
download.file(url = myurl, destfile = tmp, mode="wb")
excel_sheets(tmp)

db <- readxl::read_excel(tmp, skip = 4) %>%
  mutate(iso3 = countrycode(`Ccode QOG`, "iso3n", "iso3c")) %>%
  select(iso3, year = Year, db_PARTICIP = PARTICIP) %>%
  mutate(db_PARTICIP = as.numeric(db_PARTICIP)) %>%
  drop_na(iso3, year, db_PARTICIP)


### POVERTY (WORLD BANK) ----------------------

poverty <- WDI(country="all", indicator=c("SI.POV.NAHC"),
    start=1900, end=2018, extra=TRUE, cache=NULL) %>%
  select(iso3 = iso3c, year, wb_poverty = SI.POV.NAHC) %>%
  drop_na(iso3, year, wb_poverty)


### MERGE ALL DATA ----------------------------

merged <- full_join(db, fh, by = c("iso3", "year")) %>%
  full_join(fh_status, by = c("iso3", "year")) %>%
  full_join(polity, by = c("iso3", "year")) %>%
  full_join(polyarchy, by = c("iso3", "year")) %>%
  full_join(swiid, by = c("iso3", "year")) %>%
  full_join(vdem.part, by = c("iso3", "year")) %>%
  full_join(poverty, by = c("iso3", "year")) %>%
  mutate(country = countrycode(iso3, "iso3c", "country.name"),
         country = ifelse(iso3 == "XKX", "Kosovo", country),
         country = ifelse(iso3 == "DDR", "East Germany", country),
         country = ifelse(iso3 == "YUG", "Yugoslavia", country),
         country = ifelse(iso3 == "CSK", "Czechoslovakia", country),
         country = ifelse(iso3 == "SCG", "Serbia&Mongenegro", country),
         country = ifelse(iso3 == "SUN", "Soviet Union", country)) %>%
  filter(!is.na(iso3), !is.na(country))


### WRITE FILE TO DISC -------------------

fwrite(merged, "merged-20190306.csv")


### CORRELATIONS -----------------------

cor(merged[merged$year > 2014 , c("db_PARTICIP", "vdem_par", "fh_B_aggr", "p4_polcomp", 
                                  "polyarch_part", "gini_disp", "wb_poverty")],
    use = "pairwise.complete.obs")

cor(merged[merged$year == 2014 , c("db_PARTICIP", "vdem_par", "fh_B_aggr", "p4_polcomp", 
                                  "polyarch_part")],
    use = "pairwise.complete.obs")


### PLOTS --------------------

merged %>%
  filter(year >= 2014 & year <= 2014) %>%
  ggplot(., aes(x = p4_polcomp, y = vdem_par)) +
  geom_point() +
  geom_smooth(method='lm')


### WORLD INEQUALITY DATABASE ----------------

#devtools::install_github("WIDworld/wid-r-tool")
library("wid")

### more info: 
### https://github.com/WIDworld/wid-r-tool
### vignette("wid-demo")

wid <- download_wid(
  indicators = "shweal",
  areas = "all",
  years = "all",
  perc = "all"
)



