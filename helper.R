# 1 Libraries ----

library(tidyverse)
library(countrycode)
library(ggthemes)
library(imfr)
library(zoo)
library(plotly)
library(rgeos)
library(gifski)   
library(shiny)
library(kableExtra)
library(scales)
library(shiny)



# 2 Data ----

#* 2.1 Variables of Interest ----

vbles <- imf_codes(codelist = "CL_INDICATOR_IFS") %>%
  as.data.frame() %>%
  filter(description %in% c("Exchange Rates, Domestic Currency per U.S. Dollar, End of Period, Rate",
                            "Total International Reserves, US Dollars (gold at 35 SDRs per ounce)")) %>%
  .$codes %>%
  as.character()

#* 2.2 Download Data ----

ierp <- imf_data(database_id = "IFS" , indicator = vbles , country = "all", start = 1950, end = current_year(), freq = "M") %>%
  mutate(year = as.numeric(substr(year_month, 1, 4)),
         month = as.numeric(substr(year_month, 6, 7))) %>%
  rename(code = iso2c,
         e = ENDE_XDC_USD_RATE,
         fx = RAFA_G_USD) %>%
  select(code, year, month, e, fx)

# saveRDS(object = ierp, file = "./ierp.RDS")

# ierp <- readRDS("./ierp.RDS")

#* 2.3 Create list of countries ----

ierp2 <- ierp %>%
  filter(year > 1950) %>%
  group_by(code) %>%
  mutate(e_sd = rollapply(data = e, width = 12,
                          FUN = function(x) sd(x, na.rm = TRUE), partial = TRUE, align = "right"),
         fx_sd = rollapply(data = fx, width = 12,
                           FUN = function(x) sd(x, na.rm = TRUE), partial = TRUE, align = "right"),
         e_chg = e/lag(e)-1,
         fx_chg = fx/lag(fx)-1) %>%
  drop_na(e_chg, fx_chg) %>%
  mutate(erpi = e_chg - e_sd/fx_sd * fx_chg,
         # ERPI and triggers
         erpi_mean = rollapply(data = erpi, width = 60,
                               FUN = function(x) mean(x, na.rm = TRUE), partial = TRUE, align = "right"),
         erpi_sd = rollapply(data = erpi, width = 60,
                             FUN = function(x) sd(x, na.rm = TRUE), partial = TRUE, align = "right"),
         trigger = erpi_mean + 3*erpi_sd,
         # Currency Crisis
         above_trigger = ifelse(erpi > trigger, 1, 0),
         acc_above_trigger = rollapply(data = above_trigger, width = 6,
                                       FUN = function(x) sum(x, na.rm = TRUE), partial = TRUE, align = "right"),
         cc = ifelse((erpi > 0.1 & lag(acc_above_trigger) == 0 & above_trigger == 1), 1, 0),
         cc = ifelse(is.na(cc), 0, cc),
         acc_cc = cumsum(cc),
         # For plots
         cc_plot = ifelse((erpi > 0.1 & lag(acc_above_trigger) == 0 & above_trigger == 1), (max(erpi) + .01), NA),
         acc_cc_plot = ifelse(cc == 1, acc_cc, NA)
  ) %>%
  ungroup() %>%
  filter(code != "AN") %>%
  mutate(date = as.Date(as.yearmon(year) + (month-1)/12, frac = 0),
         code = countrycode(code, origin = "iso2c", destination = "iso3c"),
         country = countrycode(code, origin = "iso3c", destination = "country.name")) %>%
  inner_join(select(countrycode::codelist, iso3c, continent, region),
             by = c("code" = "iso3c")) %>%
  select(code, country, region, continent, date, everything()) %>%
  drop_na(code)

country_list <- ierp2$country %>%
  unique()
