### Do ESG leaders outperform ESG laggards? 
# R Code for Applied Portfolio Management and Sustainability

library(readxl)
library(dplyr)
library(data.table)
library(DescTools)
library(readr)
library(writexl)
library(stringi)
library(tidyr)
library(stargazer)
library(texreg)
library(lmtest)
library(sandwich)

### Preprocessing
## Import of Datasets
# manually for 2007
ESG_2007 <- read_excel("C:/Users/Anwender/OneDrive/Studium/10. Semester/Portfolio Management/Daten von Fabio/ESG Ratings Timeseries Expanded 2007 to 2012.xlsx")

ESG_2007$AS_OF_DATE <- as.Date(ESG_2007$AS_OF_DATE, "%Y%m%d")

ESG_2007 <- ESG_2007 %>% mutate(YEAR = format(AS_OF_DATE, "%Y"), MONTH = format(AS_OF_DATE, "%m"), QUARTER = quarter(AS_OF_DATE)) %>% 
  select(ISSUER_NAME, ISSUER_ISIN, YEAR, MONTH, QUARTER, INDUSTRY_ADJUSTED_SCORE) %>% filter(as.numeric(MONTH)/3 == round(as.numeric(MONTH)/3))

# simplified for the other years
ESG_total <- ESG_2007
rm(ESG_2007)

preprocess <- function(x){
  x[["AS_OF_DATE"]] <- as.Date(x[["AS_OF_DATE"]], "%Y%m%d")
  
  x <- x %>% mutate(YEAR = format(AS_OF_DATE, "%Y"), MONTH = format(AS_OF_DATE, "%m"), QUARTER = quarter(AS_OF_DATE)) %>% 
    select(ISSUER_NAME, ISSUER_ISIN, YEAR, MONTH, QUARTER, INDUSTRY_ADJUSTED_SCORE) %>% filter(as.numeric(MONTH)/3 == round(as.numeric(MONTH)/3))
  
  ESG_total <<-  rbind(ESG_total, x)
}

ESG_2013 <- read_excel("C:/Users/Anwender/OneDrive/Studium/10. Semester/Portfolio Management/Daten von Fabio/ESG Ratings Timeseries Expanded 2013.xlsx")
preprocess(ESG_2013)
rm(ESG_2013)

ESG_2014 <- read_excel("C:/Users/Anwender/OneDrive/Studium/10. Semester/Portfolio Management/Daten von Fabio/ESG Ratings Timeseries Expanded 2014.xlsx")
preprocess(ESG_2014)
rm(ESG_2014)

ESG_2015 <- read_excel("C:/Users/Anwender/OneDrive/Studium/10. Semester/Portfolio Management/Daten von Fabio/ESG Ratings Timeseries Expanded 2015.xlsx")
preprocess(ESG_2015)
rm(ESG_2015)

ESG_2016 <- read_excel("C:/Users/Anwender/OneDrive/Studium/10. Semester/Portfolio Management/Daten von Fabio/ESG Ratings Timeseries Expanded 2016.xlsx")
preprocess(ESG_2016)
rm(ESG_2016)

ESG_2017 <- read_excel("C:/Users/Anwender/OneDrive/Studium/10. Semester/Portfolio Management/Daten von Fabio/ESG Ratings Timeseries Expanded 2017.xlsx")
preprocess(ESG_2017)
rm(ESG_2017)

ESG_2018 <- read_excel("C:/Users/Anwender/OneDrive/Studium/10. Semester/Portfolio Management/Daten von Fabio/ESG Ratings Timeseries Expanded 2018.xlsx")
preprocess(ESG_2018)
rm(ESG_2018)

ESG_2019 <- read_excel("C:/Users/Anwender/OneDrive/Studium/10. Semester/Portfolio Management/Daten von Fabio/ESG Ratings Timeseries Expanded 2019.xlsx")
preprocess(ESG_2019)
rm(ESG_2019)

ESG_2020 <- read_excel("C:/Users/Anwender/OneDrive/Studium/10. Semester/Portfolio Management/Daten von Fabio/ESG Ratings Timeseries Expanded 2020 Q2.xlsx")
preprocess(ESG_2020)
rm(ESG_2020)

## Merge with Fama French
FamaFrenchInput <- read_excel("FamaFrenchInput2.xlsx")
FF_merged <- merge(FamaFrenchInput, ESG_total, by.x = c('isin', 'year','quarter'), by.y = c('ISSUER_ISIN', 'YEAR','QUARTER')) 

## NAs & Outlier
FF_merged$INDUSTRY_ADJUSTED_SCORE <- as.numeric(as.character(FF_merged$INDUSTRY_ADJUSTED_SCORE))

# FF_merged3$return <- Winsorize(FF_merged$return, probs = c(0.01, 0.99)) 
FF_merged_clean <- bind_rows(
  lapply(FF_merged, function(x) if(is.numeric(x)){
    Winsorize(x, probs = c(0.01, 0.99), na.rm = T)
  } else {x}))


## Descriptive Statistics Dataset
# Number of stocks in investment universe
stocks_count1 <- FamaFrenchInput %>% filter(quarter == 2) %>% select(datadate, isin) %>% group_by(datadate) %>% summarize(count=n())

# Number of stocks after preprocessing
stocks_count2 <- FF_merged_clean %>% filter(quarter == 2) %>% select(datadate, isin) %>% group_by(datadate) %>% summarize(count=n())

## Prepare FF Factors
library(xts)
download.file("https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_TXT.zip",
              destfile="F-F_Research_Data_Factors.zip", mode="wb")

unzip("F-F_Research_Data_Factors.zip")
ff.factors <- read.delim("F-F_Research_Data_5_Factors_2x3.txt",
                         sep="",
                         nrows = 694,
                         header=FALSE,
                         skip=4,
                         stringsAsFactors=FALSE)


names(ff.factors) <- c("Date", "MKT", "SMB", "HML", "RMW", "CMA", "RF")

ff.factors[,2:7] <- ff.factors[,2:7]/100+1

ff.factors <- ff.factors %>% 
  separate(Date, into = c("Year", "Month"), sep=c(4))

ff.factors$Quarter <- as.numeric(ff.factors$Month)/3
ff.factors$Quarter <- ceiling(ff.factors$Quarter)

ff.factors.quarterly <- ff.factors %>% group_by(Year, Quarter) %>% summarise(MKT=prod(MKT)-1, SMB=prod(SMB)-1, HML=prod(HML)-1, RMW=prod(RMW)-1, CMA=prod(CMA)-1, RF=prod(RF)-1)


### Portfolio Construction
mappe12 <- FF_merged_clean
rm(FF_merged_clean)

## Size Factor
# Calculate size breakpoints
tbl.size_breakpoints <- mappe12 %>%
  filter(quarter== 2 & nyse_nas == 1) %>%
  select(reference_date_size, market_cap) %>%
  group_by(reference_date_size) %>%
  summarize(size_median = median(market_cap))             

# Stocks are sorted regarding size
tbl.size_sorts <- mappe12 %>%
  filter(quarter== 2) %>%
  left_join(tbl.size_breakpoints, by = "reference_date_size") %>%
  mutate(size_portfolio = case_when(market_cap > size_median ~ "B",
                                    market_cap <= size_median ~ "S",
                                    TRUE ~ as.character(NA))) %>%
  select(isin, reference_date_size, size_portfolio)

# Add size portfolio assignment back to stock data
mappe12 <- mappe12 %>% 
  left_join(tbl.size_sorts, by = c("isin", "reference_date_size"))


## Value Factor (sequential)
# Calculate value breakpoints
tbl.value_breakpoints_for_large_caps <- mappe12 %>%
  filter(quarter== 2 & nyse_nas == 1, size_portfolio=="B") %>%
  select(reference_date_bm, book_to_market) %>%
  group_by(reference_date_bm) %>%
  summarize(value_median = median(book_to_market))

tbl.value_breakpoints_for_small_caps <- mappe12 %>%
  filter(quarter== 2 & nyse_nas == 1, size_portfolio=="S") %>%
  select(reference_date_bm, book_to_market) %>%
  group_by(reference_date_bm) %>%
  summarize(value_median = median(book_to_market))

# Stocks in both exisiting portfolios are sorted regarding value  
tbl.value_sorts_large <- mappe12 %>%
  filter(quarter== 2,size_portfolio=="B") %>%
  left_join(tbl.value_breakpoints_for_large_caps, by = "reference_date_bm") %>%
  mutate(value_portfolio= case_when(book_to_market > value_median ~ "H",
                                    book_to_market <= value_median ~ "L", 
                                    TRUE ~ as.character(NA))) %>%
  select(isin, reference_date_bm, value_portfolio)

tbl.value_sorts_small <- mappe12 %>%
  filter(quarter== 2,size_portfolio=="S") %>%
  left_join(tbl.value_breakpoints_for_small_caps, by = "reference_date_bm") %>%
  mutate(value_portfolio = case_when(book_to_market > value_median ~ "H",
                                     book_to_market <= value_median ~ "L", 
                                     TRUE ~ as.character(NA))) %>%
  select(isin, reference_date_bm, value_portfolio)

# combine both again
tbl.value_sorts_combined<-rbind(tbl.value_sorts_large,tbl.value_sorts_small)

# Add value portfolio assignment back to stock data
mappe13 <- merge(mappe12, tbl.value_sorts_combined, by.x = c('isin', 'reference_date_bm'), by.y = c('isin', 'reference_date_bm'))

## ESG Factor for different breakpoints (& subsequent regression analysis)

regressions <- list()
standard_errors <- list()
p_values <- list()

# Sequential Sorting for 4 exisiting portfolios 
for (i in c(0.05, 0.1, 0.15)){
  breakpoint <- i
  # Small & High
  tbl.esg_breakpoints <- mappe13 %>%
    filter(quarter== 2 & nyse_nas == 1 & size_portfolio == "S" & value_portfolio == "H") %>%
    select(reference_date_size, INDUSTRY_ADJUSTED_SCORE) %>%
    group_by(reference_date_size) %>%
    summarize(Top10 = quantile(INDUSTRY_ADJUSTED_SCORE, 1-breakpoint), Flop10 = quantile(INDUSTRY_ADJUSTED_SCORE, breakpoint)) 
  
  tbl.esg_sorts <- mappe13 %>%
    filter(quarter== 2 & size_portfolio == "S" & value_portfolio == "H") %>%
    left_join(tbl.esg_breakpoints, by = "reference_date_size") %>%
    mutate(esg_portfolio = case_when(INDUSTRY_ADJUSTED_SCORE >= Top10 ~ "Top10",
                                     INDUSTRY_ADJUSTED_SCORE <= Flop10 ~ "Flop10",
                                     TRUE ~ as.character(NA))) %>%
    select(isin, reference_date_size, esg_portfolio)
  
  tbl.esg_sorts_combined <- tbl.esg_sorts
  
  # Small & Low
  tbl.esg_breakpoints <- mappe13 %>%
    filter(quarter== 2 & nyse_nas == 1 & size_portfolio == "S" & value_portfolio == "L") %>%
    select(reference_date_size, INDUSTRY_ADJUSTED_SCORE) %>%
    group_by(reference_date_size) %>%
    summarize(Top10 = quantile(INDUSTRY_ADJUSTED_SCORE, 1-breakpoint), Flop10 = quantile(INDUSTRY_ADJUSTED_SCORE, breakpoint)) 
  
  tbl.esg_sorts <- mappe13 %>%
    filter(quarter== 2 & size_portfolio == "S" & value_portfolio == "L") %>%
    left_join(tbl.esg_breakpoints, by = "reference_date_size") %>%
    mutate(esg_portfolio = case_when(INDUSTRY_ADJUSTED_SCORE >= Top10 ~ "Top10",
                                     INDUSTRY_ADJUSTED_SCORE <= Flop10 ~ "Flop10",
                                     TRUE ~ as.character(NA))) %>%
    select(isin, reference_date_size, esg_portfolio)
  
  tbl.esg_sorts_combined<-rbind(tbl.esg_sorts_combined,tbl.esg_sorts)
  
  # Big & High
  tbl.esg_breakpoints <- mappe13 %>%
    filter(quarter== 2 & nyse_nas == 1 & size_portfolio == "B" & value_portfolio == "H") %>%
    select(reference_date_size, INDUSTRY_ADJUSTED_SCORE) %>%
    group_by(reference_date_size) %>%
    summarize(Top10 = quantile(INDUSTRY_ADJUSTED_SCORE, 1-breakpoint), Flop10 = quantile(INDUSTRY_ADJUSTED_SCORE, breakpoint)) 
  
  tbl.esg_sorts <- mappe13 %>%
    filter(quarter== 2 & size_portfolio == "B" & value_portfolio == "H") %>%
    left_join(tbl.esg_breakpoints, by = "reference_date_size") %>%
    mutate(esg_portfolio = case_when(INDUSTRY_ADJUSTED_SCORE >= Top10 ~ "Top10",
                                     INDUSTRY_ADJUSTED_SCORE <= Flop10 ~ "Flop10",
                                     TRUE ~ as.character(NA))) %>%
    select(isin, reference_date_size, esg_portfolio)
  
  tbl.esg_sorts_combined<-rbind(tbl.esg_sorts_combined,tbl.esg_sorts)
  
  # Big & Low
  tbl.esg_breakpoints <- mappe13 %>%
    filter(quarter== 2 & nyse_nas == 1 & size_portfolio == "B" & value_portfolio == "L") %>%
    select(reference_date_size, INDUSTRY_ADJUSTED_SCORE) %>%
    group_by(reference_date_size) %>%
    summarize(Top10 = quantile(INDUSTRY_ADJUSTED_SCORE, 1-breakpoint), Flop10 = quantile(INDUSTRY_ADJUSTED_SCORE, breakpoint)) 
  
  tbl.esg_sorts <- mappe13 %>%
    filter(quarter== 2 & size_portfolio == "B" & value_portfolio == "L") %>%
    left_join(tbl.esg_breakpoints, by = "reference_date_size") %>%
    mutate(esg_portfolio = case_when(INDUSTRY_ADJUSTED_SCORE >= Top10 ~ "Top10",
                                     INDUSTRY_ADJUSTED_SCORE <= Flop10 ~ "Flop10",
                                     TRUE ~ as.character(NA))) %>%
    select(isin, reference_date_size, esg_portfolio)
  
  tbl.esg_sorts_combined<-rbind(tbl.esg_sorts_combined, tbl.esg_sorts)
  
  
  # Add ESG portfolio assignment back to stock data
  mappe14 <- merge(mappe13, tbl.esg_sorts_combined, by.x = c('isin', 'reference_date_size'), by.y = c('isin', 'reference_date_size'))
  
  # Portfolio Returns Calculation (High ESG, Low ESG, High-Low ESG)
  tbl.portfolios <- mappe14 %>%
    group_by(datadate, size_portfolio, value_portfolio, esg_portfolio) %>%
    summarize(ret_vw = mean(return)) %>%
    ungroup() %>% 
    mutate(portfolio = paste0(size_portfolio, "/", value_portfolio, "/", esg_portfolio))
  
  esg_returns <- tbl.portfolios %>%
    group_by(datadate) %>%
    summarize(esg = mean(ret_vw[portfolio %in% c("S/H/Top10", "S/L/Top10", "B/H/Top10", "B/L/Top10")]) -
                mean(ret_vw[portfolio %in% c("S/H/Flop10", "S/L/Flop10", "B/H/Flop10", "B/L/Flop10")]),
              highesg = mean(ret_vw[portfolio %in% c("S/H/Top10", "S/L/Top10", "B/H/Top10", "B/L/Top10")]),
              lowesg = mean(ret_vw[portfolio %in% c("S/H/Flop10", "S/L/Flop10", "B/H/Flop10", "B/L/Flop10")]))
  
  ## Regression analysis 
  
  # Merge Regression input
  esg_returns <- esg_returns %>% 
    separate(datadate, into = c("Year", "Quarter"), sep=c(4))
  
  regression_input <- merge(esg_returns, ff.factors.quarterly, by.x = c('Year','Quarter'), by.y = c('Year','Quarter')) 
  regression_input$esg_premium <- regression_input$esg - regression_input$RF
  regression_input$highesg_premium <- regression_input$highesg - regression_input$RF
  regression_input$lowesg_premium <- regression_input$lowesg - regression_input$RF
  
  breakpoint = paste(breakpoint*100, "%", sep="")
  
  # High-ESG 3-Factor
  regression <- lm(highesg_premium ~ MKT + SMB + HML, data=regression_input)
  regressions[[paste("High-rated 3-Factors", breakpoint, sep = " ")]] <- summary(regression)
  regression <- coeftest(regression, vcov=vcovHC(regression, "HC1"))
  standard_errors[[paste("High-rated 3-Factors", breakpoint, sep = " ")]] <- regression[,2]
  p_values[[paste("High-rated 3-Factors", breakpoint, sep = " ")]] <- regression[,4]
  
  # High-ESG 5-Factor
  regression <- lm(highesg_premium ~ MKT + SMB + HML + RMW + CMA, data=regression_input)
  regressions[[paste("High-rated 5-Factors", breakpoint, sep = " ")]] <- summary(regression)
  regression <- coeftest(regression, vcov=vcovHC(regression, "HC1"))
  standard_errors[[paste("High-rated 5-Factors", breakpoint, sep = " ")]] <- regression[,2]
  p_values[[paste("High-rated 5-Factors", breakpoint, sep = " ")]] <- regression[,4]
  
  # Low-ESG 3-Factor
  regression <- lm(lowesg_premium ~ MKT + SMB + HML, data=regression_input)
  regressions[[paste("Low-rated 3-Factors", breakpoint, sep = " ")]] <- summary(regression)
  regression <- coeftest(regression, vcov=vcovHC(regression, "HC1"))
  standard_errors[[paste("Low-rated 3-Factors", breakpoint, sep = " ")]] <- regression[,2]
  p_values[[paste("Low-rated 3-Factors", breakpoint, sep = " ")]] <- regression[,4]
  
  # Low-ESG 5-Factor
  regression <- lm(lowesg_premium ~ MKT + SMB + HML + RMW + CMA, data=regression_input)
  regressions[[paste("Low-rated 5-Factors", breakpoint, sep = " ")]] <- summary(regression)
  regression <- coeftest(regression, vcov=vcovHC(regression, "HC1"))
  standard_errors[[paste("Low-rated 5-Factors", breakpoint, sep = " ")]] <- regression[,2]
  p_values[[paste("Low-rated 5-Factors", breakpoint, sep = " ")]] <- regression[,4]
  
  # High-Low 3-Factor
  regression <- lm(esg_premium ~ MKT + SMB + HML, data=regression_input)
  regressions[[paste("Long-short 3-Factors", breakpoint, sep = " ")]] <- summary(regression)
  regression <- coeftest(regression, vcov=vcovHC(regression, "HC1"))
  standard_errors[[paste("Long-short 3-Factors", breakpoint, sep = " ")]] <- regression[,2]
  p_values[[paste("Long-short 3-Factors", breakpoint, sep = " ")]] <- regression[,4]
  
  # High-Low 5-Factor
  regression <- lm(esg_premium ~ MKT + SMB + HML + RMW + CMA, data=regression_input)
  regressions[[paste("Long-short 5-Factors", breakpoint, sep = " ")]] <- summary(regression)
  regression <- coeftest(regression, vcov=vcovHC(regression, "HC1"))
  standard_errors[[paste("Long-short 5-Factors", breakpoint, sep = " ")]] <- regression[,2]
  p_values[[paste("Long-short 5-Factors", breakpoint, sep = " ")]] <- regression[,4]
  
}

# main regression output
model.names <- rep(c("3-Factors", "5-Factors"), times = 3)
htmlreg(regressions[7:12], digits=3, override.se = standard_errors[7:12], override.pvalues = p_values[7:12], custom.header = list("high-rated" = 1:2, "low-rated" = 3:4, "long-short" = 5:6), custom.model.names = model.names, single.row = T, padding = 10)

# robustness check
htmlreg(regressions[1:6], digits=3, override.se = standard_errors[1:6], override.pvalues = p_values[1:6], custom.header = list("high-rated" = 1:2, "low-rated" = 3:4, "long-short" = 5:6), custom.model.names = model.names, single.row = T, padding = 10)
htmlreg(regressions[13:18], digits=3, override.se = standard_errors[13:18], override.pvalues = p_values[13:18], custom.header = list("high-rated" = 1:2, "low-rated" = 3:4, "long-short" = 5:6), custom.model.names = model.names, single.row = T, padding = 10)

