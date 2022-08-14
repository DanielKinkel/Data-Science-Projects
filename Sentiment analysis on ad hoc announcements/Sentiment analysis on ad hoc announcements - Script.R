# R code for the term project in text mining WS 20/21

rm(list = objects())
graphics.off()
options(scipen = 200)

## Packages
library(tm)
library(SentimentAnalysis)
library(dplyr)
library(texreg)
library(stargazer)
library(sensemakr)
library(rlist)
library(wordcloud)
library(moments)


### 2. Dataset
load("dataset_adhoc.Rda")

# histogram of returns
hist(df_adhoc$Return, freq = T, breaks = seq(from = -85, to = 185, by = 2), xlim = c(-50, 50), col = "lightskyblue2", xlab = "Daily Return", main = "Histogram Of Daily Stock Returns", xaxt = "n")
axis(1, at = seq(-60, 60, 10))

# descriptive Table
attach(df_adhoc)
descriptive <- data.frame(Variable = c("Return", "Price-To-Book", "Company Sector"), Type = c("continuous", "continuous", "nominal"),
                          NAs = c(sum(is.na(Return)), sum(is.na(PriceToBook)), sum(CompanySector=="")), 
                          Min. = c(min(Return), min(PriceToBook, na.rm = T), NA),
                          Median = c(median(Return), median(PriceToBook, na.rm = T), NA),
                          Mean = c(mean(Return), mean(PriceToBook, na.rm = T), NA),
                          Max. = c(max(Return), max(PriceToBook, na.rm = T), NA),
                          Skewness = c(skewness(Return), skewness(PriceToBook, na.rm = T), NA),
                          Kurtosis = c(kurtosis(Return), kurtosis(PriceToBook, na.rm = T), NA))
detach(df_adhoc)
  #stargazer(descriptive, type = "html", summary = F, digits = 2, rownames = F, out = "descriptive.doc")

# remove missing data
df_adhoc <- df_adhoc[-which(df_adhoc$CompanySector=="" | df_adhoc$PriceToBook<0 | is.nan(df_adhoc$PriceToBook)==T),]

# creating dummies for year
df_adhoc <- df_adhoc %>% mutate(year = as.factor(strftime(df_adhoc$date, format = "%Y")))


### 3. Preprocessing & Sentiment Analysis
# convert announcements to corpus
adhoc <- Corpus(VectorSource(df_adhoc$main))

## manual preprocessing
adhoc_man <- adhoc %>%
  tm_map(PlainTextDocument) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords()) %>%
  tm_map(stemDocument)

# load LM dictionary
LM <- loadDictionaryLM()

# convert preprocessed corpus to dtm
dtm <- DocumentTermMatrix(adhoc_man)

# word recognition
totalpos <- sum(dtm[[6]]$Terms %in% LM$positiveWords)
totalneg <- sum(dtm[[6]]$Terms %in% LM$negativeWords)

# create word cloud
tdm <- TermDocumentMatrix(adhoc_man)
v <- tdm %>% as.matrix() %>% rowSums() %>% sort(decreasing = T)
tdf <- data.frame(word = names(v), freq = v)
set.seed(55)
png("wordcloud.png", width=12,height=8, units='in', res=300)
wordcloud(words = tdf$word, freq = tdf$freq, max.words = 200, random.order = F)
dev.off()

## sentiment analysis
ALLsentiment <- analyzeSentiment(adhoc)

# only LM Sentiment
LMsentiment <- ALLsentiment$SentimentLM

# histogram of sentiment data
hist(LMsentiment, freq = T, breaks = seq(from = -1, to = 1, by = 0.01), xlim = c(-0.1,0.1), col = "steelblue4", xlab = "Daily Return", main = "Histogram Of Net Optimism Scores", xaxt = "n")
axis(1, at = seq(-0.2, 0.2, 0.1))

meanLM <- mean(LMsentiment)
sdLM <- sd(LMsentiment)


### 4. Analysis of the effect of sentiment
## sample split and correlation with daily return
# prepare dataframe
df <- df_adhoc %>% 
  select(Return, CompanySector, PriceToBook, year) %>% 
  mutate(Sentiment = LMsentiment)

# split sample by sentiment
df_pos <- df[df$Sentiment>0,] 
df_zer <- df[df$Sentiment==0,]
df_neg <- df[df$Sentiment<0,]

df_split <- data.frame(Sentiment = c("positive Sentiment", "neutral Sentiment", "negative Sentiment"),
                       Mean = c(mean(df_pos$Return), mean(df_zer$Return), mean(df_neg$Return)), 
                       Median = c(median(df_pos$Return), median(df_zer$Return), median(df_neg$Return)),
                       SD = c(sd(df_pos$Return), sd(df_zer$Return), sd(df_neg$Return)))
  #stargazer(df_split, type = "html", summary = F, digits = 2, rownames = F, out = "sentiment_split.doc")

## regression analysis
regression1 <- lm(Return ~ 1 + Sentiment, data =df)
regression2 <- lm(Return ~ Sentiment + PriceToBook + CompanySector + year, data = df)
regression3 <- lm(Return ~ Sentiment + PriceToBook + CompanySector + year
                  + PriceToBook*CompanySector, data = df)
  #htmlreg(list(regression1, regression2, regression3), digits=3, file = "regression1.doc")

# diagnostics
par(mfrow = c(2,2))
plot(regression3)
dev.off()

# interpretation
sdLM*coefficients(regression3)["Sentiment"]

# effect sizes
sapply(list(R1 = regression1, R2 = regression2, R3 = regression3), partial_f2, covariates = "Sentiment")


### 5. Robustness Check
## robustness check 1
# outlier correction and log transformation of price-to-book
outliersZ <- function(df, Return, zCutOff) {
  stdev <- sd(Return)
  absZ <- abs(Return - mean(Return)) / stdev
  df[-which(absZ > zCutOff),] 
}
df2 <- outliersZ(df, df$Return, 3)
df2 <- outliersZ(df2, df2$PriceToBook, 3)

# repeating regression
regression4 <- lm(Return ~ 1 + Sentiment, data =df2)
regression5 <- lm(Return ~ Sentiment + log(PriceToBook) + CompanySector + year, data = df2)
regression6 <- lm(Return ~ Sentiment + log(PriceToBook) + CompanySector + year + log(PriceToBook)*CompanySector, data = df2)
  #htmlreg(list(regression4, regression5, regression6), digits=3, file = "regression2.doc")

# effect sizes
sapply(list(R4 = regression4, R5 = regression5, R6 = regression6), partial_f2, covariates = "Sentiment")

## robustness check 2
# manual LM sentiment with sparsity
LMsentiment_sp <- data.frame(LMsentiment = LMsentiment)

for (i in c(0.95, 0.90, 0.85)) {
  dtm <- DocumentTermMatrix(adhoc_man)
  dtm <- removeSparseTerms(dtm, sparse = i)
  dtm.pos <- DocumentTermMatrix(adhoc_man,
                                list(dictionary = t(LM$positiveWords)))
  dtm.neg <- DocumentTermMatrix(adhoc_man,
                                list(dictionary = t(LM$negativeWords)))
  # initialize empty vector to store results
  LM_sp <- numeric(length(adhoc_man))
  # Iterate over all documents
  for (i in 1:length(adhoc_man)){LM_sp[i] <- (sum(dtm.pos[i, ]) - sum(dtm.neg[i, ])) / sum(dtm[i, ])}
  
  LMsentiment_sp <- cbind(LMsentiment_sp, LM_sp)
}

# repeating regression
reg_sp <- list()

for (i in 1:4) {
  df_sp <- df_adhoc %>% 
    select(Return, CompanySector, PriceToBook, year) %>% 
    mutate(Sentiment = LMsentiment_sp[, i])
  
  reg <- regression <- lm(Return ~ Sentiment + PriceToBook + CompanySector + year + PriceToBook*CompanySector, data = df_sp)
  
  reg_sp <- list.append(reg_sp, reg)
}

  #htmlreg(list(summary(reg_sp[[1]]), summary(reg_sp[[2]]), summary(reg_sp[[3]]), summary(reg_sp[[4]])), digits=3, file = "regression4.doc")

# effect size
sapply(reg_sp, partial_f2, covariates = "Sentiment")

## robustness check 3
# split by company sector
sectors <- split(df, df$CompanySector)
sector.split <- data.frame()

for(i in 2:35) {
  reg <-lm(Return ~ Sentiment + PriceToBook + year , data = sectors[[i]])
  f2 <- partial_f2(reg, covariates = "Sentiment")
  reg <- summary(reg)
  sector.split <- rbind(sector.split, c(reg$coefficients[2,], f2))
}
sector.split <- round(sector.split, digits =3)
CS <- data.frame(table(df$CompanySector))
sector.split <- cbind(CS[2:35,], sector.split)
colnames(sector.split) <- c("Company Sector", "N", "Sentiment Estimate", "Std. Error", "t value", "Pr(>|t|)", "Cohens f^2")

# add significance stars
sector.split$`Sentiment Estimate` <- as.character(sector.split$`Sentiment Estimate`)
sig.stars <- rep(" ", times = 34)
sig.stars[sector.split$`Pr(>|t|)` < 0.05] <- "*"
sig.stars[sector.split$`Pr(>|t|)` < 0.01] <- "**"
sig.stars[sector.split$`Pr(>|t|)` < 0.001] <- "***"
sector.split$`Sentiment Estimate` <- paste(sector.split$`Sentiment Estimate`, sig.stars, sep = " ")

# substitute "and" for "&" due to HTML
sector.split$`Company Sector` <- gsub("&", "and", sector.split$`Company Sector`)

  #stargazer(sector.split, type = "html", summary = F, rownames = F, digits = 3, out = "company_split.doc")


## robustness check 4
# splitting NOS into positive and negative ratio
df$Positive <- ALLsentiment$PositivityLM
df$Negative <- ALLsentiment$NegativityLM

# repeating regression
regression11 <- lm(Return ~ 1 + Positive + Negative, data = df)
regression12 <- lm(Return ~ Positive + Negative + PriceToBook + CompanySector + year, data = df)
regression13 <- lm(Return ~ Positive + Negative + PriceToBook + CompanySector + year + PriceToBook*CompanySector, data = df)
  #htmlreg(list(regression7, regression8, regression9), digits=3, file = "regression3.doc")

# effect sizes
sapply(list(R11 = regression11, R12 = regression12, R13 = regression13), partial_f2, covariates = "Negative")

#rm(list = objects())
#graphics.off()
