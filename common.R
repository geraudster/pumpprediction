# Common features

library(lubridate)
prepareDataSet <- function(dataset, type) {
  emptyPos <- dataset$longitude == 0
  dataset[emptyPos, 'longitude'] <- NA
  dataset[emptyPos, 'latitude'] <- NA
  dataset[emptyPos, 'gps_height'] <- NA
  
  emptyConstrutionYear <- dataset$construction_year == 0
  dataset[emptyConstrutionYear,'construction_year'] <- NA
  
  dataset$wpt_name <- as.character(dataset$wpt_name)
  
  dataset$date_recorded <- ymd(dataset$date_recorded)
  dataset$date_recorded_year <- year(dataset$date_recorded)
  dataset$date_recorded_year_ago <- 2015 - dataset$date_recorded_year
  dataset$date_recorded_month <- month(dataset$date_recorded)
  dataset$date_recorded_day <- day(dataset$date_recorded)
  
  # dataset$construction_year <- parse_date_time(dataset$construction_year, 'y')
  dataset$region_code <- factor(dataset$region_code)
  dataset$district_code <- factor(dataset$district_code)
  dataset$type <- type
  dataset
}

library(Amelia)
imputeOnTrainAndTestset <- function(trainset, testset) {
  trainAndTestSet <- rbind(trainset, testset)
  
  set.seed(2)
  trainAndTestSetImpute <- amelia(x = trainAndTestSet, m = 5, idvars = c( "funder", "installer", "wpt_name", "basin", "subvillage", "region",  "lga", "ward", "public_meeting", "recorded_by", "permit",
                                                                          "extraction_type", "extraction_type_group",  "extraction_type_class", "management", "management_group", "payment",  "payment_type",
                                                                          "water_quality", "quality_group", "quantity",  "quantity_group", "source", "source_type", "source_class", "waterpoint_type",  "waterpoint_type_group",
                                                                          "region_code", "district_code",  "scheme_name", "type", 'date_recorded'), ts = NULL, cs = NULL, priors = NULL,  
                                  lags = NULL, empri = 0, intercs = FALSE, leads = NULL, splinetime = NULL,  
                                  logs = NULL, sqrts = NULL, lgstc = NULL, ords = NULL, noms = c("scheme_management"),  
                                  bounds = NULL, max.resample = 1000, tolerance = 1e-04) 
  imputation <- trainAndTestSetImpute$imputations$imp5
  imputation$construction_age <- 2015 - imputation$construction_year
  imputation
}

imputeValidation <- function(testset) {
  trainAndTestSet <- testset
  
  set.seed(2)
  trainAndTestSetImpute <- amelia(x = trainAndTestSet, m = 5, idvars = c( "funder", "installer", "wpt_name", "basin", "subvillage", "region",  "lga", "ward", "public_meeting", "recorded_by", "permit",
                                                                          "extraction_type", "extraction_type_group",  "extraction_type_class", "management", "management_group", "payment",  "payment_type",
                                                                          "water_quality", "quality_group", "quantity",  "quantity_group", "source", "source_type", "source_class", "waterpoint_type",  "waterpoint_type_group",
                                                                          "region_code", "district_code",  "scheme_name", "type", 'date_recorded'), ts = NULL, cs = NULL, priors = NULL,  
                                  lags = NULL, empri = 0, intercs = FALSE, leads = NULL, splinetime = NULL,  
                                  logs = NULL, sqrts = NULL, lgstc = NULL, ords = NULL, noms = c("scheme_management"),  
                                  bounds = NULL, max.resample = 1000, tolerance = 1e-04) 
  imputation <- trainAndTestSetImpute$imputations$imp5
  imputation
}

library(plyr)
library(magrittr)

# get class in dataset1 not in dataset2
getDiffByCategory <- function(dataset1, dataset2, category) {
  if(is.factor(dataset1[,category])) {
    print(category)
    setdiff(dataset1[[category]], intersect(dataset1[[category]], dataset2[[category]]))
  } else {
    list()
  }
}

correlationAggregByCategory <- function(dataset, category) {
  if(is.factor(trainsetImpute[,category])) {
    print(category)
    statsByFunderInstaller <- ddply(dataset, category, summarise,
                                    tot_functional = sum(status_group == 'functional'),
                                    tot_repair = sum(status_group == 'functional needs repair'),
                                    tot_nonfunctional = sum(status_group == 'non functional'))
    
    corMatrix <- cor(statsByFunderInstaller[,c('tot_functional', 'tot_repair', 'tot_nonfunctional')])
    res <- corMatrix %>% .[upper.tri(corMatrix)] %>%
      abs %>% 
      is_greater_than(0.92) %>%
      sum %>%
      equals(3) %>%
      c(category, .)
    res
  }
}

aggregByCategory <- function(dataset, category) {
  if(is.factor(dataset[,category])) {
    print(category)
    ddply(dataset, category, summarise,
          tot_functional = sum(status_group == 'functional'),
          tot_repair = sum(status_group == 'functional needs repair'),
          tot_nonfunctional = sum(status_group == 'non functional'))
  }
}

mergeAggregs <- function (dataset, aggregs) {
  for(var in names(aggregs)) {
    if(!is.null(aggregs[[var]])) {
      print(var)
      dataset <- merge(dataset, aggregs[[var]], all.x = TRUE)
      # print(colnames(dataset[,c('tot_functional', 'tot_repair', 'tot_nonfunctional')]))
      colnames(dataset)[colnames(dataset) %in% c('tot_functional', 'tot_repair', 'tot_nonfunctional')] <- 
        make.names(paste(c('tot_functional', 'tot_repair', 'tot_nonfunctional'), var))
    }
  }
  dataset
}

scaleMergeDataset  <- function(dataset) {
  predictorsAggregs <- colnames(dataset)[!colnames(dataset) %in% c('id', 'status_group')]
  predictorsAggregsPos <- grep('tot_', colnames(dataset))
  
  cbind(dataset[, -predictorsAggregsPos],
        as.data.frame(dataset[,predictorsAggregsPos]))
}
