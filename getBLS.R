library(devtools)
library(blsAPI)
library(rjson)
library(stringr)
library(jsonlite)

countycodes = list("LAUST080000000000003","LAUCN080010000000003","LAUCN080030000000003","LAUCN080050000000003","LAUCN080070000000003","LAUCN080090000000003","LAUCN080110000000003","LAUCN080130000000003","LAUCN080140000000003","LAUCN080150000000003","LAUCN080170000000003","LAUCN080190000000003","LAUCN080210000000003","LAUCN080230000000003","LAUCN080250000000003","LAUCN080270000000003","LAUCN080290000000003","LAUCN080310000000003","LAUCN080330000000003","LAUCN080350000000003","LAUCN080370000000003","LAUCN080390000000003","LAUCN080410000000003","LAUCN080430000000003","LAUCN080450000000003","LAUCN080470000000003","LAUCN080490000000003","LAUCN080510000000003","LAUCN080530000000003","LAUCN080550000000003","LAUCN080570000000003","LAUCN080590000000003","LAUCN080610000000003","LAUCN080630000000003","LAUCN080650000000003","LAUCN080670000000003","LAUCN080690000000003","LAUCN080710000000003","LAUCN080730000000003","LAUCN080750000000003","LAUCN080770000000003","LAUCN080790000000003","LAUCN080810000000003","LAUCN080830000000003","LAUCN080850000000003","LAUCN080870000000003","LAUCN080890000000003","LAUCN080910000000003","LAUCN080930000000003","LAUCN080950000000003","LAUCN080970000000003","LAUCN080990000000003","LAUCN081010000000003","LAUCN081030000000003","LAUCN081050000000003","LAUCN081070000000003","LAUCN081090000000003","LAUCN081110000000003","LAUCN081130000000003","LAUCN081150000000003","LAUCN081170000000003","LAUCN081190000000003","LAUCN081210000000003","LAUCN081230000000003","LAUCN081250000000003")
startyears = list("2010","1990")
blsstring = "["
#countycodes = list("LAUST080000000000003")
#startyears = list("2010")

for (county in countycodes){
  blsstring <- paste(blsstring,'{"s":"',county,'","d":[',sep="")
  #get latest value
  payload <- list(
    'seriesid'=c(county),
    'latest'='true', 
    'annualaverage'='true',
    'registrationkey'='keyhere')
  response <- blsAPI(payload)
  latestjson <- fromJSON(response)
  
  #latest <- latestDF(json$Results$series[[1]]$data)
  latest <- latestDF(latestjson[["Results"]][["series"]][["data"]])
  latestrate <- latest[1,5]
  
  for (sy in startyears){
    print(county)
    payload <- list(
      'seriesid'=c(county),
      'startyear'=sy,
      'endyear'='2022', 
      'annualaverage'='true',
      'registrationkey'='keyhere')
    response <- blsAPI(payload)
    json <- fromJSON(response)
    testdf <- as.data.frame(json[["Results"]][["series"]][["data"]])
    #unemployed <- apiDF(json$Results$series$data)
    #unemployed <- apiDF(json[["Results"]][["series"]][["data"]])
    if (sy < 2010){
      unemployed <- apiDF(testdf)
    } else {
      unemployed <- apiDF2(testdf)
    }
    
    for (row in 1:nrow(unemployed)){
      mon <- str_sub(unemployed[row,3],1,3)
      yr <- unemployed[row,1]
      rate <- unemployed[row,4]
      if (row == 1 && sy == '2010'){
        blsstring <- paste(blsstring,'{"k":"',mon,yr,'","v":"',latestrate,'"},',sep="")
      } else{
        blsstring <- paste(blsstring,'{"k":"',mon,yr,'","v":"',rate,'"},',sep="")
      }
    }
    
  }
  blsstring <- str_sub(blsstring,1,nchar(blsstring)-1)
  blsstring <- paste(blsstring,']},',sep="")
}
blsstring <- str_sub(blsstring,1,nchar(blsstring)-1)
blsstring <- paste(blsstring,']',sep="")
blsjson <- noquote(blsstring)
write_json(blsstring, "08_bls.json")


apiDF <- function(data){
  df <- data.frame(year=character(),
                   period=character(),
                   periodName=character(),
                   value=character(),
                   stringsAsFactors=FALSE)
  data <- data[-c(5)]
  datalength <- length(unlist(data))
  datalength <- datalength/4
  i <- 0
  for(d in 1:datalength){
    i <- i + 1
    df[i,] <- data[i,]
  }
  return(df)
}

apiDF2 <- function(data){
  df <- data.frame(year=character(),
                   period=character(),
                   periodName=character(),
                   value=character(),
                   stringsAsFactors=FALSE)
  data <- data[-c(4,6)]
  datalength <- length(unlist(data))
  datalength <- datalength/4
  i <- 0
  for(d in 1:datalength){
    i <- i + 1
    df[i,] <- data[i,]
  }
  return(df)
}

latestDF <- function(data){
  df <- data.frame(year=character(),
                   period=character(),
                   periodName=character(),
                   value=character(),
                   latest=character(),
                   stringsAsFactors=FALSE)
  
  i <- 0
  for(d in data){
    i <- i + 1
    df[i,] <- unlist(d)
  }
  return(df)
}
