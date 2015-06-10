#------------------------------------------------------
# keyless_NYTScraper.R (requires your own keys)
# Last updated June 10, 2015
# Author: Keith Gladstone
#------------------------------------------------------

#Initialize libraries
library(jsonlite)
library(httr)

wkdir = #This variable should be set to the path where you want to save the files

setwd(wkdir)

#------------------------------------------------------
# Return a key from set of keys given a hash code. 
# Workaround for the NYT's API call quota of 10
# calls per second per key. The more keys listed,
# the more calls per second the caller function can
# perform. Feel free to add keys from more email addresses
# and add them to the concatenation of keys in local vector "keys"
# 
#------------------------------------------------------
getKey <- function(hashCode)
{

  #<<<<<---- 
  #          Keys have been removed for privacy. Please create your own at
  #          http://developer.nytimes.com/docs/reference/keys
  #          and change variable names accordingly to fill the "keys" vector
  #<<<<<----
  
  keys = c(key1p, key2p, key3p, key1g, key2g, key3g, key1s, key2s, key3s, key1a, key2a, key3a)
  numkeys = length(keys)
  sleep = 0.1/numkeys
  keyIndex = hashCode %% numkeys
  keyIndex = keyIndex + 1
  return(keys[keyIndex])
}

#------------------------------------------------------
# Return the number of articles returned from
# a given search query "search" on NYTimes.com within 
# a given year range
#------------------------------------------------------
getQuickHits <- function(search, startYear, endYear)
{
  root = "http://api.nytimes.com/svc/search/v2/articlesearch.json?fq="
  startDate = paste(startYear,"0101",sep = "")
  endDate = paste(endYear,"1231",sep = "")
  key = getKey(1)
  url = paste(root,search,"&begin_date=",startDate,"&end_date=",endDate,"&sort=oldest&api-key=",key, sep='')
  hits = fromJSON(url)$response$meta$hits

  return(hits)
}
#------------------------------------------------------
# Return the list of article titles on a given date
#------------------------------------------------------
getTitles <- function(search, rawDate)
{
  sleep = 0.1
  root = "http://api.nytimes.com/svc/search/v2/articlesearch.json?fl=headline&fq="
  date = format(as.Date(rawDate), format="%Y%m%d")
  key = getKey(1)
  url = paste(root,search,"&begin_date=",date,"&end_date=",date,"&sort=oldest&api-key=",key, sep='')
  title = NULL
  accumulatingMatrix = NULL

  for (i in 1:length(date))
  {
    title = fromJSON(url[i])$response$docs$headline$main
    articleDate = date[i]
    currentMatrix = cbind(articleDate,title)
    accumulatingMatrix = rbind(accumulatingMatrix, currentMatrix)
    Sys.sleep(sleep) #API allows only 10 calls per second
  }
  
  df = data.frame(accumulatingMatrix)
  
  return(df)
}
#------------------------------------------------------
# Return the number of hits of a given search query "search" 
# on NYTimes.com during each YEAR in a given range, 
# writing the output to "filename".dta
#------------------------------------------------------
getHitsByYear <- function(filename, search, startYear, endYear)
{
  sleep = 0.1
  root = "http://api.nytimes.com/svc/search/v2/articlesearch.json?fq="
  startDate = format(as.Date(paste(startYear:endYear,"-01-01",sep = "")), format="%Y%m%d")
  endDate = format(as.Date(paste(startYear:endYear,"-12-31",sep = "")), format="%Y%m%d")
  key = getKey(1)
  url = paste(root,search,"&begin_date=",startDate,"&end_date=",endDate,"&sort=oldest&api-key=",key, sep='')
  hits = NULL
  for (i in 1:length(startDate))
  {
    hits = c(hits,fromJSON(url[i])$response$meta$hits)
    Sys.sleep(sleep) #API allows only 10 calls per second
  }
  library(foreign)
  filename = paste(filename, ".dta", sep= "")
  df = data.frame(search, startYear:endYear, hits)
  write.dta(df, filename)
  
  return(df)
}

#------------------------------------------------------
# Return the number of hits of a given search query "search" 
# on NYTimes.com on each DAY in a given range, 
# writing the output to "filename".dta
#------------------------------------------------------
getHitsByDay <- function(filename, searches, startDate, endDate)
{
  root = "http://api.nytimes.com/svc/search/v2/articlesearch.json?fq="
  date = format(seq(as.Date(startDate), as.Date(endDate), by="+1 day"), format="%Y%m%d")
  df = data.frame()
  numkeys = 12
  sleep = 0.1/numkeys
  
  ### For blank searches
  if (searches == "")
  {
    for (i in 1:length(date))
    {
      key = getKey(i)
      root = "http://api.nytimes.com/svc/search/v2/articlesearch.json?"
      url = paste(root,"begin_date=",date[i],"&end_date=",date[i],"&sort=oldest&api-key=", key, sep='')
      result = fromJSON(url)$response$meta$hits
      row = cbind(date[i], result)
      df = rbind(df, row)
      Sys.sleep(sleep) #API allows only 10 calls per second
    }
    names(df) = c("date", "hits")
    
  }
  
  ## For general searches
  else {
    for (searchIndex in 1:length(searches))
    {
      search = searches[searchIndex]
      #time-consuming part of the code (linear time)
      for (i in 1:length(date))
      {
        key = getKey(i)
        url = paste(root,search,"&begin_date=",date[i],"&end_date=",date[i],"&sort=oldest&api-key=", key, sep='')
        result = fromJSON(url)$response$meta$hits
        row = cbind(search, date[i], result, searchIndex + offset - 1)
        df = rbind(df, row)
        Sys.sleep(sleep) #API allows only 10 calls per second
      }
    }
  
    names(df) = c("search", "date", "hits", "searchIndex")
  }
  #save data.frame to .dta file
  library(foreign)
  filename = paste(filename, ".dta", sep= "")
  write.dta(df, filename)
  
  return(df)
}

#------------------------------------------------------
# Return the article URLs from a given date range
#------------------------------------------------------
getURLs <- function(search, startDate, endDate)
{
  date = format(seq(as.Date(startDate), as.Date(endDate), by="+1 day"), format="%Y-%m-%d")
  mtx = NULL
  for (i in 1:length(date))
  {
    mtx = rbind(mtx, getURLsOnDay(search, date[i], i))
  }

  library(foreign)
  filename = paste(search, "_titles.csv", sep= "")
  df = data.frame(mtx)
  write.csv(df, filename)
  
  return(df)
}


#------------------------------------------------------
# Return the article URLs from a given date
#------------------------------------------------------
getURLsOnDay <- function(search, rawDate, hashCode)
{
  sleep = 0.1/12 # number of keys is 12
  root = "http://api.nytimes.com/svc/search/v2/articlesearch.json?fq="
  date = format(as.Date(rawDate), format="%Y%m%d")
  key = getKey(hashCode)
  url = paste(root,search,"&begin_date=",date,"&end_date=",date,"&sort=oldest&api-key=",key, sep='')
  
  data = fromJSON(url) # call minimally
  
  if (data$response$meta$hits > 0)
  {
    web_urls = data$response$docs$web_url
    headlines = data$response$docs$headline$main
    articleDate = date
    mtx = cbind(articleDate, headlines, web_urls)
    Sys.sleep(sleep) #API allows only 10 calls per second
    df = data.frame(mtx)
    return(df)
  }
  
}