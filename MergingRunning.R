# Jacob Holman
# Merging data
# 12/26/2015 - 1/9/2016

# To do:
#    Make checks for variables ex: place is NA or numeric
#    Read in other aid stations ex: Mosquito
#    incoorporate bin age into inRead
#    create toTime function going from seconds to actual time
#    problems with 2003!!!!

# Data from 1991

# Objective of code
#    Learn to read in and merge different types of data 
#        (csv, tab delimited, and other)
#    Manipulate data into a working format
#    Predict arrival times at aid stations given finish time

setwd("~/Projects/WesterStatesData")
library(plyr)
library(stringr)

# takes in "intime outtime" returns "intime"
# input: character vector of the form "HH:MM:SS-HH:MM:SS"
# output: character vector "HH:MM:SS"
# function returns everthing before the first "-"
inTime <- function(x){
  if (!is.character(x) && !is.na(x)) stop("x must be a character string of the 
                                          form H:MM:SS-HH:MM:SS")
  unlist(lapply(x, function(i){
    if (is.na(i))
      NA
    else
      strsplit(i, '-', fixed=T)[[1]][1]}))
}

# applied to only the later aid station times ***************************************************************
# if the number is less than 6 hours then add 24 hours
# this function is for the times between 5 am and 11 am on Sunday
weirdTimechange <- function(x){
  unlist(lapply(x,function(i){
    if (is.na(i))
      NA
    else if (i <= (6*3600))
      i + 24*3600
    else
      i
    }  )  )
}


# from "HH:MM:SS" to seconds *****************************************************************************************
# input: character vector of the form "HH:MM:SS" (and NAs)
# output: numeric vector of seconds
toSeconds <- function(x){
  if (!is.character(x) && !is.na(x)) stop("x must be a character
                                          string of the form H:M:S")
  unlist(
    lapply(x,
           function(i){
             if (is.na(i))
               NA
             else{
               i <- as.numeric(strsplit(i,':',fixed=TRUE)[[1]])
               if (length(i) == 3) 
                 i[1]*3600 + i[2]*60 + i[3]
               else if (length(i) == 2)
                 i[1]*3600 + i[2]*60
               else 
                 NA}
            }  
        )  
    )    
} 


# same as toSeconds function except subtracts 5 hours *********************************************************************
LtoSeconds <- function(x){
  if (!is.character(x) && !is.na(x)) stop("x must be a character 
                                          string of the form H:M:S")
  if (length(x)<=0)return(x)
  unlist(
    lapply(x,
           function(i){
             i <- as.numeric(strsplit(i,':',fixed=TRUE)[[1]])
             if (length(i) == 3) 
               ((i[1]-5)*3600 + i[2]*60 + i[3]) %% (24*3600)
             else if (length(i) == 2)
               ((i[1]-5)*3600 + i[2]*60) %% (24*3600)
             else 
               NA
            }  
        )  
    )   
} 

# from seconds to HH:MM:SS
# input: numeric vector of seconds (and NAs)
# output: character vector of the form "HH:MM:SS"
outSeconds <- function(x){
  if (!is.numeric(x) && !is.na(x)) stop("x must be an integer in seconds")
  unlist(lapply(x, function(i) {
    if (is.na(i))
      NA
    else paste(c(i %/% 3600, (i%%3600) %/% 60, (i%%3600) %% 60),
               collapse = ":")}))
}

# bin age function
# input: numeric vector of ages (and NAs)
# output: character vector of age group same size
# function takes in ages and for each age outputs the age grouping
binAge <- function(x){
  if (!is.numeric(x) && !is.na(x)) stop("x must be an integer of age")
  n = length(x)
  for(i in 1:n){
    if (is.na(x[i]))
      NA
    else if (x[i] < 30)
      x[i] <- "< 30"
    else if (x[i] >= 30 && x[i] < 35)
      x[i] <- "30-35"
    else if (x[i] >= 35 && x[i] < 40)
      x[i] <- "35-40"
    else if (x[i] >= 40 && x[i] < 45)
      x[i] <- "40-45"
    else if (x[i] >= 45 && x[i] < 50)
      x[i] <- "45-50"
    else if (x[i] >= 50)
      x[i] <- ">50"
    else
      x[i] <- NA
    }
  x
}

# checks to see if vars and varnms are correct
# shows the entire data frame read in
# also matches the chosen titles matched to the first 4 rows of the given data frame column
checker <- function(year, vars, varnms, del){
  fileName <- paste("wser", year, sep = "")
  df <- read.table(fileName, header = F, sep = del, quote = "\"", 
                   na.strings = c("", "NA", "DNF", "DNS", "--:--",
                                  "--:-----:--"), 
                   fill = T, strip.white = T, stringsAsFactors = F)
  View(df)
  rbind(nms[varnms], head(df,4)[vars])
}

# General function that reads in data set and makes it workable
# character-year:        as "yyyy.***" (*** is file type)
# numeric vector-vars:   column numbers desired
# numeric vector-varnms: numbers correspoding to the titles in nms
# character-del:         delimeter for the file
# logical - plus5, genNA, Onenm, intm
#   plus5 if true then the aid station times are stored as actual time
#   genNA - if true then male is NA and female is F
#   Onenm - if 1 then Name is stored as "LastNmae, FirstName"
#           if 2 then Name is stored as "FirstName LastName"
#   intm - if true then varible is stored as "HH:MM:SS-HH:MM:SS"
inRead <- function(year, vars, varnms, begin, del, plus5, genNA, Onenm, intm){
  fileName <- paste("wser", year, sep = "")
  df <- read.table(fileName, header = F, sep = del, quote = "\"", 
                   na.strings = c("", "NA", "DNF", "DNS", "--:--",
                                  "--:-----:--"), skip = begin-1, 
                   fill = T, strip.white = T, stringsAsFactors = F)
  vars2 <- paste(rep('V',length(vars)), as.character(vars), sep = "")
  df <- df[,vars2]
  names(df) <- nms[varnms]
  
  
  # change all H:M:s to seconds ******************************************************************************************
  numvars = sum(varnms %in% 7:27)
  start = sum(varnms %in% 1:6)+1
  if (28 %in% varnms)
    start = start +1
  
  rangvars <- c(start:(start+numvars-1))
  #rangvars <- c(6:13)
  if (plus5){
    df[,rangvars] <- lapply(df[,rangvars], LtoSeconds)
    
    numbfvars1 <- sum(varnms %in% 7:19)
    start2 <- start+numbfvars1
    numvars2 <- sum(varnms %in% 20:27)
    rangvars2 = c(start2:(start2+numvars2-1))
    df[,rangvars2] <- lapply(df[,rangvars2], weirdTimechange)
  }else if (intm){
    df[,rangvars] <- lapply(df[,rangvars], inTime)
    df[,rangvars] <- lapply(df[,rangvars], toSeconds)
  }else
    df[,rangvars] <- lapply(df[,rangvars], toSeconds)
  
  df$Time <- toSeconds(df$Time) *****************************************************************************************
  
  # Gender NA to M and everything else to F
  if(genNA)
    df$Gender <- as.factor(unlist(lapply(
      df$Gender,function(i){if(is.na(i)){"M"}else{"F"}})))
  
  # separate one name variable
  if (Onenm == 1){
    df$Last.Name <- unlist(lapply(df$Name,
                           function(i){strsplit(i, ', ', fixed=T)[[1]][1]}))
    df$First.Name <- lapply(df$Name,
                            function(i){strsplit(i, ', ',fixed=T)[[1]][-1]})
    
    df$First.Name <- unlist(lapply(df$First.Name, 
                                  function(i){paste(i, collapse=" ")}))
    df$Name <- NULL
  }
  if (Onenm == 2){
    df$First.Name <- unlist(lapply(df$Name, 
                            function(i){strsplit(i, ' ', fixed=T)[[1]][1]}))
    df$Last.Name <- lapply(df$Name, 
                           function(i){strsplit(i, ' ', fixed=T)[[1]][-1]})
    df$Last.Name <- unlist(lapply(df$Last.Name, 
                                  function(i){paste(i, collapse=" ")}))
    df$Name <- NULL
    
  }
  
  # add year as variable and return data frame in order
  df$year <- strsplit(year,'.',fixed=TRUE)[[1]][1]
  df
}

# reads in 2003 data
inRead2003 <- function(){
  df <- read.table(text = gsub('\\s{2,}', ';', readLines("wser2003.txt")), 
  header = F, sep = ';', quote = "\"", na.strings = c("", "NA", "DNF", "DNS", "--:--","--:-----:--"), 
  fill = T, strip.white = T, blank.lines.skip = T, stringsAsFactors = F)
  
  # until row 286
  vars <- c(1,5,2,3,11,13,14,15,16,17)
  varnms <- c(1,2,4,3,8,14,16,17,21,25) 
  df1 <- df[1:286,]
  vars2 <- paste(rep('V',length(vars)), as.character(vars), sep = "")
  df1 <- df1[,vars2]
  names(df1) <- nms[varnms]
  
  df2 <- df[287:length(df$V1),]
  vars <- c(1:3,5,9,11,12,13,14,15)
  vars2 <- paste(rep('V',length(vars)), as.character(vars), sep = "")
  df2 <- df2[,vars2]
  names(df2) <- nms[varnms]
  
  df <- rbind(df1, df2)
  df[,5:10] <- lapply(df[,5:10], function(x){
    unlist(lapply(x, function(i){strsplit(i, ' ', fixed=T)[[1]][3]}))})
  
  
  df[,5:10] <- lapply(df[,5:10], LtoSeconds) ***********************************************************************
  df[,9:10] <- lapply(df[,9:10], weirdTimechange)
  df$Time <- toSeconds(df$Time)
  
  df <- df[!(df$Place == "Pl"),]
  
  df$year <- "2003"
  df
  
}

nms <- c("Place",             "Time",               "First.Name", 
         "Last.Name",         "Gender",             "Age",
         "Lyon.Ridge",        "Red.Star.Ridge",     "Duncan.Canyon", 
         "Robinson.Flat",     "Miller's.Defeat",    "Dusty.Corners", 
         "Last.Chance",       "Devil's.Thumb",      "El.Dorado.Creek", 
         "Michigan.Bluff",    "Foresthill",         "Dardanelles.(Cal-1)",
         "Peachstone.(Cal-2)","Ford's.Bar.(Cal-3)", "Rucky.Chucky", 
         "Green.Gate",        "Auburn.Lake.Trails", "Brown's.Bar",
         "Highway.49",        "No.Hands.Bridge",    "Robie.Point",
         "Name")

# Read in all data from 1986-2002 # tested max and mins for 86 and all look good
data86to02 <- function(){
  # 1986 
  #checker("1986.txt", c(1,33,2, 4:6, 8,12,16,18,22,24,26,30), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), "\t")
  df <- inRead("1986.txt", c(1,33,2, 4:6, 8,12,16,18,22,24,26,30), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), 
  4, "\t", TRUE, TRUE, TRUE, FALSE) 
  merged86 <- df
  
  # 1987  good
  #checker("1987.txt", c(1,37,2, 4:6, 8,12,16,18,22,26,30,34), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), "\t")
  df <- inRead("1987.txt", c(1,37,2, 4:6, 8,12,16,18,22,26,30,34), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), 
  4, "\t", TRUE, TRUE, TRUE, FALSE)
  merged87 <- rbind(merged86, df)
  rm(merged86)
  
  # 1988  good
  #checker("1988.txt", c(1,35,2, 4:6, 8,10,14,16,20,22,26,30), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), "\t")
  df <- inRead("1988.txt", c(1,35,2, 4:6, 8,10,14,16,20,22,26,30), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), 
  4, "\t", T, T, T,F)
  merged88 <- rbind(merged87, df)
  rm(merged87)
  
  # 1989 good
  #checker("1989.txt", c(1,37,2, 4:6, 8,10,14,16,20,24,28,32), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), "\t")
  df <- inRead("1989.txt", c(1,37,2, 4:6, 8,10,14,16,20,24,28,32), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), 
  4, "\t", T, T, T,F)
  merged89 <- rbind(merged88, df)
  rm(merged88)
  
  # 1990 bad get rid of text in "Place" variable
  #checker("1990.txt", c(1,39,2, 4:6, 8,12,16,18,22,26,30,34), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), "\t")
  df <- inRead("1990.txt", c(1,39,2, 4:6, 8,12,16,18,22,26,30,34), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), 
  4, "\t", T,T,T,F)
  merged90 <- rbind(merged89, df)
  rm(merged89)
  
  # 1991 good
  #checker("1991.txt", c(1,39,2, 4:6, 8,12,16,20,24,28,32,34), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), "\t")
  df <- inRead("1991.txt", c(1,39,2, 4:6, 8,12,16,20,24,28,32,34), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), 
  4, "\t",T,T,T,F)
  merged91 <- rbind(merged90, df)
  rm(merged90)
  
  # 1992 warning but good
  #checker("1992.txt", c(1,39,2, 4:6, 8,12,16,20,24,28,32,34), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), "\t")
  df <- inRead("1992.txt", c(1,39,2, 4:6, 8,12,16,20,24,28,32,34), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), 
  4, "\t",T,T,T,F)
  merged92 <- rbind(merged91, df)
  rm(merged91)
  
  # 1993 good
  #checker("1993.txt", c(1,39,2, 4:6, 8,12,16,18,22,26,30,34), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), "\t")
  df <- inRead("1993.txt", c(1,39,2, 4:6, 8,12,16,18,22,26,30,34), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), 
  4, "\t",T,T,T,F)
  merged93 <- rbind(merged92, df)
  rm(merged92)
  
  # 1994 good
  #checker("1994.txt", c(1,41,2, 4:6, 8,12,16,20,24,28,32,36), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), "\t")
  df <- inRead("1994.txt", c(1,41,2, 4:6, 8,12,16,20,24,28,32,36), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), 
  4, "\t",T,T,T,F)
  merged94 <- rbind(merged93, df)
  rm(merged93)
  
  # 1995 good
  #checker("1995.txt", c(1,41,2, 4:6,12,16,20,24,28,32,36), c(1,2,28,5,6,9,13,14,16,17,21,23,25), "\t")
  df <- inRead("1995.txt", c(1,41,2, 4:6,12,16,20,24,28,32,36), c(1,2,28,5,6,9,13,14,16,17,21,23,25), 
  4, "\t",T,T,T,F)
  merged95 <- rbind.fill(merged94, df)
  rm(merged94)
  
  # 1996 good
  #checker("1996.txt", c(1,41,2, 4:6, 8,12,16,20,24,28,32,36), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), "\t")
  df <- inRead("1996.txt", c(1,41,2, 4:6, 8,12,16,20,24,28,32,36), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), 
  4, "\t",T,T,T,F)
  merged96 <- rbind.fill(merged95, df)
  rm(merged95)
  
  # 1997 good
  #checker("1997.txt", c(1,41,2, 4:6, 8,12,16,20,24,28,32,36), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), "\t")
  df <- inRead("1997.txt", c(1,41,2, 4:6, 8,12,16,20,24,28,32,36), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), 
  4, "\t",T,T,T,F)
  merged97 <- rbind.fill(merged96, df)
  rm(merged96)
  
  # 1998 good
  #checker("1998.txt", c(1,37,2, 4:5, 8,12,16,20,24,28,32), c(1,2,28,5,6,13,14,16,17,21,23,25), "\t")
  df <- inRead("1998.txt", c(1,37,2, 4:5, 8,12,16,20,24,28,32), c(1,2,28,5,6,13,14,16,17,21,23,25), 
  4, "\t",T,T,T,F)
  merged98 <- rbind.fill(merged97, df)
  rm(merged97)
  
  
  # 1999 good
  #checker("1999.txt", c(1,33,2, 4:6, 8,10,12,16,20,22,26,28), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), "\t")
  df <- inRead("1999.txt", c(1,33,2, 4:6, 8,10,12,16,20,22,26,28), c(1,2,28,5,6,8,10,13,14,16,17,21,23,25), 
  4, "\t",T,T,T,F)
  merged99 <- rbind.fill(merged98, df)
  rm(merged98)
  
  # 2000 good Red star ridge is lower than other years
  #checker("2000.txt", c(1,43,2, 4:6, 8,12,14,18,22,26,30,34,38), c(1,2,28,5,6,8,10,13,14,16,17,19,21,23,25), "\t")
  df <- inRead("2000.txt", c(1,43,2, 4:6, 8,12,14,18,22,26,30,34,38), c(1,2,28,5,6,8,10,13,14,16,17,19,21,23,25), 
  4, "\t",T,T,T,F)
  merged00 <- rbind.fill(merged99, df)
  rm(merged99)
  
  # 2001 good
  #checker("2001.txt", c(1,43,2, 4:6, 8,12,16,20,24,26,30,34,38), c(1,2,28,5,6,8,10,13,14,16,17,19,21,23,25), "\t")
  df <- inRead("2001.txt", c(1,43,2, 4:6, 8,12,16,20,24,26,30,34,38), c(1,2,28,5,6,8,10,13,14,16,17,19,21,23,25), 
  4, "\t",T,T,T,F)
  merged01 <- rbind.fill(merged00, df)
  rm(merged00)
  
  # 2002 good
  #checker("2002.txt", c(1,37,2, 4:6, 10,12,16,20,24,26,30,34), c(1,2,28,5,6,8,13,14,16,17,19,21,23,25), "\t")
  df <- inRead("2002.txt", c(1,37,2, 4:6, 10,12,16,20,24,26,30,34), c(1,2,28,5,6,8,13,14,16,17,19,21,23,25), 
  4, "\t",T,T,T,F)
  merged02 <- rbind.fill(merged01, df)
  rm(merged01)
  
  merged02
}

# 2003 probably good

# Read in all data from 2004-2015
data04to15 <- function(){
  # 2004 good
  #checker("2004.csv", c(1,38,2,3,6,8,12,16,18,22,26,28,32), c(1,2,4,3,8,10,13,14,16,17,21,23,25), ",")
  df <- inRead("2004.csv", c(1,38,2,3,6,8,12,16,18,22,26,28,32), c(1,2,4,3,8,10,13,14,16,17,21,23,25),
  3, ",",T,F,F,F)
  merged04 <- rbind.fill(merged03, df)
  rm(merged03)
  
  #2005 (3rd row, v-1,2,5,49) good
  #checker("2005.csv", c(1,49,2,5,7,9,15,17,21,25,27,31,33,35,37,41,43,45),c(1,2, 28, 7:8, 10, 12:14, 16:17, 19, 21:26), ",")
  df <- inRead("2005.csv", c(1,49,2,5,7,9,15,17,21,25,27,31,33,35,37,41,43,45),c(1,2, 28, 7:8, 10, 12:14, 16:17, 19, 21:26), 
  3, ",",T,F,2,F)
  merged05 <- rbind.fill(merged04, df)
  rm(merged04)
  
  #2006 (3rd row, v-1,2,5,6,44) good
  #checker("2006.csv",c(1,44,2,4,5,6,8,10,12,14,18,22,26,30,32,34,38,40), c(1,2,28,5,6,7,8,9,10,13,14,16,17,19,21,23,24,25), ",")
  df <- inRead("2006.csv", c(1,44,2,4,5,6,8,10,12,14,18,22,26,30,32,34,38,40), c(1,2,28,5,6,7,8,9,10,13,14,16,17,19,21,23,24,25), 
  3, ",",T,F,2,F)
  merged06 <- rbind.fill(merged05, df)
  rm(merged05)
  
  #2007 (4th row, v-1,2,4,5,6,7,13) good
  #checker("2007.csv", c(1,2,4,5,6,7,13,18,23,28,38,48,53,63,73,78,88,98,103,108,128,133,138,143,153), c(1:19, 21:26), ",")
  df <- inRead("2007.csv", c(1,2,4,5,6,7,13,18,23,28,38,48,53,63,73,78,88,98,103,108,128,133,138,143,153), c(1:19, 21:26), 
  4, ",",T,F,F,F)
  merged07 <- rbind.fill(merged06, df)
  rm(merged06)
  
  #2009 (4th row, v-1,2,4,5,6,7,12) good
  #checker("2009.csv", c(1,2,4,5,6,7,12,14,16,18,20,22,24,26,28,30,32,34,36,38,42,44,46,48,50), c(1:19,21:26), ",")
  df <- inRead("2009.csv", c(1,2,4,5,6,7,12,14,16,18,20,22,24,26,28,30,32,34,36,38,42,44,46,48,50), c(1:19,21:26), 
  4, ",",T,F,F,F)
  merged09 <- rbind.fill(merged07, df)
  rm(merged07)
  
  #2010 (2nd row, v-1,2,4,5,6,7) good
  #checker("2010.csv", c(1,2,4,5,6,7,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48), c(1:6,9:17,19,21:27), ",")
  df <- inRead("2010.csv", c(1,2,4,5,6,7,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48), c(1:6,9:17,19,21:27), 
  2, ",",F,F,F,F)
  merged10 <- rbind.fill(merged09, df)
  rm(merged09)
  
  #2011 (3rd row, v-1,2,4,5,6,7) good
  #checker("2011.csv", c(1,2,4,5,6,7,15,19,21,23,25,27,29,31,33,34,36,38,40,42,44,46), c(1:6,9,11:19,21:25,27), ",")
  df <- inRead("2011.csv", c(1,2,4,5,6,7,15,19,21,23,25,27,29,31,33,34,36,38,40,42,44,46), c(1:6,9,11:19,21:25,27), 
  3, ",",F,F,F,F)
  df <- df[1:375,]
  merged11 <- rbind.fill(merged10, df)
  rm(merged10)
  
  #2012 (3rd row, v-1,2,4,5,6,7,13) good
  #checker("2012.csv", c(1,2,4,5,6,7,13,15,17,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49), c(1:9,11:17,19,21:27), ",")
  df <- inRead("2012.csv", c(1,2,4,5,6,7,13,15,17,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49), c(1:9,11:17,19,21:27), 
  3, ",",F,F,F,F)
  merged12 <- rbind.fill(merged11, df)
  rm(merged11)
  
  #2013 (3rd row, v-1,2,4,5,6,7,13) good
  #checker("2013.csv", c(1,2,4,5,6,7,seq(13,49,2)), c(1:17,19,21:27), ",")
  df <- inRead("2013.csv", c(1,2,4,5,6,7,seq(13,49,2)), c(1:17,19,21:27), 3, ",",F,F,F,F)
  merged13 <- rbind.fill(merged12, df)
  rm(merged12)
  
  #2014 (2nd row, v-1,2,4,5,6,7,13) good
  #checker("2014.csv", c(1,2,4:7,seq(13,39,2),seq(43,53,2)), c(1:19,21:27), ",")
  df <- inRead("2014.csv", c(1,2,4:7,seq(13,39,2),seq(43,53,2)), c(1:19,21:27), 2, ",",F,F,F,T)
  merged14 <- rbind.fill(merged13, df)
  rm(merged13)
  
  #2015 (2nd row, v-1,2,4,5,6,7,13) good
  #checker("2015.csv", c(1,2,4,5,6,7,seq(13,51,2)), c(1:19,21:27), ",")
  df <- inRead("2015.csv", c(1,2,4,5,6,7,seq(13,51,2)), c(1:19,21:27), 2, ",",F,F,F,T)
  merged15 <- rbind.fill(merged14, df)
  rm(merged14)
  
  merged15
}

merged02 <- data86to02()
df <- inRead2003()
merged03 <- rbind.fill(merged02, df)
rm(merged02)

merged15 <- data04to15()
merged15 <- rbind.fill(merged03,merged15)


WestStat100 <- merged15[,c(16,1,2,15,14,3,4,19,5,17,6,24,20,7,8,25,9,10,26,18,11,21,12,22,13,23,27)]

write.csv(WestStat100, "wserAll.csv")

# bin ages
merged$AgeBin <- binAge(merged$Age)





# redo analysis
Adata <- merged[,c("Time", "AgeBin")]
Adata <- Adata[rowSums(is.na(Adata)) == 0,]
Adata$AgeBin <- as.factor(Adata$AgeBin)

median(Adata$Time[Adata$AgeBin == ">50"])
median(Adata$Time[Adata$AgeBin == "< 30"])
wilcox.test(Adata$Time[Adata$AgeBin == ">50"]^7,
            Adata$Time[Adata$AgeBin == "45-50"]^7)
hist(Adata$Time[Adata$AgeBin == ">50"]^7)
hist(Adata$Time[Adata$AgeBin == "45-50"]^7)

plot(Time~AgeBin, data = Adata)

# Variance assumption
library(car)
bartlett.test((Time)^7 ~ AgeBin, data= Adata)
leveneTest((Time)^7 ~ AgeBin, data = Adata)
fligner.test((Time)^7 ~ AgeBin, data = Adata) #good

# normaility assumption
fit <- aov(Time^7 ~ AgeBin, data=Adata)
plot(fit)
shapiro.test(resid(fit)) # does not pass normality assumption
qqnorm(resid(fit))
qqline(resid(fit)) # very off qqplot

kruskal.test((Time)^7 ~ AgeBin, data = Adata)

# fun over 60
uncletime <- c(merged$Time[merged$Last.Name == "Holman"][6],
               merged$Lyon.Ridge[merged$Last.Name == "Holman"][6])

Bdata <- merged[,c("Time", "Age", "Lyon.Ridge", "year", "Last.Name")]
Bdata <- Bdata[rowSums(is.na(Bdata)) == 0,]
over60 <- subset(Bdata, Age >= 60)
length(over60$Time)
summary(over60$Time)
gover60 <- subset(over60, Time <= 24*3600)
View(gover60)

plot(Time~Lyon.Ridge, data=merged)
abline(h=24*3600)
abline(h=uncletime[1], col="red")
abline(v=uncletime[2], col="red")
abline(lm(merged$Time~merged$Lyon.Ridge), col="green")
abline(h = mean(subset(merged$Time, 
                       (merged$Age >= 60) & (!is.na(merged$Time)) )),
       col = "blue")
# number of runners 60 and above who finished under 24 hours
sum((subset(merged$Time,
            (merged$Age >= 60) & (!is.na(merged$Time))) <= 24*3600))
hist(merged$Time)
abline(v=24*3600)

outSeconds(mean(merged$Lyon.Ridge[!is.na(merged$Lyon.Ridge)]))
