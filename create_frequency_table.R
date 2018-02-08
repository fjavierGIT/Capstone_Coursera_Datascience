library("parallel")
library("quanteda")
library("data.table")
library("readr")
library("sqldf")
library("gsubfn")
library("stringr")
library("stringi")

#READ FILES
ptm <- proc.time()
myDirectory <- paste0(getwd(),'/Project/final/en_EN')   ### Modify for each language
listf <- list.files(path = myDirectory, full.names = T)
text_raw <- mclapply(listf, readr::read_lines)

resample <- list()
set.seed(100)
resample[[1]] <- sample(1:length(text_raw[[1]]),size = length(text_raw[[1]])*.80,replace=FALSE)
resample[[2]] <- sample(1:length(text_raw[[2]]),size = length(text_raw[[2]])*.80,replace=FALSE)
resample[[3]] <- sample(1:length(text_raw[[3]]),size = length(text_raw[[3]])*.80,replace=FALSE)

train<-list() #Create Train data set
train[[1]] <- lapply(text_raw[1],function(x){x[resample[[1]]]})
train[[2]] <- lapply(text_raw[2],function(x){x[resample[[2]]]})
train[[3]] <- lapply(text_raw[3],function(x){x[resample[[3]]]})

test<-list() #Create Test data set
test[[1]] <- lapply(text_raw[1],function(x){x[-resample[[1]]]})
test[[2]] <- lapply(text_raw[2],function(x){x[-resample[[2]]]})
test[[3]] <- lapply(text_raw[3],function(x){x[-resample[[3]]]}) 
test <- unlist(test)

#Save objects for later use
save(list = c("text_raw"),file = paste0(getwd(),"/EN/text_raw_EN.RData"))
save(list = c("train"),file = paste0(getwd(),"/EN/train_EN.RData"))
save(list = c("test"),file = paste0(getwd(),"/EN/test_EN.RData"))

#CREATE CORPUS, SAMPLE
myCorpus_1 <- corpus(char_tolower(unlist(train[1]),compress=F))
myCorpus_2 <- corpus(char_tolower(unlist(train[2]),compress=F))
myCorpus_3 <- corpus(char_tolower(unlist(train[3]),compress=F))
save(list = c("myCorpus_1","myCorpus_2","myCorpus_3"),file=paste0(getwd(),"/EN/myCorpuses.RData"))
corp_clean_1 <- myCleanCorpus(myCorpus_1) 
save(list = c("corp_clean_1"), file=paste0(getwd(),"/EN/corpusClean_1.RData"))
corp_clean_2 <- myCleanCorpus(myCorpus_2) 
save(list = c("corp_clean_2"), file=paste0(getwd(),"/EN/corpusClean_2.RData"))
corp_clean_3 <- myCleanCorpus(myCorpus_3) 
save(list = c("corp_clean_3"), file=paste0(getwd(),"/EN/corpusClean_3.RData"))
rm(myCorpus_1,myCorpus_2,myCorpus_3) ##CAUTION !!! Frees up memory

tok1 <- tokens(corp_clean_1, verbose = TRUE,
               remove_numbers = TRUE, remove_punct = TRUE,
               remove_symbols = TRUE, remove_separators = TRUE,
               remove_twitter = TRUE, remove_hyphens = TRUE, 
               remove_url = TRUE)
save(list = c("tok1"),file=paste0(getwd(),"/EN/tok_1.RData"))
tok2 <- tokens(corp_clean_2, verbose = TRUE,
               remove_numbers = TRUE, remove_punct = TRUE,
               remove_symbols = TRUE, remove_separators = TRUE,
               remove_twitter = TRUE, remove_hyphens = TRUE, 
               remove_url = TRUE)
save(list = c("tok2"),file=paste0(getwd(),"/EN/tok_2.RData"))
tok3 <- tokens(corp_clean_3, verbose = TRUE,
               remove_numbers = TRUE, remove_punct = TRUE,
               remove_symbols = TRUE, remove_separators = TRUE,
               remove_twitter = TRUE, remove_hyphens = TRUE, 
               remove_url = TRUE)
save(list = c("tok3"),file=paste0(getwd(),"/EN/tok_3.RData"))
rm(corp_clean_1,corp_clean_2,corp_clean_3,tok2,tok3)  #CAUTION !!!

tokens_1_3 <- tokens_ngrams(tok1, n = 1:3, concatenator = " ")
save(list = c("tokens_1_3"),file=paste0(getwd(),"/EN/tokens_1_3.RData"))
rm(tokens_1_3)
tokens_1_4 <- tokens_ngrams(tok1, n = 4, concatenator = " ")
save(list = c("tokens_1_4"),file=paste0(getwd(),"/EN/tokens_1_4.RData"))
rm(tokens_1_4)
tokens_1_5 <- tokens_ngrams(tok1, n = 5, concatenator = " ")
save(list = c("tokens_1_5"),paste0(getwd(),"/EN/tokens_1_5.RData"))
rm(tokens_1_5,tok1)

load("/media/DATA/capstone/DE/tok_2.RData")
tokens_2_3 <- tokens_ngrams(tok2, n = 1:3, concatenator = " ")
save(list = c("tokens_2_3"),paste0(getwd(),"/EN/tokens_2_3.RData"))
rm(tokens_2_3)
tokens_2_4 <- tokens_ngrams(tok2, n = 4, concatenator = " ")
save(list = c("tokens_2_4"),paste0(getwd(),"/EN/tokens_2_4.RData"))
rm(tokens_2_4)
tokens_2_5 <- tokens_ngrams(tok2, n = 5, concatenator = " ")
save(list = c("tokens_2_5"),paste0(getwd(),"/EN/tokens_2_5.RData"))
rm(tokens_2_5,tok2)

load("/media/DATA/capstone/DE/tok_3.RData")
tokens_3_3 <- tokens_ngrams(tok3, n = 1:3, concatenator = " ")
save(list = c("tokens_3_3"),paste0(getwd(),"/EN/tokens_3_3.RData"))
rm(tokens_3_3)
tokens_3_4 <- tokens_ngrams(tok3, n = 4, concatenator = " ")
save(list = c("tokens_3_4"),paste0(getwd(),"/EN/tokens_3_4.RData"))
rm(tokens_3_4)
tokens_3_5 <- tokens_ngrams(tok3, n = 5, concatenator = " ")
save(list = c("tokens_3_5"),paste0(getwd(),"/EN/tokens_3_5.RData"))
rm(tokens_3_5,tok3)
proc.time() - ptm


#CREATE DFM
## 1
load(paste0(getwd(),"/EN/tokens_1_3.RData"))
myDFM1_3 <- create_dfm(tokens_1_3);rm(tokens_1_3)
save(list = c("myDFM1_3"),file=paste0(getwd(),"/EN/myDFM1_3.RData"))
rm(myDFM1_3)
load(paste0(getwd(),"/EN/tokens_1_4.RData"))
myDFM1_4 <- create_dfm(tokens_1_4);rm(tokens_1_4)
save(list = c("myDFM1_4"),file=paste0(getwd(),"/EN/myDFM1_4.RData"))
rm(myDFM1_4)
load(paste0(getwd(),"/EN/tokens_1_5.RData"))
myDFM1_5 <- create_dfm(tokens_1_5);rm(tokens_1_5)
save(list = c("myDFM1_5"),file=paste0(getwd(),"/EN/myDFM1_5.RData"))
rm(myDFM1_5)

load(paste0(getwd(),"/EN/myDFM1_3.RData"))
load(paste0(getwd(),"/EN/myDFM1_4.RData"))
load(paste0(getwd(),"/EN/myDFM1_5.RData"))
myFreqTable_1 <- rbind(myDFM1_3,myDFM1_4,myDFM1_5)
save(list = c("myFreqTable_1"),file=paste0(getwd(),"/EN/myFreqTable_1.RData"))
rm(myDFM1_3,myDFM1_4,myDFM1_5)
rm(myFreqTable_1)

## 2
load(paste0(getwd(),"/EN/tokens_2_3.RData"))
myDFM2_3 <- create_dfm(tokens_2_3);rm(tokens_2_3)
save(list = c("myDFM2_3"),file=paste0(getwd(),"/EN/myDFM2_3.RData"))
rm(myDFM2_3)
load(paste0(getwd(),"/EN/tokens_2_4.RData"))
myDFM2_4 <- create_dfm(tokens_2_4);rm(tokens_2_4)
save(list = c("myDFM2_4"),file=paste0(getwd(),"/EN/myDFM2_4.RData"))
rm(myDFM2_4)
load(paste0(getwd(),"/EN/tokens_2_5.RData"))
myDFM2_5 <- create_dfm(tokens_2_5);rm(tokens_2_5)
save(list = c("myDFM2_5"),file=paste0(getwd(),"/EN/myDFM2_5.RData"))
load(paste0(getwd(),"/EN/myDFM2_3.RData"))
load(paste0(getwd(),"/EN/myDFM2_4.RData"))
myFreqTable_2 <- rbind(myDFM2_3,myDFM2_4,myDFM2_5)
save(list = c("myFreqTable_2"),file=paste0(getwd(),"/EN/myFreqTable_2.RData"))
rm(myDFM2_3,myDFM2_4,myDFM2_5)
rm(myFreqTable_2)

## 3
load(paste0(getwd(),"/EN/tokens_3_3.RData"))
myDFM3_3 <- create_dfm(tokens_3_3);rm(tokens_3_3)
save(list = c("myDFM3_3"),file=paste0(getwd(),"/EN/myDFM3_3.RData"))
rm(myDFM3_3)
load(paste0(getwd(),"/EN/tokens_3_4.RData"))
myDFM3_4 <- create_dfm(tokens_3_4);rm(tokens_3_4)
save(list = c("myDFM3_4"),file=paste0(getwd(),"/EN/myDFM3_4.RData"))
rm(myDFM3_4)
load(paste0(getwd(),"/EN/tokens_3_5.RData"))
myDFM3_5 <- create_dfm(tokens_3_5);rm(tokens_3_5)
save(list = c("myDFM3_5"),file=paste0(getwd(),"/EN/myDFM3_5.RData"))
rm(myDFM3_5)
load(paste0(getwd(),"/EN/myDFM3_3.RData"))
load(paste0(getwd(),"/EN/myDFM3_4.RData"))
load(paste0(getwd(),"/EN/myDFM3_5.RData"))
myFreqTable_3 <- rbind(myDFM3_3,myDFM3_4,myDFM3_5)
save(list = c("myFreqTable_3"),file=paste0(getwd(),"/EN/myFreqTable_3.RData"))
rm(myDFM3_3,myDFM3_4,myDFM3_5)
rm(myFreqTable_3)

#Combine into 1 table
load(paste0(getwd(),"/EN/myFreqTable_1.RData"))
myFreqTable_1 <- pruneDT(myFreqTable_1)
load(paste0(getwd(),"/EN/myFreqTable_2.RData"))
myFreqTable_2 <- pruneDT(myFreqTable_2)
load(paste0(getwd(),"/EN/myFreqTable_3.RData"))
myFreqTable_3 <- pruneDT(myFreqTable_3)
myFreqTable <- rbind(myFreqTable_1,myFreqTable_2,myFreqTable_3)
save(list = c("myFreqTable_1","myFreqTable_2","myFreqTable_3"),file=paste0(getwd(),"/EN/myFreqTable_1_2_3.RData"))
rm(myFreqTable_1,myFreqTable_2,myFreqTable_3)
myFreqTable <- myFreqTable[, .(frequency=sum(frequency)),by=feature]

freqTab <- calculateScores() #Creates final frequency data.table
save(list = c("freqTab"),file=paste0(getwd(),"/EN/freqTab.RData"))

setorder(freqTab,-score)
setkey(freqTab,bas)

#DEFINE FUNCTIONS
myCleanCorpus <- function(corp) {
    corp_clean <- stringr::str_replace_all(corp,"-"," ") # replace dash with space
    corp_clean <- stringr::str_replace_all(corp_clean,"rt","") # remove twitter's rt
    corp_clean <- stringr::str_replace_all(corp_clean,"'","") ## eliminates ' character
    corp_clean <- stringr::str_replace_all(corp_clean,"[0-9]","") ## eliminates numbers
    corp_clean <- stringr::str_replace_all(corp_clean,"[^A-Za-z]"," ") ## eliminates non alphabetic
    corp_clean <- stringr::str_replace_all(corp_clean,"[^[:print:]]","") ##remove non-printable char
    corp_clean <- stringr::str_replace_all(corp_clean,"\\s"," ") ## collapse empty spaces to single space
    return(corp_clean)
}
wordcount <- function(str) {
    sapply(gregexpr("\\b\\W+\\b", str, perl=TRUE), function(x) sum(x>0) ) + 1 
}
create_dfm <- function(ngrams){  
    df <- data.table::data.table(textstat_frequency(dfm(ngrams,verbose = TRUE)))
    return(df)
}
pruneDT <- function(dtable) { 
    dtable <- dtable[,c("rank","docfreq","group"):=NULL]
    dtable <- dtable[frequency>10, ]
    return(dtable)
}

calculateScores <- function() {
    if (!is.null(getOption("sqldf.connection"))) sqldf() # close an old connection if it exists
    myFreqTable2 <- myFreqTable
    candquery0 <- paste0("select matched.feature as feab, matched.base as bas, matched.prediction as pred, matched.frequency as freq, matched.ntok as toks, ",
                         "case matched.ntok when 1 then 0 when 2 then 0.064 when 3 then 0.16 when 4 then 0.4 else 1 end as mult, ",
                         "case matched.ntok when 1 then 0 when 2 then 0.064 when 3 then 0.16 when 4 then 0.4 else 1 end * matched.frequency / candidate.frequency as score ", 
                         "from myFreqTable2 matched inner join myFreqTable2 candidate ",
                         "on matched.base = candidate.feature",sep=" ")
    tab <- as.data.table(sqldf::sqldf(candquery0));  setorder(tab,-score)
    setkey(tab,bas)
    return(tab)
}


