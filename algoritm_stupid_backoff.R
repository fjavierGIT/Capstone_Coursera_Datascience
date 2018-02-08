
matchData <- function(myEntry) {
    topFreq <- c("the","to","and","a","of","in","for","is")
    tokensFromChar <- paste(tokens(char_tolower(myEntry), verbose = F,
                                   remove_numbers = TRUE, remove_punct = TRUE,
                                   remove_symbols = TRUE, remove_separators = TRUE,
                                   remove_twitter = TRUE, remove_hyphens = TRUE, 
                                   remove_url = TRUE), collapse = " ")
    tokensFromChar <- gsub("'", "", tokensFromChar)
    len <- ifelse(wordcount(tokensFromChar)>4,4,wordcount(tokensFromChar))
    cand <- data.table()
    for(i in len:1) {  #iterates from 4 to 1 ngrams to find candidates
        input <- stringr::word(tokensFromChar,start=-i,end=-1)
        cand <- rbind(cand,freqTab[.(input),nomatch=0])
    }
    listPrediction <- unique(cand,by="pred")$pred[1:3]
    listPrediction[is.na(listPrediction)|listPrediction==""]<-sample(topFreq,size=sum(is.na(listPrediction)|listPrediction==""),replace = F)
    return(listPrediction)
}
