##  functions

findmatchgreg <- function(text,pattern){
    re <- gregexpr(pattern,text)
    rm <- regmatches(text, re)
    unlist(Filter(function(x) !identical(character(0),x), rm))
}

findmatchexec <- function(text,pattern){
    found <- findmatchgreg(text, pattern) 
    applied <- lapply(found,
                      function(x) {regmatches(x, regexec(pattern,x))[[1]][2]})
    unlist(Filter(function(x) !identical(character(0),x), applied))
}

lineLength <- function(filename){
    testcon <- file(filename,open="r")
    readsizeof <- 50000
    linesread <- 50000
    nooflines <- 0
    while(linesread==50000)
    {
        linesread <- length(readLines(testcon, readsizeof))
        nooflines <- nooflines + linesread
    }
    close(testcon)
    nooflines
}

get_training_set1 <- function(data, prob, seed){
    set.seed(seed)
    n <- length(data)  
    size <- 1
    inTrain  <- rbinom(n, size, prob)
    inTraining <- inTrain==1
    data[inTraining]
}

get_training_set  <- function(filename, seed, length, lineqty){
    
    set.seed(seed)
    sections <- 1024
    readsizeof <- floor(length/1024)
    
    n <- sections
    size <- 1 ## Each test is equivalent to a single coin flip.
    prob <- lineqty/length
    inTrain  <- rbinom(n, size, prob)
    inTraining <- inTrain==1
    inTesting <- inTrain!=1
    
    sectionnum <- c(1:sections)
    dftraining <- data.frame(sectionnum,inTraining)
    trainsections <- dftraining[dftraining$inTraining==1,]
    x <- trainsections$sectionnum
    
    testcon <- file(filename,open="r")
    
    list1 <- lapply(x,function(x){
        readLines(testcon, readsizeof,skip=x) })
    close(testcon)
    
    trainset <- c("")
    for(i in seq_along(x)){
        trainset <- c(trainset,list1[[i]])
    }
    trainset
}

get_small_set  <- function(filename, seed, length, lineqty=10){
    testcon <- file(filename3,open="r")
    trainset <- readLines(testcon, lineqty)
    close(testcon)
    trainset
}

get_dictionary <- function(data, minimum_word_count=3){
    pattern <- "[a-zA-Z']{1,15}"
    anyword <- findmatchgreg(data,pattern)
    wordquantity <- length(anyword)
    table1 <- table(anyword)
    #sorted <- sort(table1, decreasing=TRUE)
    ##  popular_dictionary is a named vector of word count
    ##  where the name is the Word.
    popular_dictionary <- table1[table1>=minimum_word_count]
    df <- data.frame(popular_dictionary)
    anyword <- as.character(df[,1])
    df[,"anyword"] <- anyword
    df[order(-df$Freq),]
}

build_data_base <- function(){ 
    
    #setwd("~/edu/Data Science/capstone/Quiz/Quiz2")
    
    library(tm)
    library(XML)
    options(warn = -1)
    
    ##  fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    ##  download.file(fileUrl, dest="zipData.zip")
    ##  unzip("zipData.zip", exdir=".")
    
    #offensive_url <- "https://raw.github.com/dotmh/Rudex/master/db.txt"
    #download.file(offensive_url , dest="offensive.txt")
    
    ## https://github.com/dwyl/english-words
    dictionary_url <- "https://github.com/dwyl/english-words.git"
    #download.file(dictionary_url , dest="dictionary.txt")
    #unzip("english-words-master.zip", exdir=".")
    
    
    # read.table("./english-words-master/words2.txt")
    # getwd()
    
    filename1 <- "./Data/en_US/en_US.blogs.txt"
    filename2 <- "./Data/en_US/en_US.news.txt"
    filename3 <- "./Data/en_US/en_US.twitter.txt"
    
    ##############################################################################
    ##############################################################################
    
    bloglineqty <-  lineLength(filename1)
    newslineqty <-  lineLength(filename2)
    twitlineqty <-  lineLength(filename3)
    
    training1 <- get_training_set(filename1, seed=334, length=bloglineqty, lineqty=100000) #200000
    training2 <- get_training_set(filename2, seed=334, length=newslineqty, lineqty=40000)   #20000
    training3 <- get_training_set(filename3, seed=334, length=twitlineqty, lineqty=200000) #300000
    
##  Use blogs and twitter for raw data.
##  Use news for dictionary purposes, especially for short words.
    training <- c(training1, training3)
    training <- get_sentences(training)
    training2 <- get_sentences(training2)
    ###########################################################################
    
    txts <- Corpus(VectorSource(training))
    newstxt <- Corpus(VectorSource(training2))
    
    txts <- tm_map(txts, stripWhitespace)
    txts <- tm_map(txts, content_transformer(tolower))
    offensive_words <- readLines("offensive.txt")
    txts <- tm_map(txts, removeWords, offensive_words)
    
    newstxt <- tm_map(newstxt, stripWhitespace)
    newstxt <- tm_map(newstxt, content_transformer(tolower))
    newstxt <- tm_map(newstxt, removeWords, offensive_words)
    
    str(txts[[100]]$content)
    txts[[100]]$content
    len <- length(txts)
    len
    x <- c(1:len)
    applied <- lapply(x,function(x) txts[[x]]$content)
    training <- unlist(applied)
    length(training)
    training <- data.frame(training)
    
    len_news <- length(newstxt)
    x <- c(1:len_news)
    applied2 <- lapply(x,function(x) newstxt[[x]]$content)
    training2 <- unlist(applied2)
    training2 <- data.frame(training2)
    
    write.csv(training,"training.csv",row.names=FALSE)
    write.csv(training2,"trainingnews.csv",row.names=FALSE)
    
    ##########################################################################
    
##  Single Word Dictionaries (1-grams)
    
    gram1freqa <- get_dictionary(training2,minimum_word_count=4)
    gramchar <- nchar(gram1freqa$anyword)
    gram1freqa8 <-  gram1freqa[gramchar>8, ]
    head(gram1freqa8)
    tail(gram1freqa8,10)
    dim(gram1freqa8)[1]
    # ##########################################################################
    
    gram1freqb <- get_dictionary(training2,minimum_word_count=10)
    gramchar <- nchar(gram1freqb$anyword)
    gram1freqb8 <-  gram1freqb[gramchar < 9, ]
    head(gram1freqb8)
    tail(gram1freqb8,10)
    dim(gram1freqb8)[1]
    
    gramchar <- nchar(gram1freqb8$anyword)
    gram1freqb48 <-  gram1freqb8[gramchar > 3, ]
   
    head(gram1freqb48)
    tail(gram1freqb48,10)
    dim(gram1freqb48)
    
    # ##########################################################################
    
    newsfreq <- get_dictionary(training2,minimum_word_count=40)
    newschar <- nchar(as.character(newsfreq$anyword))
    three_letter_words <-  newsfreq[newschar==3, ]
    
    head(three_letter_words)
    tail(three_letter_words,50)
    dim(three_letter_words)[1]
    
    # ##########################################################################
    # 
    two_letter_words100 <- get_dictionary(training2,minimum_word_count=250)
    newschar <- nchar(as.character(two_letter_words100$anyword))
    two_letter_words <-   two_letter_words100[newschar==2, ]
    
    head(two_letter_words)
    tail(two_letter_words,50)
    dim(two_letter_words)[1]
    
    ##########################################################################
    
    one_letter_words100 <- get_dictionary(training2,minimum_word_count=2000)
    newschar <- nchar(as.character(one_letter_words100$anyword))
    one_letter_words <-   one_letter_words100[newschar==1, ]
    
    head(one_letter_words)
    tail(one_letter_words,50)
    dim(one_letter_words)[1]
    
    ###########################################################################
    
    dictionary <- rbind(gram1freqa8,gram1freqb48,three_letter_words,
                        two_letter_words,one_letter_words)
    lowered <- tolower(dictionary$anyword)
    dictionary$anyword <- lowered
    dictionary <- dictionary[order(-dictionary$Freq,dictionary$anyword),]
    str(dictionary)
    head(dictionary)
    tail(dictionary,100)
    dim(dictionary)
    
    write.csv(dictionary,"dictionary.csv",row.names=FALSE)
    
    ##########################################################################
    traincolumns <- get_training_columns(training,6)
    head(traincolumns)
    write.csv(traincolumns,"traincolumns.csv",row.names=FALSE)
    #
    #
    gram2 <- get_2gram_table(traincolumns)
    #
    #gram2[gram2$first_word=="offense",]
    
    ###########################################################################
    
    gram2_sep1 <- get_2gram_sep(traincolumns, sep=1)
    #
    # head(gram2_sep1,20)
    # tail(gram2_sep1,20)
    #
    # gram2_sep1[gram2_sep1$first_word=="offense",]
    # gram2_sep1[gram2_sep1$second_word=="offense",]
    #
    gram2_sep2 <- get_2gram_sep(traincolumns, sep=2)
    # head(gram2_sep2,20)
    # tail(gram2_sep2,20)
    #
    # gram2_sep2[gram2_sep2$first_word=="offense",]
    # gram2_sep2[gram2_sep2$second_word=="offense",]
    #
    gram2_sep3 <- get_2gram_sep(traincolumns, sep=3)
    #  head(gram2_sep3,20)
    # #
    #  gram2_sep3[gram2_sep3$first_word=="offense",]
    #  gram2_sep3[gram2_sep3$second_word=="offense",]
    
    gram3 <- get_3gram_table(traincolumns)
    # head(gram3,200)
    
    write.csv(gram2,"gram2.csv",row.names=FALSE)
    write.csv(gram2_sep1,"gram2_sep1.csv",row.names=FALSE)
    write.csv(gram2_sep2,"gram2_sep2.csv",row.names=FALSE)
    write.csv(gram2_sep3,"gram2_sep3.csv",row.names=FALSE)
    write.csv(gram3,"gram3.csv",row.names=FALSE)
}

get_word_count <- function(data){
    pattern <- "[a-zA-Z']{1,15}"
    anyword <- findmatchgreg(data,pattern)
    length(anyword)
}

get_3_gram_count <- function(data){
    pattern <- "[a-zA-Z']{1,18}[ +]{1,2}[a-zA-Z']{1,18}[ +]{1,2}[a-zA-Z']{1,18}"
    any3 <- findmatchgreg(data,pattern)
    length(any3)
}


sentence_length <- function(sentences){
    len <- lapply(sentences, function(x) {length(strsplit(x, "\\s+")[[1]])})
    unlist(Filter(function(x) !identical(character(0),x), len))
}

removefirstword <- function(sentences){
    for(i in seq_along(sentences)){
        len <- sentence_length(sentences[i]) 
        lenvec <- 1:len
        if(len > 1){
            sentence <- ""
            for(j in seq_along(lenvec)){
                sentence[j] <- strsplit(sentences[i], "\\s+")[[1]][j]
            }
            sentences[i] <- paste(sentence[2:len], collapse=" ")
        } else {
            sentences[i] <- NA
        }
    }
    sentences
}

get_first_word <- function(sentences){
    for(i in seq_along(sentences)){
        len <- sentence_length(sentences[i]) 
        if(len > 1){
            sentences[i] <- strsplit(sentences[i], "\\s+")[[1]][1]
        } 
    }
    sentences
}

removefirstworda <- function(data){
    pattern <- "^[a-zA-Z']{1,128}\\s(.*)"
    findmatchexec(data, pattern)
}

reverse_word_order <- function(phrase){       
    words <- unlist(strsplit(phrase, "\\s+"))
    sentencelength <- length(words)
    back <- words[sentencelength]
    
    for(i in seq_along(words)){
        if(sentencelength>i){
            back[i+1] <- words[sentencelength-i]
        }
    }
    tolower(back)
}

get_sentences <- function(string_vector,
                          pattern="([a-zA-Z']{1,20})\\s[a-zA-Z' ]{1,70}\\s([a-zA-Z']{1,20})")
{
    findmatchgreg(string_vector, pattern) 
}

get_2gram_skip_n <- function(data,word_separation=0){
    
    if(word_separation==0){
        df12 <- data.frame(data[,1],data[,2])
        colnames(df12) <- c("word_1","word_2") 
        df23 <- data.frame(data[,2],data[,3])
        colnames(df23) <- c("word_1","word_2") 
        df34 <- data.frame(data[,3],data[,4])
        colnames(df34) <- c("word_1","word_2") 
        df45 <- data.frame(data[,4],data[,5])
        colnames(df45) <- c("word_1","word_2") 
        df56 <- data.frame(data[,5],data[,6])
        colnames(df56) <- c("word_1","word_2") 
        
        df <- rbind(df12,df23,df34,df45,df56)
        df
    }
    
    if(word_separation==1){
        df13 <- data.frame(data[,1],data[,3])
        colnames(df13) <- c("word_1","word_2") 
        df24 <- data.frame(data[,2],data[,4])
        colnames(df24) <- c("word_1","word_2") 
        df35 <- data.frame(data[,3],data[,5])
        colnames(df35) <- c("word_1","word_2") 
        df46 <- data.frame(data[,4],data[,6])
        colnames(df46) <- c("word_1","word_2") 
        
        df <- rbind(df13,df24,df35,df46)
        df
    }
    
    if(word_separation==2){
        df14 <- data.frame(data[,1],data[,4])
        colnames(df14) <- c("word_1","word_2") 
        df25 <- data.frame(data[,2],data[,5])
        colnames(df25) <- c("word_1","word_2") 
        df36 <- data.frame(data[,3],data[,6])
        colnames(df36) <- c("word_1","word_2") 
        
        df <- rbind(df14,df25,df36)
        df
    }
    
    if(word_separation==3){
        df15 <- data.frame(data[,1],data[,5])
        colnames(df15) <- c("word_1","word_2") 
        df26 <- data.frame(data[,2],data[,6])
        colnames(df26) <- c("word_1","word_2") 
        df <- rbind(df15,df26)
        df
    }
    
    if(word_separation==4){
        df <- data.frame(data[,1],data[,5])
        colnames(df) <- c("word_1","word_2") 
        df
    }
    
    ok <- complete.cases(df)
    df[ok,]
    
}

##  Get word colmuns from training set
get_training_columns <- function(string_vector,n=1){
    #n<-2
    #string_vector <- df[,1] 
    string_vector <- as.character(string_vector)
    
    applied <- lapply(string_vector,function(x) {strsplit(x, "\\s+")[[1]][1]})
    first_word <- unlist(Filter(function(x) !identical(character(0),x), applied))
    
    if(n>1){
        applied <- lapply(string_vector, function(x) {strsplit(x, "\\s+")[[1]][2]})
        second_word <- unlist(Filter(function(x) !identical(character(0),x), applied))
        df <- data.frame(first_word,second_word)
    }
    if(n>2){
        applied <- lapply(string_vector,function(x) {strsplit(x, "\\s+")[[1]][3]})
        third_word <- unlist(Filter(function(x) !identical(character(0),x), applied))
        df <- data.frame(first_word,second_word,third_word)
    }
    if(n>3){ 
        applied <- lapply(string_vector, function(x) {strsplit(x, "\\s+")[[1]][4]})
        fourth_word <- unlist(Filter(function(x) !identical(character(0),x), applied))
        df <- data.frame(first_word,second_word,third_word,fourth_word)
    }
    if(n>4){
        applied <- lapply(string_vector,
                          function(x) {strsplit(x, "\\s+")[[1]][5]})
        fifth_word <- unlist(Filter(function(x) !identical(character(0),x), applied))
        df <- data.frame(first_word,second_word,third_word,fourth_word,fifth_word)
    }
    if(n>5){ 
        applied <- lapply(string_vector,
                          function(x) {strsplit(x, "\\s+")[[1]][6]})
        sixth_word <- unlist(Filter(function(x) !identical(character(0),x), applied))
        df <- data.frame(first_word,second_word, third_word, fourth_word, fifth_word, sixth_word)
    }
    
    na.omit(df)
}
# word_frame <- traincolumns
get_2gram_table <- function(word_frame, minimum_word_count=2){
  
    pair12 <- word_frame[,c(1,2)]
    colnames(pair12) <- c("word_1","word_2") 
    pair23 <- word_frame[,c(2,3)]
    colnames(pair23) <- c("word_1","word_2") 
    pair34 <- word_frame[,c(3,4)]
    colnames(pair34) <- c("word_1","word_2") 
    pair45 <- word_frame[,c(4,5)]
    colnames(pair45) <- c("word_1","word_2") 
    pair56 <- word_frame[,c(5,6)]
    colnames(pair56) <- c("word_1","word_2") 
    pairs <- rbind(pair12,pair23,pair34,pair45,pair56)
 
    lines <- get_concatenated_strings(pairs)
    #length(lines)
    
    table2 <- table(lines)
    #sorted <- sort(table2, decreasing=TRUE)
    popular_phrases <-  table2[table2>=minimum_word_count]
    df <- data.frame(popular_phrases,stringsAsFactors = FALSE)
    wordcols <- get_training_columns(df[,1],n=2)
    wordcols$Freq <- df$Freq 
    
    puncts1 <- grepl("[a-zA-Z]",wordcols[,"first_word"])
    wordcols <- wordcols[puncts1,]
    head(wordcols)
    
    puncts1 <- grepl("''",wordcols[,"first_word"])
    wordcols <- wordcols[!puncts1,]
    head(wordcols)
    
#     puncts1 <- grepl("^'",wordcols[,"first_word"])
#     wordcols <- wordcols[!puncts1,]
#     tail(wordcols,10)
#     
#     puncts2 <- grepl("'$",wordcols[,"first_word"])
#     wordcols <- wordcols[!puncts2,]
    wordcols <- wordcols[order(-wordcols[,3]),]
    wordcols
}

# head(traincolumns)
# tail(traincolumns)
# dim(traincolumns)
# word_frame <- traincolumns
get_2gram_sep <- function(word_frame, sep=1, minimum_word_count=1){
  
    if(sep==1){
        pair13 <- word_frame[,c(1,3)]
        colnames(pair13) <- c("word_1","word_2") 
        pair24 <- word_frame[,c(2,4)]
        colnames(pair24) <- c("word_1","word_2") 
        pair35 <- word_frame[,c(3,5)]
        colnames(pair35) <- c("word_1","word_2") 
        pair46 <- word_frame[,c(4,6)]
        colnames(pair46) <- c("word_1","word_2") 
        pairs <- rbind(pair13,pair24,pair35,pair46)
    } 
    if(sep==2){
        pair14 <- word_frame[,c(1,4)]
        colnames(pair14) <- c("word_1","word_2") 
        pair25 <- word_frame[,c(2,5)]
        colnames(pair25) <- c("word_1","word_2") 
        pair36 <- word_frame[,c(3,6)]
        colnames(pair36) <- c("word_1","word_2") 
        pairs <- rbind(pair14,pair25,pair36)
    }
    
    if(sep==3){
        pair15 <- word_frame[,c(1,5)]
        colnames(pair15) <- c("word_1","word_2") 
        pair26 <- word_frame[,c(2,6)]
        colnames(pair26) <- c("word_1","word_2") 
        pairs <- rbind(pair15,pair26)
    }
    
    if(sep==4){
        pair16 <- word_frame[,c(1,6)]
        colnames(pair16) <- c("word_1","word_2") 
        pairs <- pair16
    }
    
    lines <- get_concatenated_strings(pairs)
    
    table2 <- table(lines)
    popular_phrases <- table2[table2>=minimum_word_count]
    df <- data.frame(popular_phrases,stringsAsFactors = FALSE)
    wordcols <- get_training_columns(df[,1],n=2)
    wordcols$Freq <- as.numeric(as.character(df$Freq))
    #puncts1[1:100]
    puncts1 <- grepl("[a-zA-Z]",wordcols[,"first_word"])
    sum(puncts1)
    wordcols <- wordcols[puncts1,]
    head(wordcols)
    dim(wordcols)[1]
    
    puncts1 <- grepl("''",wordcols[,"first_word"])
    wordcols <- wordcols[!puncts1,]
    head(wordcols)
    
    puncts1 <- grepl("^'",wordcols[,"first_word"])
    wordcols <- wordcols[!puncts1,]
    tail(wordcols,10)
    
    puncts1 <- grepl("'$",wordcols[,"first_word"])
    wordcols <- wordcols[!puncts1,]
    wordcols <- wordcols[order(-wordcols[,3]),]
    wordcols
}

get_3gram_table <- function(word_frame, minimum_word_count=1){
   
    triple123 <- word_frame[,c(1,2,3)]
    colnames(triple123) <- c("word_1","word_2","word_3") 
    triple234 <- word_frame[,c(2,3,4)]
    colnames(triple234) <- c("word_1","word_2","word_3") 
    triple345 <- word_frame[,c(3,4,5)]
    colnames(triple345) <- c("word_1","word_2","word_3") 
    triple456 <- word_frame[,c(4,5,6)]
    colnames(triple456) <- c("word_1","word_2","word_3") 
    
    triple <- rbind(triple123,triple234,triple345,triple456)
 
    lines <- get_concatenated_triple_strings(triple)
    
    table3 <- table(lines)
    popular_phrases <- table3[table3>=minimum_word_count]
    df <- data.frame(popular_phrases ,stringsAsFactors = FALSE)
    wordcols <- get_training_columns(df[,1],n=3)
    wordcols$first_word <- as.character(wordcols$first_word)
    wordcols$second_word <- as.character(wordcols$second_word)
    wordcols$third_word <- as.character(wordcols$third_word)
    wordcols$Freq <- as.numeric(as.character(df$Freq))
    #head(wordcols)
    wordcols[order(-wordcols[,4]),]
}

get_4_gram_table <- function(lines, minimum_word_count=1){
    pattern <- "[a-zA-Z']{1,18}[ +]{1,2}[a-zA-Z']{1,18}[ +]{1,2}[a-zA-Z']{1,18}[ +]{1,2}[a-zA-Z']{1,18}"
    any4 <- findmatchgreg(lines,pattern)
    table4 <- table(any4)
    #sorted <- sort(table, decreasing=TRUE)
    popular_phrases <- table4[table4>=minimum_word_count]
    data.frame(popular_phrases)
}

get_separated_pairs <- function(lines,sep=-1){
    if(sep==-1 | sep==2){
        skip2 <- get_2gram_skip_n(lines,2)
        skip2
    }
    
    if(sep==-1 | sep==3){    
        skip3 <- get_2gram_skip_n(lines,3)
        skip3
    }
    
    if(sep==-1 | sep==4){
        skip4 <- get_2gram_skip_n(lines,4)
        skip4
    }
    
    if(sep==-1){
        ##  Return vector of 2-grams
        rbind(skip1,skip2,skip3,skip4)
    }
}

removeMostPunctuation <- function (x, keep="'") 
{
    x <- gsub(keep, "\002", x)
    x <- gsub("[[:punct:]]+", "", x)
    gsub("\002", keep, x, fixed = TRUE)
}

get_concatenated_strings<- function(pair_table=data.frame(c("word11","word21"),
                                                          c("word12","word22")),
                                    str_vector_1=c("a"),str_vector_2=c("b")){
    if(pair_table[1,1]!="word11"){
        len1 <- length(str_vector_1)
        len2 <- length(str_vector_2)
    } else {
        len1 <- length(pair_table[,1])
        len2 <- length(pair_table[,2])
    }
    
    if(pair_table[1,1]=="word11"){
        if(len1 != len2){
            print("Vectors have unequal lengths at get_concatenated_strings")
            return("Vectors have unequal lengths at get_concatenated_strings")
        } else {
            paste(str_vector_1,str_vector_2)}
    } else {
        paste(pair_table[,1],pair_table[,2])
    }
}

get_concatenated_triple_strings<- function(pair_table=data.frame(c("word11","word21","word31"),
                                                                 c("word12","word22","word32")),
                                           str_vector_1=c("a"),str_vector_2=c("b"),str_vector_3=c("c")){
    if(pair_table[1,1]!="word11"){
        len1 <- length(str_vector_1)
        len2 <- length(str_vector_2)
        len3 <- length(str_vector_3)
    } else {
        len1 <- length(pair_table[,1])
        len2 <- length(pair_table[,2])
        len3 <- length(pair_table[,3])
    }
    
    if(pair_table[1,1]=="word11"){
        if(len1 != len2){
            print("Vectors have unequal lengths at get_concatenated_strings")
            return("Vectors have unequal lengths at get_concatenated_strings")
        } else {
            paste(str_vector_1,str_vector_2, str_vector_2)}
    } else {
        paste(pair_table[,1],pair_table[,2], pair_table[,3])
    }
}

get_2gram_choices <- function(word, minimum=4){
    #minimum <- 2
    #phrase <- "Go on a romantic date at the"  
    #backref<-1
    
    ##  Get 3-grams
    word <- gsub("^ +\\b", "", word)
    word <- gsub("\\b +$", "", word)
    ispunct <- grepl("''",gram2[,"first_word"])
    gram2 <- gram2[ispunct!=TRUE,]
    choices <- gram2[gram2[,"first_word"] == word,]
    choices <- choices[choices[,3]>=minimum,]
    numberofchoices <- dim(choices)[1]
    
    if(numberofchoices < minimum){
        choices <- head(gram2[gram2[,"first_word"]=="the",])[1:3,]
    }
    choices
}

get_3gram_choices <- function(phrase, minimum=2){
    #minimum <- 2
    #phrase <- "Go on a romantic date at the"  
    #backref<-1
    back <- reverse_word_order(phrase)
    
##  Get 3-grams
    choices <- gram3[gram3[,"first_word"] == back[2] & gram3[,"second_word"] == back[1] ,]
    choices <- choices[choices[,4]>=minimum,]
    numberofchoices <- dim(choices)[1]
    
    if(numberofchoices < minimum){
        choices <- gram2[gram2[,"first_word"]==back[1] ,]
    }
    choices
}

get_uncommon_words <- function(phrase, gram_3_choices){
    #phrase <- "Go on a romantic date at the" 
    #gram_3_choices <- choices3
    back <- reverse_word_order(phrase)
    backlen <- length(back)
    back3 <- setdiff(back[3:backlen], top400)
    back <- c(back[1:2],back3)
    backlen <- length(back)
    if(backlen<3){
    } else if(backlen>=3){
        #skippy1 <- as.character(gram2_sep1[gram2_sep1$first_word==back[3],"second_word"])
        skippy1r <- as.character(gram2_sep1[gram2_sep1$second_word==back[3],"first_word"])
        #skippy2 <- as.character(gram2_sep2[gram2_sep2$first_word==back[3],"second_word"])
        skippy2r <- as.character(gram2_sep2[gram2_sep2$second_word==back[3],"first_word"])
        #skippy3 <- as.character(gram2_sep3[gram2_sep3$first_word==back[3],"second_word"])
        skippy3r <- as.character(gram2_sep3[gram2_sep3$second_word==back[3],"first_word"])
    
    #skippy <- c(skippy1,skippy1r,skippy2,skippy2r,skippy3,skippy3r)
    skippy <- c(skippy1r,skippy2r,skippy3r)
    }
    
    if(backlen>=4){
        skippy1 <- as.character(gram2_sep1[gram2_sep1$first_word==back[4],"second_word"])
        skippy1r <- as.character(gram2_sep1[gram2_sep1$second_word==back[4],"first_word"])
        skippy2 <- as.character(gram2_sep2[gram2_sep2$first_word==back[4],"second_word"])
        skippy2r <- as.character(gram2_sep2[gram2_sep2$second_word==back[4],"first_word"])
        skippy3 <- as.character(gram2_sep3[gram2_sep3$first_word==back[4],"second_word"])
        skippy3r <- as.character(gram2_sep3[gram2_sep3$second_word==back[4],"first_word"])
        
        skippy <- c(skippy,skippy1,skippy1r,skippy2,skippy2r,skippy3,skippy3r)
    }
    # if(backlen>=5 & length(skippy) < 50){
    #     skippy1 <- as.character(gram2_sep1[gram2_sep1$first_word==back[5],"second_word"])
    #     skippy1r <- as.character(gram2_sep1[gram2_sep1$second_word==back[5],"first_word"])
    #     skippy2 <- as.character(gram2_sep2[gram2_sep2$first_word==back[5],"second_word"])
    #     skippy2r <- as.character(gram2_sep2[gram2_sep2$second_word==back[5],"first_word"])
    #     skippy3 <- as.character(gram2_sep3[gram2_sep3$first_word==back[5],"second_word"])
    #     skippy3r <- as.character(gram2_sep3[gram2_sep3$second_word==back[5],"first_word"])
    #     
    #     skippy <- c(skippy,skippy1,skippy1r,skippy2,skippy2r,skippy3,skippy3r)
    # }
    
    
    t <- table(skippy)
    
    dd <- data.frame(t,stringsAsFactors = FALSE)
    str(dd)
    
    choicess1 <- dd[ order(-dd$Freq), ][,1]
    punct <- grepl("[[:punct:]]",choicess1)
    choicess1  <- choicess1 [punct==FALSE]
    
##  Insure that matching 3-grams are not deleted with other popular words     
    popular <- setdiff(short400, gram_3_choices)
    
    choicesuncommon <- setdiff(choicess1, popular)
    yy <- as.character(gram1[,1])
    possible <- intersect(yy, choicesuncommon)
    spelling <- setdiff(possible, mispelled_punctution)
    
    tempvec <- gram1$anyword
    foundindictionary <- intersect(spelling,tempvec)
    word_vector <- setdiff(foundindictionary, words_with_punctution)
    word_vector
}

bestpick <- function(phrase){       
    #phrase <- "Very early observations on the Bills game: Offense still struggling but the "  
    #phrase <- "Go on a romantic date at the"
    #phrase <- "" # false
    #phrase <- "  " # false
    no_words <- !grepl("[a-zA-Z]",phrase)
    atleasttwo <- grepl("[a-zA-Z,] [a-zA-Z,]",phrase)
    if(no_words==TRUE){
        outvec <- c("The","A","To")
        choice_vec <- gram1[1:3,1]
        bestchoice1 <<- gram1[1,1]
        bestchoice2 <<- gram1[2,1]
        bestchoice3 <<- gram1[3,1]
    } else {                                        # one or more
        phrase <- removeMostPunctuation(phrase)
        word_vec <- strsplit(phrase, "\\s+")[[1]]
        lenwordvec <- length(word_vec)
        
        if(lenwordvec==1) {
        ## There is one word.
            oneword <- gsub(" +", "", phrase)           # one word
            choice_vec <- get_2gram_choices(oneword)
            bestchoice1 <<- choice_vec[1,2]
            bestchoice2 <<- choice_vec[2,2]
            bestchoice3 <<- choice_vec[3,2]
            
        } else if(lenwordvec==2){                       # two words
            word_vec <- strsplit(phrase, "\\s+")[[1]]
            #lenwordvec <- length(word_vec)
            choice_vec <- get_3gram_choices(word_vec)
            bestchoice1 <<- choice_vec[1,2]
            bestchoice2 <<- choice_vec[2,2]
            bestchoice3 <<- choice_vec[3,2]
   
        } else {                                   # three or more
#             clauses <- get_sentences(phrase)
#             clause_quantity <- length(clauses)
#             try_to_spell <- FALSE
    
        ##  last character
            last_char_is_space <- grepl("[ .]$", phrase)
   
#             reverseorder <- clauses[clause_quantity]
#             for(i in seq_along(clauses)){
#                 if(clause_quantity>i){
#                 reverseorder[i+1] <- clauses[clause_quantity-i]
#                 }
#             }
    
##############################################################################        
            #last_sentence <- reverseorder[1] 
            backlen <- lenwordvec #length(last_sentence)
        
#             sentence <- strsplit(last_sentence, "\\s+")[[1]]
#             sentence <- tolower(sentence)
#             sentencelength <- length(sentence)
        ##  initialize back
#             back1 <- sentence[sentencelength]
#     
#             for(i in seq_along(sentence)){
#                 if(sentencelength>i){
#                     back1[i+1] <- sentence[sentencelength-i]
#                 }
#             }
#         
#             back2 <- ""
#         
#             if(clause_quantity==2){
#                 sentence <- reverseorder[2]    
#                 sentence2 <- strsplit(sentence, "\\s+")[[1]]
#                 sentence2 <- tolower(sentence2)
#                 sentencelength <- length(sentence2)
#             ##  initialize back
#                 back2 <- sentence2[sentencelength]
            
#             for(i in seq_along(sentence2)){
#                 if(sentencelength>i){
#                     back2[i+1] <- sentence2[sentencelength-i]
#                 }
#             }
            back <- word_vec #c(back1, back2)        
        #}
        backlen <- length(back)


##########################################################################    

##  phrase ends with space
 
#         if(last_char_is_space==FALSE
#            &
#            nchar(back[1]) < 3
#            &
#            back[1] != "a")
#             {
#                 back <- back[2:sentencelength]
#                 try_to_spell <- TRUE
#             }
    
##############################################################################
    
##  Three grams
    
        choices3 <- get_3gram_choices(sentence)
        number_of_3_grams <- dim(choices3)[1]
        minqty <- 2
        choices3 <- choices3[choices3$Freq >= minqty,]
        number_of_3_grams <- dim(choices3)[1]

        if(number_of_3_grams > 7){
            choices3 <- choices3[1:7,]
        }
    ##############################################################################
    ##############################################################################
    
##  less common words; combine sentences
        if(clause_quantity>1){
            phrase <- paste(reverseorder[2],reverseorder[1])
        }
        
        phrase <- tolower(removeMostPunctuation(phrase))
        
    #debug(get_uncommon_words)
        uncommonchoices <- get_uncommon_words(phrase, choices3)
        uncommonchoices <- setdiff(uncommonchoices, back)
        uncommonchoices <- setdiff(uncommonchoices, short400)
        print(uncommonchoices)
        
        choices_in_dictionary <- intersect(choices3[,3],gram1$anyword)
        
        choices <- unique(c(uncommonchoices,choices_in_dictionary))
        choices1 <- sort(choices)
    
        number_of_lookback_words <- length(back)
        lookback_count <- number_of_lookback_words - 1
        loop_count <- 1:lookback_count 
    
        datadf <- 0
        datadf <- list() 

        columnnames <- 0
        backlen <- length(back)
    
    ####################################################################
        
#         gram2_sep1[gram2_sep1$first_word==choices1[56] ,"Freq"]
#         
#         
#         gram2_sep1[gram2_sep1$first_word==choices1[56] ,]
#         gram2_sep1[gram2_sep1$second_word==choices1[56] ,]
#         gram2_sep2[gram2_sep2$first_word==choices1[56] ,]
#         gram2_sep2[gram2_sep2$second_word==choices1[56] ,]
                       #|
                           #(gram2_sep2$second_word==backshort[i] &
                            #    gram2_sep2$first_word==choices1[56])
                       
                      
    
        backshort <- setdiff(back, short400)
        i <- 3
        j <- 2
        for(i in seq_along(backshort)){
            back_wordfreq <- 0
       
            datavector <- 0
            head(gram2_sep1)
      
            for(j in seq_along(choices1)){
                datavector[j] <- sum(gram2_sep1[#(gram2_sep1$first_word==backshort[i] &
                                                # gram2_sep1$second_word==choices1[j]) 
                                                # |
                                               (gram2_sep1$second_word==backshort[i] &
                                                gram2_sep1$first_word==choices1[j])
                                                |
                                               (gram2_sep2$first_word==backshort[i] &
                                                gram2_sep2$second_word==choices1[j]) 
                                                |
                                               (gram2_sep2$second_word==backshort[i] &
                                                gram2_sep2$first_word==choices1[j])
                                               #  |
                                               # (gram2_sep3$first_word==backshort[i] &
                                               #  gram2_sep3$second_word==choices1[j]) 
                                               #  |
                                               # (gram2_sep3$second_word==backshort[i] &
                                               #  gram2_sep3$first_word==choices1[j])
                                                ,"Freq"])
                datavector
            }

            specialsymbol <- "$"
            columnnames[i] <- paste("back",i,sep="")
            datavector

            datadf[[i]] <- datavector
            datadf
        }
        
        ##############################################################
        print(backshort)
        print(choices1)
        results <- as.matrix(sapply(datadf, as.numeric))
        backshortlength <- dim(results)[2]
        rowtotals <- apply(results, 1, sum)
        coltotals <- apply(results, 2, sum)
        
        if(backshortlength==1){
            resultsdf <- data.frame(choices1,rowtotals,results[,1])
            colnames(resultsdf) <-  c("choices","rowtotals",columnnames[1]) 
        }
        if(backshortlength==2){
        resultsdf <- data.frame(choices1,rowtotals,results[,1],
                            results[,2])
        colnames(resultsdf) <-  c("choices","rowtotals",columnnames[1],
                                  columnnames[2]) 
        } else if (backshortlength>2){
            resultsdf <- data.frame(choices1,rowtotals,results[,1],
                                    results[,2],results[,3])
            colnames(resultsdf) <-  c("choices","rowtotals",columnnames[1],
                                      columnnames[2],columnnames[3]) 
        }
         
####################################################################### 
    
        head(resultsdf)
        lenresults <- dim(resultsdf)[1]
        out_vec <- as.character(resultsdf[,1][1:lenresults])
       
        probs <- unlist(lapply(out_vec, function(x) {gram1[gram1$anyword==x,"Freq"][[1]][1]}))
        wordsvec <- resultsdf[1:lenresults,1]
        oddsvec <- resultsdf[1:lenresults,2]/probs[1:lenresults]
        output <- data.frame(wordsvec,oddsvec)
        punct <- grepl("[[:punct:]]",wordsvec)
        output <- output[punct==FALSE,]
        out <- output[order(-output$oddsvec),]
    if(try_to_spell == TRUE) {
        lastletters <- findmatchexec(back[1],"\\w")
        pattern <- paste("^",lastletters,sep=" ")
        spellingmatches <- grep(pattern, resultsdf[,1])
        out <- spellingmatches[1]
    }
        outvec <- as.character(out[1:3,1])
        bestchoice1 <<- outvec[1]
        print(outvec[1])
        bestchoice2 <<- outvec[2]
        bestchoice3 <<- outvec[3]
        }  # three or more
    } # no words
        
    outvec
} 

get_choices <- function(phrase){
#     bestchoice1 <- "the"
#     bestchoice2 <- "a"
#     bestchoice3 <- "I"
debug(get_choices)
    choicedf <- bestpick(phrase)
    print(bestchoice1)
    print(bestchoice2)
    print(bestchoice3)
    head(choicedf)
}
    
###############################################################################
###############################################################################

#############################################################################
#############################################################################
#############################################################################
    
library(ggplot2)
library(lubridate)
library(xtable)
library(gridExtra)
library(tm)
library(XML)
options(warn = -1)

#setwd("~/edu/Data Science/capstone/Quiz/Quiz2")

trainingcol <- read.csv(file = "trainingcol.csv",stringsAsFactors = FALSE)
training <- trainingcol[,1]
head(training)

# training2col <- read.csv(file = "training2col.csv",stringsAsFactors = FALSE)
# training2 <- training2col[,1]
# head(training2)

traincolumns <- read.csv(file = "traincolumns.csv",stringsAsFactors = FALSE)
head(traincolumns)
# 
# gram1 <- read.csv(file = "dictionary.csv",stringsAsFactors = FALSE)
# head(gram1)
# 
gram2 <- read.csv(file = "gram2.csv",stringsAsFactors = FALSE)
head(gram2)
# 
# gram3 <- read.csv(file = "gram3.csv",stringsAsFactors = FALSE)
# head(gram3)
# 
# gram2_sep1 <- read.csv("gram2_sep1.csv",stringsAsFactors = FALSE)
# head(gram2_sep1)
# 
# gram2_sep2 <- read.csv("gram2_sep2.csv",stringsAsFactors = FALSE)
# head(gram2_sep2)
# 
# gram2_sep3 <- read.csv("gram2_sep3.csv",stringsAsFactors = FALSE)
# head(gram2_sep3)


##########################################################################
##########################################################################

   

    