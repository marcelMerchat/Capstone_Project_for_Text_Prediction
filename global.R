library(shiny)
library(gridExtra)
library(DT)

options(warn=-1)

gram1 <- read.csv("dictionary.csv",stringsAsFactors = FALSE)
gram2 <- read.csv("gram2.csv",     stringsAsFactors = FALSE)
gram3 <- read.csv("gram3.csv",     stringsAsFactors = FALSE)

gram1 <<- gram1

punct <- grepl("[[:punct:]]",gram1[,1])
words_with_punctution <- gram1[punct==TRUE,1]
mispelled_punctution <- gsub("[[:punct:]]+", "", words_with_punctution)
# 
# #########################################################################
# 
top400 <- as.character(head(gram1,400)[,1])
short400 <- top400[nchar(top400)<5]

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
    popular_dictionary <- table1[table1>=minimum_word_count]
    df <- data.frame(popular_dictionary)
    anyword <- as.character(df[,1])
    df[,"anyword"] <- anyword
    df[order(-df$Freq),]
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

get_2gram_table <- function(word_frame, minimum_word_count=1){
    
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
    wordcols[order(-wordcols[,3]),]
    wordcols
}

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
         choices <- gram3[gram3[,"second_word"] == back[1],]
         #choices$second_word <- gram2[gram2[,"first_word"]==back[1] ,2]
     }
    choices
}

get_uncommon_words <- function(phrase, gram_3_choices){
    phrase <- "Go on a romantic date at the"
    #gram_3_choices <- choices3
    #phrase <- "aaa bbb tokwqrdww forward wedfg eer"
    
    back <- reverse_word_order(phrase)
    backlen <- length(back)
    back3 <- setdiff(back[3:backlen], top400)
    back <- c(back[1:2],back3)
    backlen <- length(back)
    if(backlen==0){back<-phrase}
    if(backlen==1){back<-phrase}
    if(backlen==2){back<-phrase}
    if(backlen>=2){
        #skippy1 <- as.character(gram2_sep1[gram2_sep1$first_word==back[3],"second_word"])
        skippy1r <- as.character(gram2_sep1[gram2_sep1$second_word==back[3],"first_word"])
        #skippy2 <- as.character(gram2_sep2[gram2_sep2$first_word==back[3],"second_word"])
        #skippy2r <- as.character(gram2_sep2[gram2_sep2$second_word==back[3],"first_word"])
        skippya <- c(skippy1r)
        len <- length(skippya)
        set.seed <- 334
        if(len>70){
            skippya <- sample(skippya, 70)
        }
    }
    
    if(backlen>=3){
        skippy1 <- as.character(gram2_sep1[gram2_sep1$first_word==back[4],"second_word"])
        #skippy1r <- as.character(gram2_sep1[gram2_sep1$second_word==back[4],"first_word"])
        skippy2 <- as.character(gram2_sep2[gram2_sep2$first_word==back[4],"second_word"])
        #skippy2r <- as.character(gram2_sep2[gram2_sep2$second_word==back[4],"first_word"])
        
        skippyb <- c(skippy1,skippy2)
        len <- length(skippyb)
        if(len>40){
            set.seed <- 333
            skippyb <- sample(skippyb, 30)
        }
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
    
    print("at uncommon words")
    #skippy <- intersect(skippya, skippyb)
    skippy <- c(skippya, skippyb)

    if(identical(skippy, character(0))){
        skippy <- c("the","and","to")
    }
    
    t <- table(skippy)
    dd <- data.frame(t,stringsAsFactors = FALSE)
    choicess1 <- dd[ order(-dd$Freq), ][,1]
   
    popular <- short400
    ##  Insure that matching 3-grams are not deleted with other popular words
    popular <- setdiff(short400, gram_3_choices)
    
    choicesuncommon <- setdiff(choicess1, popular)
    yy <- as.character(gram1[,1])
    possible <- intersect(yy, choicesuncommon)
    spelling <- setdiff(possible, mispelled_punctution)
    tempvec <- gram1$anyword
    foundindictionary <- intersect(spelling,tempvec)
    word_vector <- setdiff(foundindictionary, words_with_punctution)
    print(word_vector)
    word_vector
}

bestpick <- function(phrase){       
    #phrase <- "Very early observations on the Bills game: Offense still struggling but the "  
    #phrase <- "Go on a romantic date at the xyz"

    phrase <- tolower(phrase)
    no_words <- !grepl("[a-zA-Z]",phrase)
    atleasttwo <- grepl("[a-zA-Z,] [a-zA-Z,]",phrase)
    
    
    if(no_words==TRUE){ 
        choicedf1 <- gram1
        choicedf <- choicedf1
        bestchoice1 <<- choicedf1[1,1]
        bestchoice2 <<- choicedf1[2,1]
        bestchoice3 <<- choicedf1[3,1]
        choice_table  <<- choicedf
    } else {                                        # one or more
        phrase <- removeMostPunctuation(phrase)
        word_vec <- strsplit(phrase, "\\s+")[[1]]
        lenwordvec <- length(word_vec)
        #print(paste("lenwordvec",lenwordvec))
        
        back <- reverse_word_order(phrase)
        # print(back)
        diff <- setdiff(back[3:lenwordvec], short400)
        lastwordcheck <- intersect(back[1], short400)
        stripped <- c(back[1:2], diff)
        striplen <- length(stripped)
        
        if(lenwordvec==1) {
            oneword <- gsub(" +", "", phrase)           # one word
            choicedf2 <- get_2gram_choices(oneword)
            choicedf2 <- choicedf2[order(-choicedf2$Freq),]
            choicedf <- choicedf2
            bestchoice1 <<- choicedf2[1,2]
            bestchoice2 <<- choicedf2[2,2]
            bestchoice3 <<- choicedf2[3,2]
            choice_table  <<- choicedf
            
        } else if(lenwordvec==2 | striplen < 4 | identical(lastwordcheck, character(0))){                       # two words

            choicedf3 <- get_3gram_choices(phrase)
            choicedf3 <- choicedf3[order(-choicedf3$Freq),]
            choicedf <- choicedf3
            # print("gram3")
            # print(head(choicedf3))
            bestchoice1 <<- choicedf3[1,3]
            bestchoice2 <<- choicedf3[2,3]
            bestchoice3 <<- choicedf3[3,3]
            choice_table  <<- choicedf
            #print(bestchoice1)

        } else {                                   # three or more
 
        ##  last character
            last_char_is_space <- grepl("[ .]$", phrase)

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
            
            choicesdf3 <- get_3gram_choices(phrase)
            number_of_3_grams <- dim(choicesdf3)[1]
            if(number_of_3_grams == 0){
                choicedf <- gram3[1:10,]
            } else {
            choices3 <- choicesdf3[order(-choicesdf3$Freq),]
            number_of_3_grams <- dim(choices3)[1]
            
            minqty <- 2
            choices3 <- choices3[choices3$Freq >= minqty,]

            #print(paste("three gram count is",number_of_3_grams))
            

            if(number_of_3_grams > 0 & number_of_3_grams < 5){
                choices3 <- choices3[1:3,]
            } else if(number_of_3_grams > 4){
                choices3 <- choices3[1:5,]
            }
            choicedf <- choices3
            ##############################################################################
            ##############################################################################
            
            choices_in_dictionary <- intersect(choices3[,3],gram1$anyword)
            # if(backlen > 2){
            #     
            #     #debug(get_uncommon_words)
            #     uncommonchoices <- get_uncommon_words(phrase, choices3)
            #     uncommonchoices <- setdiff(uncommonchoices, back)
            #     uncommonchoices <- setdiff(uncommonchoices, short400)
            #     choices <- unique(c(uncommonchoices,choices_in_dictionary))
            # } else {
            #     choices <- choices_in_dictionary
            # }
            
            choices1 <- choices_in_dictionary
            #choices1 <- sort(choices)
            
            }  # three or more
            
            ###################################################################
            
            number_of_lookback_words <- length(back)
            lookback_count <- number_of_lookback_words - 1
            loop_count <- 1:lookback_count 
            
            datadf <- 0
            datadf <- list() 
            columnnames <- 0
            
            ####################################################################
            
            backshort <- setdiff(back, short400)
            backlen <- length(backshort)
            if(backlen > 0){
                
                for(i in seq_along(backshort)){
                    back_wordfreq <- 0
                    
                    datavector <- 0
                    #head(gram2)
                    
                    for(j in seq_along(choices1)){
                        datavector[j] <- sum(gram2[#(gram2$first_word==backshort[i] &
   
                            (gram2$second_word==backshort[i] &
                                 gram2$first_word==choices1[j])
                            |
                                (gram2$first_word==backshort[i] &
                                     gram2$second_word==choices1[j]) 
                            |
                                (gram2$second_word==backshort[i] &
                                     gram2$first_word==choices1[j])
                            ,"Freq"])
                        #datavector
                    }
                    
                    specialsymbol <- "$"
                    columnnames[i] <- paste("back",i,sep="")
                    #datavector
                    
                    datadf[[i]] <- datavector
                    #datadf
                }
                
                ##############################################################
                # print(backshort)
                # print(choices1)
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
                
                #head(resultsdf)
                lenresults <- dim(resultsdf)[1]
                out_vec <- as.character(resultsdf[,1][1:lenresults])
                
                probs <- unlist(lapply(out_vec, function(x) {gram1[gram1$anyword==x,"Freq"][[1]][1]}))
                wordsvec <- resultsdf[1:lenresults,1]
                oddsvec <- resultsdf[1:lenresults,2]/probs[1:lenresults]
                output <- data.frame(wordsvec,oddsvec)
                punct <- grepl("[[:punct:]]",wordsvec)
                output <- output[punct==FALSE,]
                choicedf <- output[order(-output$oddsvec),]
                #     if(try_to_spell == TRUE) {
                #         lastletters <- findmatchexec(back[1],"\\w")
                #         pattern <- paste("^",lastletters,sep=" ")
                #         spellingmatches <- grep(pattern, resultsdf[,1])
                #         out <- spellingmatches[1]
                #     }
                #print(choicedf)
                bestchoice1 <<- as.character(choicedf[1,1])
                bestchoice2 <<- as.character(choicedf[2,1])
                bestchoice3 <<- as.character(choicedf[3,1])
                choice_table <<- choicedf

            } #word selection algorithym
        } #word selection algorithym with no matches
       
    } # no words
    print(str(choicedf))
    choicedf
} 

get_choices <- function(phrase){
    #     bestchoice1 <- "the"
    #     bestchoice2 <- "a"
    #     bestchoice3 <- "I"
    debug(bestpick)
    choicedf <- bestpick(phrase)
    print(bestchoice1)
    print(bestchoice2)
    print(bestchoice3)
    print(choicedf)
}


