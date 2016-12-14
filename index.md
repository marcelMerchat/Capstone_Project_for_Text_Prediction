---
title       : Text Prediction Capstone Project
subtitle    : Coursera-John Hopkins School of Public Health
author      : Marcel Merchat
job         : Data Scientist
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # {zenburn}
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Introduction
This project developed a text prediction algorithm that predicts the next desired word. As the emphasis is on speed, all predictions are chosen from data frames that consist of a dictionary called "Gram-1," two word phrases in "Gram-2," and three word phrases in "Gram-3." For short sentence fragments of only a few words, the prediction is based on the phrase with the highest frequency of occurrence. If the fragment is long enough and contains at least two non-trivial words, the five most likely words are evaluated for their frequency of appearance with the non-trivial words.

---

## Uncommon Words
In the development of the algorithm, more unusual words in the fragment were used to find other associated words and a formula was adjusted to find about one hundred such words without regard to frequency of appearance as the initial search was simply to find candidates. The candidate words were ranked for frequency of occurrence with the non-trivial words in the fragment. For words like "offense", a related word like "defense" could usually be found amoung the candidate words; but this was dropped in order to obtain faster results and only candidates from 1-gram, 2-gram and 3-gram data files remained in the final program.  




---

## Processed Raw Data
========================================================

The raw data files were first divided into 1024 segments, and a relatively small training set of these were randomly selected to develop the model. Before using the tm package to clean the data, all lines of text were divided into sentences so that only unit sentences appeared on each line which permitted mining related words only found together in the same sentence.  



---

## Only one clause or sentence per line:
========================================================
Long twitter and other lines were broken up so that the prediction model was only based on phrases and relationships within a sentence. The function get_sentences broke up long lines of raw text into sentences. For example, the final sentence was isolated as Line-17 and is shown as the last line below. 


```r
      traininga <- get_sentences(training)
```

```
## Error in gregexpr(pattern, text): object 'training' not found
```

```r
      training[5]
```

```
## Error in eval(expr, envir, enclos): object 'training' not found
```

```r
##    Only Sentences      
      get_sentences(traininga)[17]
```

```
## Error in gregexpr(pattern, text): object 'traininga' not found
```



```
## Error in eval(expr, envir, enclos): object 'traininga' not found
```

```
## Error in tolower(phrase): object 'phrase' not found
```

```
## Error in tolower(phrase): object 'shortened_phrase' not found
```

```
## Error in xtable(choicedf, digits = 4): object 'choicedf' not found
```

```
## Error in eval(expr, envir, enclos): object 'traininga' not found
```

```
## Error in tolower(phrase): object 'phrase2' not found
```

```
## Error in tolower(phrase): object 'shortened_phrase2' not found
```

```
## Error in xtable(choicedf2, digits = 4): object 'choicedf2' not found
```

---

## Prediction Results


```r
     shortened_phrase
```

```
## Error in eval(expr, envir, enclos): object 'shortened_phrase' not found
```

```r
     print(x1, type="html") ## Ranked Choices
```

```
## Error in print(x1, type = "html"): object 'x1' not found
```

---

## More Results


```r
   shortened_phrase2
```

```
## Error in eval(expr, envir, enclos): object 'shortened_phrase2' not found
```

```r
   print(x2, type="html") ## Ranked Choices
```

```
## Error in print(x2, type = "html"): object 'x2' not found
```

