## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, 
                      comment = "##")

## ----show=FALSE----------------------------------------------------------
require(quanteda)

## ------------------------------------------------------------------------
myCorpus <- corpus(data_char_ukimmig2010)  # build a new corpus from the texts
summary(myCorpus)

## ------------------------------------------------------------------------
docvars(myCorpus, "Party") <- names(data_char_ukimmig2010)
docvars(myCorpus, "Year") <- 2010
summary(myCorpus)

## ------------------------------------------------------------------------
metadoc(myCorpus, "language") <- "english"
metadoc(myCorpus, "docsource")  <- paste("data_char_ukimmig2010", 1:ndoc(myCorpus), sep = "_")
summary(myCorpus, showmeta = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  require(readtext)
#  
#  # Twitter json
#  mytf1 <- readtext("~/Dropbox/QUANTESS/social media/zombies/tweets.json")
#  myCorpusTwitter <- corpus(mytf1)
#  summary(myCorpusTwitter, 5)
#  # generic json - needs a textfield specifier
#  mytf2 <- readtext("~/Dropbox/QUANTESS/Manuscripts/collocations/Corpora/sotu/sotu.json",
#                    textfield = "text")
#  summary(corpus(mytf2), 5)
#  # text file
#  mytf3 <- readtext("~/Dropbox/QUANTESS/corpora/project_gutenberg/pg2701.txt", cache = FALSE)
#  summary(corpus(mytf3), 5)
#  # multiple text files
#  mytf4 <- readtext("~/Dropbox/QUANTESS/corpora/inaugural/*.txt", cache = FALSE)
#  summary(corpus(mytf4), 5)
#  # multiple text files with docvars from filenames
#  mytf5 <- readtext("~/Dropbox/QUANTESS/corpora/inaugural/*.txt",
#                    docvarsfrom = "filenames", sep = "-", docvarnames = c("Year", "President"))
#  summary(corpus(mytf5), 5)
#  # XML data
#  mytf6 <- readtext("~/Dropbox/QUANTESS/quanteda_working_files/xmlData/plant_catalog.xml",
#                    textfield = "COMMON")
#  summary(corpus(mytf6), 5)
#  # csv file
#  write.csv(data.frame(inaugSpeech = texts(data_corpus_inaugural),
#                       docvars(data_corpus_inaugural)),
#            file = "/tmp/inaug_texts.csv", row.names = FALSE)
#  mytf7 <- readtext("/tmp/inaug_texts.csv", textfield = "inaugSpeech")
#  summary(corpus(mytf7), 5)

## ------------------------------------------------------------------------
texts(data_corpus_inaugural)[2]

## ------------------------------------------------------------------------
summary(data_corpus_irishbudget2010)

## ---- fig.width = 8------------------------------------------------------
tokenInfo <- summary(data_corpus_inaugural)
if (require(ggplot2))
    ggplot(data=tokenInfo, aes(x = Year, y = Tokens, group = 1)) + geom_line() + geom_point() +
        scale_x_discrete(labels = c(seq(1789,2012,12)), breaks = seq(1789,2012,12) ) 

# Longest inaugural address: William Henry Harrison
tokenInfo[which.max(tokenInfo$Tokens), ] 

## ------------------------------------------------------------------------
library(quanteda)
mycorpus1 <- corpus(data_corpus_inaugural[1:5], note = "First five inaug speeches.")
mycorpus2 <- corpus(data_corpus_inaugural[53:58], note = "Last five inaug speeches.")
mycorpus3 <- mycorpus1 + mycorpus2
summary(mycorpus3)

## ------------------------------------------------------------------------
summary(corpus_subset(data_corpus_inaugural, Year > 1990))
summary(corpus_subset(data_corpus_inaugural, President == "Adams"))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 200)
kwic(data_corpus_inaugural, "terror")
kwic(data_corpus_inaugural, "terror", valuetype = "regex")
kwic(data_corpus_inaugural, "communist*")

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# inspect the document-level variables
head(docvars(data_corpus_inaugural))

# inspect the corpus-level metadata
metacorpus(data_corpus_inaugural)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
txt <- c(text1 = "This is $10 in 999 different ways,\n up and down; left and right!", 
         text2 = "@kenbenoit working: on #quanteda 2day\t4ever, http://textasdata.com?page=123.")
tokens(txt)
tokens(txt, remove_numbers = TRUE,  remove_punct = TRUE)
tokens(txt, remove_numbers = FALSE, remove_punct = TRUE)
tokens(txt, remove_numbers = TRUE,  remove_punct = FALSE)
tokens(txt, remove_numbers = FALSE, remove_punct = FALSE)
tokens(txt, remove_numbers = FALSE, remove_punct = FALSE, remove_separators = FALSE)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tokens("Great website: http://textasdata.com?page=123.", what = "character")
tokens("Great website: http://textasdata.com?page=123.", what = "character", 
         remove_separators = FALSE)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# sentence level         
tokens(c("Kurt Vongeut said; only assholes use semi-colons.", 
           "Today is Thursday in Canberra:  It is yesterday in London.", 
           "En el caso de que no puedas ir con ellos, ¿quieres ir con nosotros?"), 
          what = "sentence")

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myCorpus <- corpus_subset(data_corpus_inaugural, Year > 1990)

# make a dfm
myDfm <- dfm(myCorpus)
myDfm[, 1:5]

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# make a dfm, removing stopwords and applying stemming
myStemMat <- dfm(myCorpus, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
myStemMat[, 1:5]

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(stopwords("english"), 20)
head(stopwords("russian"), 10)
head(stopwords("arabic"), 10)

## ----warning=FALSE, fig.width = 8, fig.height = 8-----------------------------------------------------------------------------------------------------------------------------------------------------
mydfm <- dfm(data_char_ukimmig2010, remove = stopwords("english"), remove_punct = TRUE)
mydfm

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
topfeatures(mydfm, 20)  # 20 top words

## ----warning=FALSE, fig.width = 7, fig.height = 7-----------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)
textplot_wordcloud(mydfm, min.freq = 6, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
byPartyDfm <- dfm(data_corpus_irishbudget2010, groups = "party", remove = stopwords("english"), remove_punct = TRUE)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sort(byPartyDfm)[, 1:10]

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
recentCorpus <- corpus_subset(data_corpus_inaugural, Year > 1991)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myDict <- dictionary(list(terror = c("terrorism", "terrorists", "threat"),
                          economy = c("jobs", "business", "grow", "work")))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
byPresMat <- dfm(recentCorpus, dictionary = myDict)
byPresMat

## ---- eval = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  liwcdict <- dictionary(file = "~/Dropbox/QUANTESS/dictionaries/LIWC/LIWC2001_English.dic",
#                         format = "LIWC")
#  liwcdfm <- dfm(data_corpus_inaugural[52:58], dictionary = liwcdict, verbose = FALSE)
#  liwcdfm[, 1:10]

## ----fig.width = 6------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
presDfm <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980), 
               remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
obamaSimil <- textstat_simil(presDfm, c("2009-Obama" , "2013-Obama"), 
                             margin = "documents", method = "cosine")
obamaSimil
# dotchart(as.list(obamaSimil)$"2009-Obama", xlab = "Cosine similarity")

## ---- fig.width = 10, fig.height = 7, eval = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------
#  data(data_corpus_SOTU, package = "quantedaData")
#  presDfm <- dfm(corpus_subset(data_corpus_SOTU, Date > as.Date("1980-01-01")),
#                 stem = TRUE, remove_punct = TRUE,
#                 remove = stopwords("english"))
#  presDfm <- dfm_trim(presDfm, min_count = 5, min_docfreq = 3)
#  # hierarchical clustering - get distances on normalized dfm
#  presDistMat <- textstat_dist(dfm_weight(presDfm, "relfreq"))
#  # hiarchical clustering the distance object
#  presCluster <- hclust(presDistMat)
#  # label with document names
#  presCluster$labels <- docnames(presDfm)
#  # plot as a dendrogram
#  plot(presCluster, xlab = "", sub = "", main = "Euclidean Distance on Normalized Token Frequency")

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sim <- textstat_simil(presDfm, c("fair", "health", "terror"), method = "cosine", margin = "features")
lapply(as.list(sim), head, 10)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# make prettier document names
ieDfm <- dfm(data_corpus_irishbudget2010)
textmodel(ieDfm, model = "wordfish", dir = c(2, 1))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
quantdfm <- dfm(data_corpus_irishbudget2010, 
                remove_punct = TRUE, remove_numbers = TRUE, remove = stopwords("english"))
quantdfm <- dfm_trim(quantdfm, min_count = 4, max_docfreq = 10, verbose = TRUE)
quantdfm

if (require(topicmodels)) {
    myLDAfit20 <- LDA(convert(quantdfm, to = "topicmodels"), k = 20)
    get_terms(myLDAfit20, 5)
}

