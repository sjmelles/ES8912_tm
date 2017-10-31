##############################################################################
## ES8912 Code to read PDF files from Journal of Applied Ecology 
## and do some text mining
## Collaborative project with class of 2017
## Stephanie Melles
## October 22, 2017
#####

version
# load and install libraries  
install.packages(c("tm", "SnowballC")) # only need to do this step once 
install.packages(c("pdftools", "data.table", "plyr"))
# toggle the above line on and off if necessary



library(tm)
library(SnowballC)
library(pdftools)
library(data.table)
library(plyr)

# get and set working directory
getwd()
setwd("./PDFS/2007/bin64") # changes working directory to the folder with your pdfs

#Read in the csv for countries to join to. 
Countries<- read.csv("../../../countries_terms.csv", header = TRUE)

# Next we will create a vector of PDF file names 
files <- list.files(pattern = ".pdf")

# View your list of file names and make sure it matches your PDF files
edit(files) # this allows us to view the vector of file names that we just created. 

##############
# Next we will set up a wrapper function to read the PDFs using xpdf
Rpdf <- readPDF(control = list(text = "-f 1 -l 1"))

# Next we want to convert the PDF files to text and
# store them in a corpus. A corpus is a database for text. 
# We can do all that with the following code:
textdata <- Corpus(URISource(files), readerControl = list(reader = Rpdf))
# system.time(textdata <- Corpus(URISource(files), readerControl = list(reader = Rpdf)))
# approx 24 seconds for 110 pdfs.

?Corpus
?URISource

Sys.which("pdftotext")

# The Corpus function creates a corpus. 
# The first argument to Corpus is what we want to use to 
# create the corpus. In this case, it’s the vector of PDF files, 
# which we called 'files'.
# The URISource function is used to indicate that the files 
# vector contains Uniform Resource Identifiers. 
# In other words, we’re telling the Corpus function 
# that the vector of file names identifies our resources. 
# The second argument, readerControl, tells Corpus 
# which reader to use to read in the text from the PDF files. 
# That would be Rpdf, the wrapper function we created. 
# The readerControl argument requires a list of control 
# parameters, one of which is reader, 
# so we enter list(reader = Rpdf). 
# Finally we save the result to an object called “textdata”.
# And we’re done!  We read in the PDF files without manually 
# converting them to text. Just what we wanted to do. 
# Now we’re ready to do some text mining on our corpus. 

summary(textdata)
inspect(textdata[1])
edit(as.character(textdata[1]))
writeLines(as.character(textdata[1]))
corpus.array <- content(content(textdata)[[1]])
edit(corpus.array)
write.csv(corpus.array, "../../../output/JEarticle1.csv")

# creating two versions of a term document matrix. One (textdata1.tdm) has stopwords (like 'and') included, and only includes terms that are used 3 or more times (bounds)
textdata1.tdm <- TermDocumentMatrix(textdata, control = list(removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE, stemming = TRUE, removeNumbers = TRUE, bounds = list(global = c(3, Inf)))) 
textdata2.tdm <- TermDocumentMatrix(textdata, control = list(removePunctuation = TRUE, stopwords = FALSE, tolower = TRUE, stemming = FALSE, removeNumbers = TRUE, bounds = list(global = c(1, Inf)))) 

inspect(textdata1.tdm[1:10,])
inspect(textdata2.tdm[1:10,])

##Create a variable to create a df
Term<-textdata2.tdm$dimnames$Terms

## Create data frame to merge, while turning all the words into an lower case
TermDF<-data.frame(tolower(Term))

## rename the column name 
colnames(TermDF)<-"Term"

## Make country name variable all upper case
Countries$CNTRY_TERM<-(tolower(Countries$CNTRY_TERM))


## The joined Names
MergeTest<-merge(x = TermDF, y = Countries, by.x = "Term", by.y = "CNTRY_TERM" )

#Creating a Frequency column for terms to merge to the Joined Countries

TM<- as.matrix(textdata2.tdm)
# Freq gives sum of times the term was used in all papers in doc matrix
#FreqTM<-data.frame(ST= rownames(TM),
#                   Freq= rowSums(TM),
#                   row.names = NULL)

CountTM<-data.frame(ST= rownames(TM),
                   Cnt= rowSums(TM>0),
                   row.names = NULL)

# Merge country terms and count of articles they were used in
MergeTest2<-merge(x = MergeTest, y = CountTM, by.x = "Term", by.y = "ST")
edit(MergeTest2)

write.csv(MergeTest2, file = "../../../output/CountryJAECOUNTCalc2007.csv", row.names = TRUE)

##################################################
# now try merging in a different way
TM2<-as.data.frame(TM) # first data frame of terms and freq of use by article
TM2$Terms <- row.names(TM2)
# Countries will be the second data frame 
Countries$Terms <- Countries$CNTRY_TERM

###Using data.table

dt1 <- data.table(TM2, key = "Terms") 
dt2 <- data.table(Countries, key = "Terms")
joined.dt1.dt.2 <- dt1[dt2]

###Using plyr

joined.TM2.Countries <- join(TM2, Countries, type = "inner")
write.csv(joined.TM2.Countries, file = "../../../output/CountryJAEArticleCalc2007.csv", row.names = TRUE)

# plot(textdata2.tdm, corThreshold = 0.2, weighting = TRUE)
# plot doesn't work as it requires an older version of R; tm package not updated yet.

# playing 
# textdata2.tdm$dimnames$Terms
# Terms(textdata2.tdm)
# textdata2.tdm$dimnames$Docs
# Docs(textdata2.tdm)

# Explore function
?TermDocumentMatrix

# Under construction; make the text matrix one step at a time. First remove punctuation
textdata <- tm_map(textdata, removePunctuation)   


####################################
# Get info on disciplinary fields from key-words tag...
# Key-words:
