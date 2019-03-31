##############################################################################
## From manuscript submitted to Ecoscience, March 2019
## Code to read PDF files from various journals of Applied Ecology and Conservation Biology 
## and do some text mining
## Collaborative project with Ryerson University, Applied Ecology (graduate level) 
## class of 2017 (ES8912)
## Stephanie Melles with contributions from Christopher Scarpone
## October - November, 2017
## NOTE that this code relies on pdftotext.exe version 4.01 
## to be installed and working on your machine.
## https://www.xpdfreader.com/pdftotext-man.html
##############################################################################

# set input/output directories 
inputdir = "./DemoPDFS/" 
# note this is directory with PDFs to be read 
# SJM note add line to make this directory if it doesn't exist

outputdir = "./output/"  
# this is the directory for output files.
outputname = "Demo"
year = "2007"

# load and install libraries  
# install.packages(c("tm", "SnowballC")) # only need to do this step once 
# install.packages(c("pdftools", "data.table", "plyr"))
# toggle the above line on and off if necessary
# by removing the hashtag and running the command with Ctrl + Enter

library(tm)
library(SnowballC)
library(pdftools)
library(data.table)
library(plyr)

#Read in the csv for countries to join to. 
Countries<- read.csv("countries_terms.csv", header = TRUE)

# Next we will create a vector of PDF file names 
files <- list.files(path = inputdir, pattern = ".pdf")

##############
# Next we will set up a wrapper function to read the PDFs using xpdf
Rpdf <- readPDF(control = list(text = "-f 1 -l 2"))
# The control parameters, '-f 1 -l 2' tells pdftotext.exe to read
# the first page '-f' from page '1', and the last page '-l' as page 2. 
# XPDFtools has numerous control parameters that determine how PDFs 
# are scanned to text. Please see associated documentation for 
# XPDFtools.

# Next we want to convert the PDF files to text and
# store them in a corpus. 

# A CORPUS is a database for text. 
# We can do all that with the following code:
setwd(inputdir)
textdata <- Corpus(URISource(files), readerControl = list(reader = Rpdf))
setwd("../")

# how long did that take?
# system.time(textdata <- Corpus(URISource(files), readerControl = list(reader = Rpdf)))
# approx 24 seconds for 110 pdfs.

# Toggle next two lines to get help on functions
# ?Corpus # gives more info and help files on the Corpus function in package tm
# ?URISource # gives more info and help on the URISource function in package tm

# Toggle next line to check where pdftotext is
# Sys.which("pdftotext") # returns where your system is finding the pdftotext executable files

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

# the following lines of code are just various ways of looking at 
# the text matrix data and metadata that is stored in our corpus.
summary(textdata) # summarizes details of the corpus
# inspect(textdata[1])
# edit(as.character(textdata[1])) # opens a text editor window
# writeLines(as.character(textdata[1])) # writes the textdata 
# (first element of corpus selected with square brackets) to the screen
corpus.array <- content(content(textdata)[[1]]) # saves the content of the text to an object called corpus.array
# edit(corpus.array) #opens an editing window that shows all text strings separated by commas and concatenated together (c("",""))
# write.csv(corpus.array, "../../../output/AppliedEco/JEarticle1.csv") # writes the contents of corpus.array to a comma delimited file 
# so we can look at it in notepad or wherever if we wish.
# this is just for practice. We will not use this .csv file


#####################################
# Next we create a few versions of a TERM DOCUMENT MATRIX. 

# textdata1.tdm has stopwords (like 'and') excluded (stopwords = TRUE), and only includes terms that are used in 10 or more PDFs across all scanned PDFs (bounds = list(global = c(10, Inf)))))
textdata1.tdm <- TermDocumentMatrix(textdata, control = list(removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE, stemming = FALSE, removeNumbers = TRUE, bounds = list(global = c(10, Inf)))) 
# textdata2.tdm) has stopwords (like 'and') excluded (stopwords = TRUE), and includes all terms that are used (bounds) or more PDFs across those that are scanned (bounds = list(global = c(1, Inf)))))
textdata2.tdm <- TermDocumentMatrix(textdata, control = list(removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE, stemming = FALSE, removeNumbers = TRUE, bounds = list(global = c(1, Inf)))) 
# Explore function
# ?TermDocumentMatrix


# EXTRACT only the array of terms in each 
text.data1.array<-as.data.frame.array(textdata1.tdm)
text.data2.array<-as.data.frame.array(textdata2.tdm)

# WRITE the array to a .csv file
write.csv(text.data1.array, paste(outputdir, outputname, "textdata1array.csv", sep=""))
write.csv(text.data2.array, paste(outputdir, outputname,"textdata2array.csv", sep=""))

# inspect(textdata1.tdm[1:10,]) # inspect first 10 rows of tdm
# inspect(textdata2.tdm[1:10,])
# inspect(textdata2.tdm) #most frequent terms? 

##########################################
# Now we will match up the country terms to our text data matrix

##Create a variable of 'terms' 
Term<-textdata2.tdm$dimnames$Terms
# Term

## Create data frame to merge, while turning all the words to lower case
TermDF<-data.frame(tolower(Term))
head(TermDF)

## rename the column name 
colnames(TermDF)<-"Term"
head(TermDF)

## Make country name variable all lower case
Countries$CNTRY_TERM<-(tolower(Countries$CNTRY_TERM))

## Merge the terms from our term doc matrix with countries in our list
MergeTerm<-merge(x = TermDF, y = Countries[, 5:11], by.x = "Term", by.y = "CNTRY_TERM" )
# have a look at MergeTest
head(MergeTerm) # just prints first 5 rows to console

# Create a Frequency column for terms to merge to the Joined Countries
TM<- as.matrix(textdata2.tdm)

# Freq gives sum of times the term was used in all papers in term doc matrix
FreqTM<-data.frame(ST= rownames(TM),
                   Freq= rowSums(TM),
                   row.names = NULL)

# Count gives count of times the term was used at least once across all papers in term doc matrix
CountTM<-data.frame(ST= rownames(TM),
                   Cnt= rowSums(TM>0),
                   row.names = NULL)


# Merge country terms and count of articles they were used in
MergeTerm2<-merge(x = MergeTerm, y = CountTM, by.x = "Term", by.y = "ST")
# edit(MergeTest2)

# Bring in freq of use - just in case we need it.
MergeTerm3<-merge(x = MergeTerm, y = FreqTM, by.x = "Term", by.y = "ST")
# Combine it with our count
MergeTerm2$Freq <- MergeTerm3$Freq

write.csv(MergeTerm2, file = paste(outputdir, outputname, "CountryCOUNT.csv", sep=""), row.names = TRUE)

# sort Merged dataframe by continent
MergeTerm4<- arrange(MergeTerm2, Continent) 

MergeTerm4 <- data.table(MergeTerm4)

ContinentCount <- MergeTerm4[, sum(Cnt), by = Continent]
names(ContinentCount)[2] <- year

write.csv(ContinentCount, file = paste(outputdir, outputname, "ContinentCount.csv", sep=""), row.names = TRUE)



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
#edit(joined.dt1.dt.2)

###Using plyr
joined.TM2.Countries <- join(TM2, Countries, type = "inner")
write.csv(joined.TM2.Countries, file = paste(outputdir, outputname, "CountryArticleCalc.csv", sep=""), row.names = TRUE)

##########################################################



