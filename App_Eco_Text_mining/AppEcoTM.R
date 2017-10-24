##############################################################################
## ES8912 Code to read PDF files from Journal of Applied Ecology 
## and do some text mining
## Collaborative project with class of 2017
## Stephanie Melles
## October 22, 2017
##############################################################################

# load and install libraries  
install.packages(c("tm", "SnowballC")) # only need to do this step once 
# toggle the above line on and off if necessary

library(tm)
library(SnowballC)

# get and set working directory
getwd()
setwd("./PDFS/2007") # changes working directory to the folder with your pdfs

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
textdata <- Corpus(URISource(files), 
                  readerControl = list(reader = Rpdf))

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
write.csv(corpus.array, "../../output/JEarticle1.csv")


textdata1.tdm <- TermDocumentMatrix(textdata, control = list(removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE, stemming = TRUE, removeNumbers = TRUE, bounds = list(global = c(3, Inf)))) 

textdata2.tdm <- TermDocumentMatrix(textdata, control = list(removePunctuation = TRUE, stopwords = FALSE, tolower = TRUE, stemming = FALSE, removeNumbers = TRUE, bounds = list(global = c(1, Inf)))) 

inspect(textdata1.tdm[1:10,])
inspect(textdata2.tdm[1:10,])

textdata2.tdm$dimnames$Terms

test<-inspect(textdata2.tdm[1:10,])

plot(textdata2.tdm, corThreshold = 0.2, weighting = TRUE)

edit(test)

textdata2.tdm$dimnames$Terms
Terms(textdata2.tdm)
textdata2.tdm$dimnames$Docs
Docs(textdata2.tdm)


?TermDocumentMatrix

# Remove punctuation
textdata <- tm_map(textdata, removePunctuation)   


####################################
# Key-words:
