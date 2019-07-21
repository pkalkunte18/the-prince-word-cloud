textDoc <- readLines(file.choose()) #imported the Prince

#make docs interactable
docs <- Corpus(VectorSource(textDoc))
#inspect(docs)
 
#make a method to replace things with spaces
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
addE <- content_transformer(function(x, pattern) gsub(pattern, paste(pattern, "e "), x))

#time to clean some stuff up
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, content_transformer(tolower)) #words to lowercase
docs <- tm_map(docs, removeNumbers)
docs<- tm_map(docs, removeWords, stopwords("english")) #gets rid of prepositions, etc.
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument) #stemming gets rid of adjustments to root terms (moving, moved ---> move)

#specific words to remove for Machiavelli's The Prince
docs<- tm_map(docs, removeWords, c("one", "can", "may", "therefor", "fortun", "peopl", "gutenberg", "gutenbergtm", "mani", "castruccio"))

#now to make a doccument matrix to contain the frequency of words
docMatrix <- TermDocumentMatrix(docs)
m <- as.matrix(docMatrix)
v <- sort(rowSums(m), decreasing = TRUE)

#last minute translation cleaning for the Prince specifically
names(v)[1] <- "prince"
names(v)[7] <- "people"
names(v)[12] <- "therefore"

#let's look at our most popular words
d <- data.frame(word = names(v), freq = v)
head(d, 15)

#generating the wordcloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 2, 
          max.words = 200, random.order = FALSE, 
          rot.per = .35, colors = brewer.pal(8, "Dark2"))

#some further analysis