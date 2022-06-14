#Packages
#This package was used for the slice function and the %>%
library(dplyr)
#This package was used for visualization
library(ggplot2)
#This package was used to tune the number of topics and visualize the metrics
library(ldatuning)
#This package was used to construct the corpus and the document-term matrix
library(tm)
#This package was used to conduct the Latent Dirichlet Allocation
library(topicmodels)

#Loading in the data
#Loading the titles for each paper
load("latent_dirichlet_allocation/papers.rda")
#Loading the textual data
load("latent_dirichlet_allocation/content.rda")

#Preprocessing the data
#The alphanumerical characters were removed beforehand
text <- stringr::str_replace_all(text, "[^[:alnum:]]", " ")
#The last four characters ".pdf" were removed from the titles
lf <- gsub('.{4}$', '', lf)
#A dataframe for the corpus was created
text <- data.frame(lf[1:201], text)
#Renaming the column with the titles to "doc_id"
names(text)[1] <- "doc_id"

#Creating a corpus from the dataframe
myCorpus <- Corpus(DataframeSource(text))

#All characters in the corpus were lower-cased 
Corpus <- tm_map(myCorpus, content_transformer(tolower))

#Gathering external English stopwords dataset
english_stopwords <- readLines("https://slcladal.github.io/resources/stopwords_en.txt", encoding = "UTF-8")
#Adding additional stopwords based on the topic of this research and the generated results were added
english_stopwords <- c(english_stopwords,"audit","auditor","auditors","audits","auditing","audited","data","analytics","analysis","accounting","accountancy", "model", "modeling", "modelling","technology","technological", "technologies","research", "researching","researches","researched","researcher","https","doi","pdf","accessed", "journal", "horizon","vasarhelyi", "studies","study","paper","papers","article","articles","author","authors","literature","reference","references","source","sources","keyword","keywords","review","reviews","reviewing","reviewed","company","companies","firms","firm","client","clients","year","years","figure","figures","table","tables", "business","businesses","digital","digitalization","digitalisation", "tion","ing")
#All stop words were removed from the corpus
Corpus <- tm_map(Corpus, removeWords, english_stopwords)

#The punctuation was removed from the corpus
Corpus <- tm_map(Corpus, removePunctuation)

#All numbers were removed from the corpus
Corpus <- tm_map(Corpus, removeNumbers)

#All terms in the corpus were stemmed
Corpus <- tm_map(Corpus, stemDocument, language = "en")

#All white spaces were removed from the corpus
Corpus <- tm_map(Corpus, stripWhitespace)

#The corpus was finalized
Corpus

#Document-term matrix
#The corpus was turned into a document-term matrix 
DTM <- DocumentTermMatrix(Corpus)

#Removing infrequent terms with the sparseness
DTM <- removeSparseTerms(DTM, 0.99)

#The document-term matrix was finalized
DTM
#Dimensions of the document-term matrix (201 documents and 7411 terms)
dim(DTM)
#Inspecting the document-term matrix
inspect(DTM)

#LDA model
#Tuning the number of topics (k)
result <- FindTopicsNumber(
  DTM,
  topics = seq(from = 2, to = 100, by = 2),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)

#Plotting the results of the relevant metrics to determine the number of topics (k)
FindTopicsNumber_plot(result)

#Assigning a value to the number of topics (k)
K <- 22

#Creating the LDA topic model
#Set seed to reproduce the same results on the used device
set.seed(777)
#Compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 1000, verbose = 25))

#Results
#Extracting all terms from the document-term matrix with their corresponding frequency
highest_freq <- slam::col_sums(DTM)
#The terms with a frequency >= 100 were extracted from the document-term matrix
highest_freq <- highest_freq[highest_freq >= 100]
#Creating a dataframe with all words with a frequency >= 100 and their corresponding frequency
frequency <- data.frame(words = names(highest_freq), freq = highest_freq , row.names = NULL)

#The ten most frequent terms were extracted from the "frequency" dataframe
top_10_terms <- frequency %>% 
  arrange(desc(freq)) %>% 
  slice(1:10)

#The ten most frequent terms
top_10_terms

#The ten most frequent terms with their corresponding frequency were visualized
ggplot(top_10_terms, aes(x = freq, y = reorder(words, freq))) +
  geom_bar(
    stat = "identity", position = position_dodge(0.8),
    width = 0.7, color="black", fill="light blue") +
  geom_text(aes(label = freq),hjust = -0.3, size = 3.5)+
  labs(x= "Count", y="Term", title = "Ten most frequent terms")

#Generating the topics including the ten most likely terms
terms(topicModel, 10)

#Labels based on the researcher's interpretation were manually assigned to the topics
topic_n_1 <- 1:22
interpretation_1 <- c("Adoption Computer assisted audit techniques (CAAT)", "Management accounting", "Future of auditing", "Audit evidence", "Generalized audit software (GAS) and Computer assisted audit techniques (CAAT)", "Artificial intelligence", "Machine learning", "Data security and privacy", "Big Data","Audit data analytics (ADA)", "Academic development", "Financial reporting", "Internet of things", "Textual analysis", "Machine learning (2)", "Clustering", "Blockchain", "Visualization tools for audit", "Big data analytics (BDA)","Robotic process automation", "Risk", "Fraud detection" )
interpretation_model <- data.frame(topic_n_1,interpretation_1)
names(interpretation_model) <- c("topic", "interpretation")
interpretation_model

#Topic ranking
#Extracting the relevant parameters from the LDA model for the probability and rank-1 methods
#The betas of each term and thetas of each topic were extracted
tmResult <- posterior(topicModel)
attributes(tmResult)
#Extracting the beta of each term
beta <- tmResult$terms
#Extracting the theta of each topic
theta  <- tmResult$topics

#Probability
#Mean probabilities over all papers
topicProportions <- colSums(theta) / nDocs(DTM)
#Assigning names based on the top five most likely terms
topicNames <- apply(lda::top.topic.words(beta, 5, by.score = T), 2, paste, collapse = " ")
names(topicProportions) <- topicNames
#Sorting the topics with their corresponding probability (decreasing)
prob <- sort(topicProportions, decreasing = TRUE)
#Formatting the result
paste(round(prob, 5), ":", names(prob))

#Labels based on the researcher's interpretation were manually assigned to the topics with their corresponding probability
probability <- c(0.13238, 0.06063, 0.05867, 0.05795, 0.05570, 0.05044, 0.04958, 0.04818, 0.04733, 0.04263, 0.04078, 0.03890, 0.03655, 0.03642, 0.03637, 0.03325, 0.03145, 0.03071, 0.03039, 0.02815, 0.02693, 0.02658)
interpretation_2 <- c("Future of auditing", "Risk", "Blockchain", "Big Data", "Internet of things", "Fraud detection", "GAS and CAAT", "Machine learning", "Academic development", "Artificial intelligence", "Audit evidence", "Robotic process automation", "Adoption CAATT", "Management accounting", "Financial reporting", "Data security and privacy", "ADA", "Clustering", "Textual analysis", "Machine learning (2)", "BDA", "Visualization tools for audit")
interpretation_model_probability <- data.frame(probability,interpretation_2)
names(interpretation_model_probability) <- c("probability", "topic")
interpretation_model_probability

#The dataframe with the topics and corresponding probability was visualized. A width of 1750 and a height of 900 is recommended to view the plot.
ggplot(interpretation_model_probability, aes(x = probability, y = reorder(topic, probability))) +
  geom_bar(
    stat = "identity", position = position_dodge(0.8),
    width = 0.7, color="black", fill="light blue") +
  geom_text(aes(label = round(probability,3)),hjust = -0.3, size = 5)+
  labs(x= "Probability", y="Topic", title = "Topics with corresponding probability")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18), title = element_text(size=22))

#Rank-1
#Creating a vector with k (22) number of 0's 
countsOfPrimaryTopics <- rep(0, K)
#Adding the names based on the top five most likely terms to the vector, turning it into a named vector containing 0's
names(countsOfPrimaryTopics) <- topicNames
#Counting the primary topic of each document and assigning the count to the corresponding topic
for (i in 1:nDocs(DTM)) {
  topicsPerDoc <- theta[i, ] 
  primaryTopic <- order(topicsPerDoc, decreasing = TRUE)[1] 
  countsOfPrimaryTopics[primaryTopic] <- countsOfPrimaryTopics[primaryTopic] + 1
}
#Sorting the primary topics (decreasing)
sort(countsOfPrimaryTopics, decreasing = TRUE)

#Labels based on the researcher's interpretation were manually assigned to the topics with their corresponding number of papers
papers <- c(22,19,15,13,12,12,12,11,10,8,8,8,7,6,6,5,5,5,5,5,4,3)
interpretation_3 <- c("Future of auditing", "Blockchain", "Big Data", "Internet of things", "Fraud detection", "GAS and CAAT","Academic development","Risk","Machine learning", "Artificial intelligence", "Robotic process automation", "ADA", "Adoption CAAT", "Data security and privacy", "Visualization tools for audit", "Management accounting", "Clustering","Textual analysis", "Machine learning (2)","BDA","Financial reporting", "Audit evidence")
interpretation_model_numbers <- data.frame(papers, interpretation_3)
names(interpretation_model_numbers) <- c("Papers", "Topic")
interpretation_model_numbers

#The dataframe with the topics and corresponding number of papers was visualized. A width of 1750 and a height of 900 is recommended to view the plot
ggplot(interpretation_model_numbers, aes(x = Papers, y = reorder(Topic, Papers))) +
  geom_bar(
    stat = "identity", position = position_dodge(0.8),
    width = 0.7, color="black", fill="light blue") +
  geom_text(aes(label = Papers),hjust = -1, size = 5)+
  labs(x= "Number of papers", y="Topic", title = "Topics with the corresponding total number of papers")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18), title = element_text(size=22))

#Distribution of papers
#All gammas were extracted from the model
gamma <- tidytext::tidy(topicModel, matrix = "gamma")
#The topic with the highest gamma (primary topic) was filtered out for each paper
highest_gamma <- gamma %>% group_by(document) %>% top_n(1, gamma)
#Labels based on the researcher's interpretation were manually assigned to the primary topics
highest_gamma <- highest_gamma %>% 
  tibble::add_column(label = ifelse(.$topic == 1, "Adoption CAAT",
                                    ifelse(.$topic == 2, "Management accounting",
                                           ifelse(.$topic == 3, "Future of auditing",
                                                  ifelse(.$topic == 4, "Audit evidence",
                                                         ifelse(.$topic == 5, "GAS & CAAT",
                                                                ifelse(.$topic == 6, "Artificial intelligence",
                                                                       ifelse(.$topic == 7, "Machine learning",
                                                                              ifelse(.$topic == 8, "Data security and privacy",
                                                                                     ifelse(.$topic == 9, "Big Data",
                                                                                            ifelse(.$topic == 10, "Audit data analytics", 
                                                                                                   ifelse(.$topic == 11, "Academic development",
                                                                                                          ifelse(.$topic == 12, "Financial reporting",
                                                                                                                 ifelse(.$topic == 13, "Internet of things",
                                                                                                                        ifelse(.$topic == 14, "Textual analysis",
                                                                                                                               ifelse(.$topic == 15, "Machine learning (2)", ifelse(.$topic == 16, "Clustering", ifelse(.$topic == 17, "Blockchain", ifelse(.$topic == 18, "Visualization tools for audit", ifelse(.$topic == 19, "Big data analytics", ifelse(.$topic == 20, "Robotic process automation", ifelse(.$topic == 21, "Risk", ifelse(.$topic == 22, "Fraud detection", "NA")))))))))))))))))))))))

#This resulted in the finalized list consisting of all papers with their corresponding topic and label
highest_gamma

# Session info
sessionInfo()

# References

#Grun, B., Hornik, K., Blei, D., Lafferty, J., Phan, X., Matsumoto, M., Nishimura, T., & Cokus, S. 2021. Package 'topicmodels'. url: https://cran.r-project.org/web/packages/topicmodels/topicmodels.pdf
#Niekler, A., & Wiedemann, G. 2020. Tutorial 6: Topic Models. url: https://tm4ss.github.io/docs/Tutorial_6_Topic_Models.html
#Schweinberger, M. 2022. Topic Modeling with R. Brisbane: The University of Queensland. url: https://slcladal.github.io/topicmodels.html (Version 2022.03.18).
#Silge, J., & Robinson, D. 2017. Text mining with R: A tidy approach. " O'Reilly Media, Inc.".

