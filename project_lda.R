#see topicmodels.pdf
#https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf

#read dictionary
f_dict = "RTV_csv/thinned_combined/dictionary.csv"
df_dict = read.csv(f_dict,header = TRUE, stringsAsFactors = FALSE)

library(readtext)

#read DSIs
dir_dsi = "DSI/"
f_dsi = dir(dir_dsi)
l_dsi = lapply(f_dsi, function(f) {
  path = paste(dir_dsi,f,sep="")
  doc = readtext(path)
  return(doc)
})

for( j in seq(length(l_dsi))) {
  text = l_dsi[[j]]$text
  #gsub works
  for( i in seq(nrow(df_dict))) {
    tr = df_dict[i,]
    text = gsub(tr$Term,tr$Replacement, text, fixed=TRUE)
  }
  l_dsi[[j]]$text = text
}

#now build corpus
vs <- VectorSource(l_dsi)
cc = Corpus(vs)

########   LDA  ##############

Sys.setlocale("LC_COLLATE", "C")
dtm <- DocumentTermMatrix(cc,
                          control = list(stemming = FALSE, stopwords = TRUE, minWordLength = 3,
                                         removeNumbers = TRUE, removePunctuation = TRUE))
dim(dtm)


library("topicmodels")
k <- 6
SEED <- 2010
vem = LDA(dtm, k = k,
          control = list(estimate.alpha = FALSE, seed = SEED))


# terms: matrix of [k * numTerms]
# topics: matrix of [num docs * k]
p = posterior(vem)
p$terms[1,] # term values for 1st topic

#top 10 terms for each topic
tmx = 
  apply(p$terms,1,function(topic) {
    
    ii = order(topic,decreasing = TRUE)
    terms = topic[ii]
    terms = terms[1:10]
    nn = names(terms)
    
    df = data.frame(Term=nn,percent=terms)
    rownames(df)=NULL
    return(df)
  })

tmx

#combine lists into 1 data.frame for easier posting in discussion
for( i in seq_along(tmx)) {
  nn = names(tmx[[i]])
  nn = sprintf( "%s_%d",nn,i)
  names(tmx[[i]])=nn
}
nn = unlist(lapply(seq_along(tmx), function(i) {
  nn = names(tmx[[i]])
  nn = sprintf( "%s_%d",nn,i)
  return(nn)
}))
df_tmx = data.frame(lapply(tmx,c))
colnames(df_tmx)=nn
df_tmx













#########################################################
#this doesn't work
build_docs_from_rtv = function(df_all, num_dsi) {
  
  terms = df_all$Term
  counts = df_all[(1:num_dsi)+1]
  
  docs = apply(counts,2,function(col) {
    
    words = lapply(seq_along(col),function(i) {
      
      rep = ifelse(is.na(col[i]),0,col[i])
      rep = round(rep)
      
      v = NA
      if( rep > 0 ) {
        v = rep(terms[i],rep)
      }
      
      return(v)
      
    })
    
    #remove NA
    words = words[!is.na(words)]
    doc = paste(unlist(words),collapse = " ")
    
    return(doc)
  })
  
  names(docs) = colnames(counts)
  return(docs)
}

######### doesn't work ###############
#rebuild corpus from RTVs
f = "RTV_csv/thinned_combined_out/rtv_ec.csv"
df_all = read.csv(f,header = TRUE, stringsAsFactors = FALSE)
docs = build_docs_from_rtv(df_all, num_dsi)


#docs <- c("This is a text.", "This another one.")
vs <- VectorSource(docs)
#vc = VCorpus(vs)
cc = Corpus(vs)

tt = termFreq(docs)

inspect(VCorpus(vs))

Sys.setlocale("LC_COLLATE", "C")
dtm <- DocumentTermMatrix(cc,
                              control = list(stemming = FALSE, stopwords = TRUE, minWordLength = 3,
                                             removeNumbers = TRUE, removePunctuation = TRUE))
dim(dtm)


library("topicmodels")
k <- 8
SEED <- 2010
vem = LDA(dtm, k = k,
          control = list(estimate.alpha = FALSE, seed = SEED))
  

# terms: matrix of [k * numTerms]
# topics: matrix of [num docs * k]
p = posterior(vem)
p$terms[1,] # term values for 1st topic

#top 10 terms for each topic
tmx = 
apply(p$terms,1,function(topic) {
  
  ii = order(topic,decreasing = TRUE)
  terms = topic[ii]
  terms = terms[1:10]
  nn = names(terms)
  
  df = data.frame(Term=nn,percent=terms)
  rownames(df)=NULL
  return(df)
})

#combine lists into 1 data.frame for easier posting in discussion
nn = unlist(lapply(seq_along(tmx), function(i) {
  nn = names(tmx[[i]])
  nn = sprintf( "%s_%d",nn,i)
  return(nn)
}))
df_tmx = data.frame(lapply(tmx,c))
colnames(df_tmx)=nn
df_tmx


df_topics = data.frame(p$topics)
df_topics = replace_dsi_numbers(df_topics)
df_topics

##################################################



### this isn't working - need to go back to original docs and do EC substitution


docs_tm <-
  list(VEM = LDA(JSS_dtm, k = k, control = list(seed = SEED)),
       VEM_fixed = LDA(JSS_dtm, k = k,
                       control = list(estimate.alpha = FALSE, seed = SEED)),
       Gibbs = LDA(JSS_dtm, k = k, method = "Gibbs",
                   control = list(seed = SEED, burnin = 1000,
                                  thin = 100, iter = 1000)),
       CTM = CTM(JSS_dtm, k = k,
                 control = list(seed = SEED,
                                var = list(tol = 10^-4), em = list(tol = 10^-3))))

#calculate tfidf
term_tfidf <-
  tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
  log2(nDocs(dtm)/col_sums(dtm > 0))

summary(term_tfidf)


#get corpus
if(!nzchar(system.file(package = "corpus.JSS.papers"))) {
  templib <- tempfile(); dir.create(templib)
  install.packages("corpus.JSS.papers", lib = templib,
                     repos = "https://datacube.wu.ac.at/",
                     type = "source")
  data("JSS_papers", package = "corpus.JSS.papers",
         lib.loc = templib)
  } else {
    data("JSS_papers", package = "corpus.JSS.papers")
  }

jss_papers = JSS_papers


#alternate method
library("OAIHarvester")
x <- oaih_list_records("http://www.jstatsoft.org/oai", se = "jss:ART")
x <- x[vapply(x[, "metadata"], length, 1L) > 0L, ]
JSS_papers <- oaih_transform(x[, "metadata"])
JSS_papers <- JSS_papers[order(as.Date(unlist(JSS_papers[, "date"]))), ]

#For reproducibility of results we use only abstracts published up to 2010-08-05 and omit those
#containing non-ASCII characters in the abstracts.
JSS_papers <- JSS_papers[JSS_papers[,"date"] < "2010-08-05",]
JSS_papers <- JSS_papers[sapply(JSS_papers[, "description"],
                                   Encoding) == "unknown",]

#The final data set contains 342 documents. Before analysis we transform it to a "Corpus"
#using package tm. HTML markup in the abstracts for greek letters, subscripting, etc., is
#removed using package XML (Temple Lang 2010).
library("tm")
library("XML")

remove_HTML_markup <-
  function(s) tryCatch({
    doc <- htmlTreeParse(paste("<!DOCTYPE html>", s),
                         asText = TRUE, trim = FALSE)
    xmlValue(xmlRoot(doc))
    }, error = function(s) s)

corpus <- Corpus(VectorSource(sapply(JSS_papers[, "description"],
                                        remove_HTML_markup)))

#The corpus is exported to a document-term matrix using function DocumentTermMatrix()
#from package tm. The terms are stemmed and the stop words, punctuation, numbers and
#terms of length less than 3 are removed using the control argument. (We use a C locale for
#                                                                     reproducibility.)
Sys.setlocale("LC_COLLATE", "C")
JSS_dtm <- DocumentTermMatrix(corpus,
                                 control = list(stemming = TRUE, stopwords = TRUE, minWordLength = 3,
                                 removeNumbers = TRUE, removePunctuation = TRUE))
dim(JSS_dtm)

#The mean term frequency-inverse document frequency (tf-idf) over documents containing
#this term is used to select the vocabulary. This measure allows to omit terms which have low
#frequency as well as those occurring in many documents. We only include terms which have
#a tf-idf value of at least 0.1 which is a bit more than the median and ensures that the very
#frequent terms are omitted.
library("slam")
summary(col_sums(JSS_dtm))

#Min. 1st Qu. Median Mean 3rd Qu. Max.
#1.000 1.000 2.000 8.333 6.000 555.000
term_tfidf <-
  tapply(JSS_dtm$v/row_sums(JSS_dtm)[JSS_dtm$i], JSS_dtm$j, mean) *
  log2(nDocs(JSS_dtm)/col_sums(JSS_dtm > 0))

summary(term_tfidf)

#Min. 1st Qu. Median Mean 3rd Qu. Max.
#0.01490 0.07294 0.09749 0.12592 0.13800 1.08617

JSS_dtm <- JSS_dtm[,term_tfidf >= 0.1]
JSS_dtm <- JSS_dtm[row_sums(JSS_dtm) > 0,]
summary(col_sums(JSS_dtm))

#Min. 1st Qu. Median Mean 3rd Qu. Max.
#1.000 1.000 2.000 3.543 4.000 69.000

#After this pre-processing we have the following document-term matrix with a reduced vocabulary
#which we can use to fit topic models.
dim(JSS_dtm)
#[1] 342 1461

#In the following we fit an LDA model with 30 topics using (1) VEM with alpha estimated, (2)
#VEM with fixed and (3) Gibbs sampling with a burn-in of 1000 iterations and recording
#every 100th iterations for 1000 iterations. The initial alpha is set to the default value. By default
#only the best model with respect to the log-likelihood log(p(w|z)) observed during Gibbs
#sampling is returned. In addition a CTM is fitted using VEM estimation.
#We set the number of topics rather arbitrarily to 30 after investigating the performance with
#the number of topics varied from 2 to 200 using 10-fold cross-validation. The results indicated
#that the number of topics has only a small impact on the model fit on the hold-out data.
#There is only slight indication that the solution with two topics performs best and that the
#performance deteriorates again if the number of topics is more than 100. For applications a
#model with only two topics is of little interest because it enables only to group the documents
#very coarsely. This lack of preference of a model with a reasonable number of topics might be
#due to the facts that (1) the corpus is rather small containing less than 500 documents and
#(2) the corpus consists only of text documents on statistical software.
library("topicmodels")
k <- 30
SEED <- 2010
jss_TM <-
  list(VEM = LDA(JSS_dtm, k = k, control = list(seed = SEED)),
         VEM_fixed = LDA(JSS_dtm, k = k,
                           control = list(estimate.alpha = FALSE, seed = SEED)),
         Gibbs = LDA(JSS_dtm, k = k, method = "Gibbs",
                       control = list(seed = SEED, burnin = 1000,
                                        thin = 100, iter = 1000)),
         CTM = CTM(JSS_dtm, k = k,
                     control = list(seed = SEED,
                                      var = list(tol = 10^-4), em = list(tol = 10^-3))))

#model@loglikelihood contains the log likelihood for each document. How do I maximize? maximize the mean?
#my experimenting, 2 was best, each got progressively worse, as implied by the article. they just arbitrarily chose 30.
vem = LDA(JSS_dtm, k = 2, control = list(seed = SEED))
mean(vem@loglikelihood)
vem = LDA(JSS_dtm, k = 10, control = list(seed = SEED))
mean(vem@loglikelihood)
vem = LDA(JSS_dtm, k = 30, control = list(seed = SEED))
mean(vem@loglikelihood)
vem = LDA(JSS_dtm, k = 100, control = list(seed = SEED))
mean(vem@loglikelihood)
vem = LDA(JSS_dtm, k = 343, control = list(seed = SEED))
mean(vem@loglikelihood)


#To compare the fitted models we first investigate the alpha values of the models fitted with VEM
#and alpha estimated and with VEM and alpha fixed.

sapply(jss_TM[1:2], slot, "alpha")

#VEM VEM_fixed
#0.0117878 1.6666667

#We see that if alpha is estimated it is set to a value much smaller than the default. This indicates
#that in this case the Dirichlet distribution has more mass at the corners and hence, documents
#consist only of few topics. The infuence of alpha on the estimated topic distributions for documents
#is illustrated in Figure 1 where the probabilities of the assignment to the most likely
#topic for all documents are given. The lower alpha the higher is the percentage of documents
#which are assigned to one single topic with a high probability. Furthermore, it indicates that
#the association of documents with only one topic is strongest for the CTM solution.
#The entropy measure can also be used to indicate how the topic distributions differ for the four
#tting methods. We determine the mean entropy for each fitted model over the documents.
#The term distribution for each topic as well as the predictive distribution of topics for a
#document can be obtained with posterior(). A list with components "terms" for the term
#distribution over topics and "topics" for the topic distributions over documents is returned.
sapply(jss_TM, function(x)
  mean(apply(posterior(x)$topics,
               1, function(z) - sum(z * log(z)))))

# split it out to figure out what is happening
for( i in seq_along(jss_TM)) {
  
  #one model
  j = jss_TM[[i]]
  
  #predict model: gives terms and topics (how do I determine error?)
  t = posterior(j)
  
  mm = rep(0,nrow(t$topics))
  for( ii in nrow(t$topics)) {
    mm[ii] = sum(t$topics[ii,]-log(t$topics[ii,]))
  }
  mean(mm)
  
  mean(apply(t$topics,1,function(z) {
    print(z)
    - sum(z*log(z))
  }))

  ## t returns a list of 2 items:
  # terms: matrix of [k * numTerms]
  # topics: matrix of [num docs * k]
  t$terms[1,] # term values for 1st topic
  
  #sums to 1 - proportion of each term in the document
  sum(t$terms[1,])

  #rows sum to 1 - proportion of each topic in DSI
  t$topics[1,]
  sum(t$topics[1,])
  
  tt = t$topics %*% t$terms
  
  #matrix = [num docs * num terms]
  dim(tt)
  #rows sum to 1 - proportion of each term that is important to document (do I care?)
  
  #multiply to get topics per doc?
}

#VEM VEM_fixed Gibbs CTM
#0.4183708 3.1664120 3.2804883 0.2425984

#Higher values indicate that the topic distributions are more evenly spread over the topics.
#The estimated topics for a document and estimated terms for a topic can be obtained using
#the convenience functions topics() and terms(). The most likely topic for each document
#is obtained by


Topic <- topics(jss_TM[["VEM"]], 1)

#same as using which.max on postieror?
all_tt = topics(jss_TM[["VEM"]])
tt = apply(t$topics,1,which.max)
#yes!
sum(all_tt - tt)


#The five most frequent terms for each topic are obtained by
#matrix = [num terms * num topics]

Terms <- terms(jss_TM[["VEM"]], 5)
Terms[,1:5]

#Topic 1 Topic 2 Topic 3 Topic 4 Topic 5
#[1,] "lisp" "initi" "popul" "mixtur" "confid"
#[2,] "stat" "cell" "ecolog" "threshold" "kernel"
#[3,] "vista" "record" "captur" "densiti" "econometr"
#[4,] "excel" "mixlow" "rcaptur" "wavelet" "cca"
#[5,] "add" "mixtur" "period" "sequenc" "intern"

#If any category labelings of the documents were available, these could be used to validate
#the fitted model. Some JSS papers should have similar content because they appeared in the
#same special volume. The most likely topic of the papers which appeared in Volume 24 called
#'Statistical Modeling of Social Networks with `statnet"' is given by
(topics_v24 <-
topics(jss_TM[["VEM"]])[grep("v024", vapply(JSS_papers[, "identifier"],
 "[", 2, FUN.VALUE = ""))])

tt = topics(jss_TM[["VEM"]])

id = JSS_papers[, "identifier"]

#what is this doing?
# "[" is the function, 2 is the parameter.
# each item is a 2 item array of strings.
# this gets the 2nd item -> z[2]

#sapply and vapply do the same thing
lll = sapply(id, "[",2)
ll = vapply(id, "[",2,FUN.VALUE="")
zz = grep("v024",ll)
tt[zz]

#    235 236 237 238 239 240 241 242 243
#    27 11 27 14 27 27 27 25 27
most_frequent_v24 <- which.max(tabulate(topics_v24))

#The similarity between these papers is indicated by the fact that the majority of the papers
#have the same topic as their most likely topic. The ten most likely terms for topic 27 are
#given by

terms(jss_TM[["VEM"]], 10)[, most_frequent_v24]

#[1] "network" "ergm" "speci" "event"
#[5] "rare" "abund" "statnet" "graph"
#[9] "fechnerian" "fmri"

#Clearly this topic is related to the general theme of the special issue. This indicates that
#the fitted topic model was successful at detecting the similarity between papers in the same
#special issue without using this information.






data("AssociatedPress", package = "topicmodels")
lda <- LDA(AssociatedPress[1:20,], control = list(alpha = 0.1), k = 2)
topics(lda)
terms(lda,10)

lda_inf <- posterior(lda, AssociatedPress[21:30,])
topics(lda_inf)

## t returns a list of 2 items:
# terms: matrix of [k * numTerms]
# topics: matrix of [num docs * k]
lda_inf$terms[1,] # term values for 1st topic

#sums to 1 - proportion of each term in the document
sum(lda_inf$terms[1,])

#rows sum to 1 - proportion of each topic in DSI
lda_inf$topics[1,]
sum(lda_inf$topics[1,])

tt = lda_inf$topics %*% lda_inf$terms

