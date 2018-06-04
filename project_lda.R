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

#substitute my dictionary values into
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


################# this didn't work ################
## used my ontology RTV to recreate DSIs.
## it just has terms from the RTV repeated n times
#########################################################

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

#rebuild corpus from RTVs
f = "RTV_csv/thinned_combined_out/rtv_ec.csv"
df_all = read.csv(f,header = TRUE, stringsAsFactors = FALSE)
docs = build_docs_from_rtv(df_all, num_dsi)

#build corpus
vs <- VectorSource(docs)
cc = Corpus(vs)
tt = termFreq(docs)

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

