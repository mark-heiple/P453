#see topicmodels.pdf
#https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf


########   LDA  ##############

vs <- VectorSource(l_dsi)
cc = Corpus(vs)

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

tmx

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
k <- 5
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



##### use original corpus text, with EC substitutions #####
##### this is worse than using the ontology RTV       #####

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
  #remove punctuation
  txt = gsub('[[:punct:] ]+',' ',x$text)
  
  # stemming
  txt = stemDocument(txt,language="english")
  
  #add to list
  l_dsi[[j]]$text = text
}


#build RTV from list
build_rtv_from_corpus = function(f_dsi, l_dsi) {
  
  num_dsi = length(f_dsi)
  
  l_tf = lapply(l_dsi,FUN=function(x) {
    return(termFreq(txt))
  })
  
  #turn into term,num lists
  rtvs = lapply(l_tf,FUN=function(x) {
    return (data.frame(Term=names(x),Occurrences=as.numeric(x)))
  })
  
  #merge into 1 data.frame
  
  z = data.frame()
  dsinames = c("Term")
  for( i in seq_along(files)) {
    
    df = rtvs[[i]]
    
    dsinames = c(dsinames,f_dsi[i])
    if( nrow(z) == 0 ) {
      z = df
    } else { 
      z = merge(z, df, by="Term", all.x=TRUE,all.y=TRUE)
    }
    names(z) = dsinames
  }
  
  ni_col = apply(z[-1],MARGIN=1,FUN=function(x){
    length(which(x>0))
  })
  
  idf_col = log(num_dsi/ni_col)
  
  ## now calculate per DSI tf and tf*idf
  dsisum_row = apply(z[-1], MARGIN=2, FUN=sum,na.rm=TRUE)
  df_freq = z
  for( i in seq(num_dsi)) {
    df_freq[(i+1)] = df_freq[,(i+1)]/dsisum_row[i]
  }
  
  #tf*idf
  df_tfidf = df_freq
  for( i in seq(nrow(df_tfidf))) {
    df_tfidf[i,2:(num_dsi+1)] =  df_tfidf[i,2:(num_dsi+1)]*df_all$IDF[i]
  }
  
  #NA to 0
  z = apply(df_tfidf[2:(num_dsi+1)], MARGIN=2, FUN=function(x){ifelse(is.na(x),0,x)})
  z = t(z)
  
  return(z)
}

z = build_rtv_from_corpus(f_dsi,l_dsi)

#cluster
dist_z = dist(scale(as.matrix(z)))
cluster = hclust(dist_z,method="complete")
jpeg(file = paste(outdir,"fig_z1.jpg",sep=""), width = 1200, height = 600)
plot(cluster, xlab="",main="DSI Clustering by Frequency of Terms", hang=0, cex=1.0)
dev.off()

# Compute hierarchical clustering and cut into 3 clusters
res <- hcut(z, k = 3, stand = TRUE)

jpeg(file = paste(outdir,"fig_z1.jpg",sep=""), width = 960, height = 960)
fviz_dend(res, rect = TRUE, cex = 1.4, horiz=TRUE, labels_track_height = 30,
          k_colors = c("red","blue", "darkgreen", "purple"))
dev.off()


