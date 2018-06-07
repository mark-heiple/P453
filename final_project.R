library(ape)

#http://www.sthda.com/english/wiki/factoextra-r-package-easy-multivariate-data-analyses-and-elegant-visualization
library(factoextra)
library(FactoMineR)

#C-like printf function
printf = function(...) {
  invisible(print(sprintf(...)))
}

#for reading xls files
library(gdata)

#the xlsx files were originally created by running FiveFilters, then
#copying the extracted terms to an excel spreadsheet. This function
#converts them back to csv so they can be usd in R.
# inbase: directory containing xlsx files
# outbase: directory where csv files are to be written
xlsx_to_csv = function(inbase, outbase) {
  
  dir.create(outdir)
  
  files = dir(indir)
  for( f in files) {
    
    printf( "converting %s...", f)
    
    fo = gsub(".xlsx",".csv",x=f,fixed = TRUE)
    f_in = paste(indir,f,sep="")
    f_out = paste(outdir,fo,sep="")
    
    df = read.xls(f_in, sheet=1, header=TRUE)
    
    #remove word count column (don't need it)
    df = df[,1:2]
    
    write.csv(df, f_out,row.names=FALSE)
  }
}

#input files are in RTV_xls, save csv files to RTV_csv/raw/
inbase = "RTV_xls/"
outbase = "RTV_csv/raw/"
xlsx_to_csv(indir,outdir)

#### show which csv's contain terms that are not ascii. It doesn't actually modify anything, 
# you must manually copy orginal files to cleaned and edit them there
library(stringi)

#the directory that where the excel files were converted in the previous step
indir = "RTV_csv/raw/"

files = dir(indir)
for( i in seq_along(files)) {
  f = files[i]
  if( length(grep(".csv",f,fixed = TRUE))>0) {
    f_in = paste(indir,f,sep="")
    printf("%s...",f_in)
    df = read.csv(f_in, header=TRUE)
    print(which(!stri_enc_isascii(df$Term)))
  }
}

######################################################################
# reorder individual files by combined tf*idf
# df_all is a data.frame with all the terms and tf*idf values
#   it was created by combine_csv()
# indir contains all of the individual RTVs for each DSI (1 per DSI)
# sorted RVS will be saved to outdir
#
# This is used to make it easier to remove unimportant terms
#####################################################################
sort_by_tfidf = function(df_all, indir, outdir) {
  
  files = dir(indir)
  for( i in seq_along(files)) {
    f = files[i]
    printf( "sorting by tf*idf: %s...", f)
    
    f_in = paste(indir,f,sep="")
    f_out = paste(outdir,f,sep="")
    
    df = read.csv(f_in, stringsAsFactors = FALSE, header=TRUE)
    df = df[,c("Term","Occurrence")]
    
    sorted = order(ordered(df$Term,levels=df_all$Term))
    df = df[sorted,]
    
    write.csv(df, f_out,row.names=FALSE)
  }
}

###############################################################################
# sort columns of data.frame so that they are grouped into the assumed clusters
#
# df: the data.frame to sort
# returns: sorted data.frame
##############################################################################
sort_df = function(df) {
  
  sortby = c("15","18","77","89",
             "38","66","80","103",
             "22","61","93","94")
  
  ff = ordered(sortby,levels=sortby)
  nn = colnames(df)
  o = order(ordered(nn,levels=ff))
  sorted = df[,o]
  
  return(sorted)  
}

########################################################
# replace the row names of the data.frame with titles
# It is assumed that the input data.frame has row names
# that are the original DSI numbers
#
# returns a data.frame with titles
########################################################
replace_dsi_numbers = function(df) {
  
  #numbers order must match title order
  numbers = c(15,18,22,38,61,66,77,80,89,93,94,103)
  
  #these may be too long to plot well
  titles = c(
    "Trump Insists Mexico Will Pay for Wall",
    "House GOP, Trump team hatch border wall plan",
    "Trump acknowledges Russia role in U.S. election hacking",
    "Tillerson on North Korea: Military action an option",
    "Trump says Russia collusion is all a hoax",
    "Preventative War Revisited",
    "White House seeks $18B to extend border wall",
    "Trump's Biggest Foreign Policy Headache For 2018",
    "More Control Over U.S. Lands for Border Wall",
    "Trump expels diplomats",
    "Trump Putin Meeting",
    "A U.S.-North Korea summit: What could possibly go wrong"
  )
  
  
  #get rows - these are named (X{num})
  rn = rownames(df)
  rn = gsub("X","",rn)
  rn = as.numeric(rn)
  
  ff = ordered(rn, levels=rn)
  nn = order(ordered(numbers,levels=ff))
  rownames(df) = titles[nn]
  return(df)
}

#####################################################################
# combine RTVs of individual DSIs into one RTV, each
# DSI is a column. It also calculates tf*idf values
# inbase: directory containing individual RTV .csv files
# outbase: directory where combined files will be written.
#   Several files will be created here:
#       rtv_ec.csv: each DSI column has n (the number of
#                   occurrences of a term) for its DSI
#                   includes global NI, TF, and TF*IDF columns
#
#       rtv_tf.csv: each DSI column is the term frequency of the 
#                   term in that DSI
#
#       rtv_tfidf.csv: each DSI column is the tf*idf of the 
#                      term in that DSI
#
#       rtv_nidf.csv:  each DSI column is the n*idf of the 
#                      term in that DSI
#
# returns: a data.frame that is the same as rtv_ec.csv
#####################################################################
combine_csv = function( inbase, outbase) {
  
  #input and output directories
  indir = paste(curdir,inbase,sep="")
  outdir = paste(curdir,outbase,sep="")
  dir.create(outdir)
  
  #input file list
  files = dir(indir)
  #just the dsis
  files = files[grep ("dsi-", files, fixed=TRUE)]
  
  #start new data frame
  z = data.frame()
  dsinames = c("Term")
  printf( "combining %s...", indir)
  for( i in seq_along(files)) {
    f = files[i]
    printf( "adding %s...", f)
    
    f_in = paste(indir,f,sep="")
    df = read.csv(f_in, stringsAsFactors = FALSE, header=TRUE)
    
    #truncate name for column name
    dsicol = gsub(".csv","",f)
    dsicol = gsub("dsi-","",dsicol)
    dsinames = c(dsinames,dsicol)
    
    if( nrow(z) == 0 ) {
      z = df
    } else { 
      z = merge(z, df, by="Term", all.x=TRUE,all.y=TRUE)
    }
    names(z) = dsinames
  }
  
  #sort columns by DSI
  df_all = sort_df(z[-1])
  df_all = cbind(z[1],df_all)
  
  #now do corpus-wide term frequencies
  num_dsi = ncol(df_all)-1
  total_tf = sum(df_all[,2:(num_dsi+1)],na.rm=TRUE)
  sum_col = apply(df_all[,2:(num_dsi+1)],MARGIN=1,FUN=sum,na.rm=TRUE)
  tf_col = sum_col/total_tf
  ni_col = apply(df_all[,2:(num_dsi+1)],MARGIN=1,FUN=function(x){
    length(which(x>0))
  })
  
  idf_col = log(num_dsi/ni_col)
  tfidf_col = tf_col*idf_col
  
  #add to df_all
  df_all = cbind(df_all,"Total Occurances"=sum_col,"TF"=tf_col, "NI"=ni_col,"IDF"=idf_col,"tf*idf"=tfidf_col)
  
  #sort by tf*idf
  df_all = df_all[order(df_all[["tf*idf"]], decreasing=TRUE),]
  
  ### other individual columns are now invalid!!! ###
  rm(idf_col)
  rm(tfidf_col)
  rm(ni_col)
  rm(sum_col)
  rm(tf_col)
  
  f_out = paste(outdir,"rtv_ec.csv",sep="")
  write.csv(df_all,f_out,na="",row.names=FALSE)
  
  ## now calculate per DSI tf and tf*idf
  dsisum_row = apply(df_all[,2:(num_dsi+1)], MARGIN=2, FUN=sum,na.rm=TRUE)
  df_freq = df_all[1:(num_dsi+1)]
  for( i in seq(num_dsi)) {
    df_freq[(i+1)] = df_freq[,(i+1)]/dsisum_row[i]
  }
  
  #just term freq
  f_out = paste(outdir,"rtv_tf.csv",sep="")
  write.csv(df_freq,f_out,na="",row.names=FALSE)
  
  #tf*idf
  df_tfidf = df_freq
  for( i in seq(nrow(df_tfidf))) {
    df_tfidf[i,2:(num_dsi+1)] =  df_tfidf[i,2:(num_dsi+1)]*df_all$IDF[i]
  }
  f_out = paste(outdir,"rtv_tfidf.csv",sep="")
  write.csv(df_tfidf,f_out,na="",row.names=FALSE)
  
  #n*idf
  df_nidf = df_all[1:13]
  for( i in seq(nrow(df_nidf))) {
    df_nidf[i,2:(num_dsi+1)] =  df_nidf[i,2:(num_dsi+1)]*df_all$IDF[i]
  }
  f_out = paste(outdir,"rtv_nidf.csv",sep="")
  write.csv(df_nidf,f_out,na="",row.names=FALSE)
  
  return(df_all)
}

####################################################################################
# selectively removes terms from df. Also removes specific bad terms.
# df_all: has the tf*idf values that is optionally used to remove values
# df: the input data.frame that will be pruned.
# threshold: if specified, terms that have a tf*idf value < threshold will be removed
# singles: if TRUE, terms that only appear once will be removed
#
# returns: pruned data.frame
#####################################################################################
remove_terms = function(df_all, df, threshold = NA, singles = FALSE) {
  
  bad_terms = c(
    "usMedia",
    "presidentObama",
    
    "presidentTrump",
    "trumpAdministration",
    "unitedStates",
    "trumpCampaign",
    "trumpTweets",
    "presidentBush",
    "obamaAdministration",
    "democrat",
    "republican"
  )
  #remove rows that are only appear once
  ss = which(df_all$`Total Occurances`==1)
  if( length(ss)>0) {
    df = df[-ss,]
  }
  
  #remove specific terms that are not helping
  bad_rows = which(df$Term %in% bad_terms)
  if( length(bad_rows) > 0) {
    df = df[-bad_rows,]
  }
  
  #remove singles
  if(singles==TRUE) {
    oneDsi = which(df_all$`NI` < 2)
    bad = df_all$Term[oneDsi]
    ibad = which(df$Term %in% bad)
    if( length(ibad) > 0) {
      df = df[-ibad,]  
    }
  }
  
  #remove low tf*idf terms
  if(!is.na(threshold)) {
    bad_rows = which(df_all$`tf*idf`<threshold)
    bad = df_all$Term[bad_rows]
    ibad = which(df$Term %in% bad)
    df = df[-ibad,]
  }
  
  return(df)
}

################ make trimmed RTV #############################
# Same as remove_terms, but also changes NA values to 0
#
# df_all is used as a guide to help remove terms from df
# df is term frequencies, either tf*idf or n*idf
# threshold: remove terms with a tf*idf < threshold
# singles: remove terms that appear in only 1 DSI
#
# returns: pruned data.frame
###############################################################
trim_df = function(df_all, df, threshold = NA, singles = FALSE, rm.na=TRUE) {
  
  df = remove_terms(df_all, df, threshold=threshold, singles=singles)
  
  #replace NA with 0?
  if( rm.na == TRUE) {
    z = apply(df[2:(num_dsi+1)], MARGIN=2, FUN=function(x){ifelse(is.na(x),0,x)})
    z = data.frame(df[1],z)
  } else {
    z = df[1:(num_dsi+1)]
  }
  
}

############################################################
# Transposes RTV into format suitable for clustering algos
#
# df is a data.frame that is assumed to have columns
# Term, DSI_1, DSI_2, DSI_3, ..., DSI_N
#
# returns: transposed data.frame (without the Term column)
############################################################
transpose_df = function(df){
  #remove Term column
  df_terms = df[-1]
  z = t(df_terms)
  return(z)
}


######### start here ######

# this directories contain my final, manually edited RTVs for each DSI
inbase="/RTV_csv/thinned_combined/"

# where the RTV will be created, all analysis is done here
outbase="/RTV_csv/thinned_combined_out/"

#combine the RTVs with ECs
df_all = combine_csv("/RTV_csv/thinned_combined/", "/RTV_csv/thinned_combined_out/" )

#input file to use for clustering
f = "RTV_csv/thinned_combined_out/rtv_tfidf.csv"
df_tfidf = read.csv(f,header = TRUE, stringsAsFactors = FALSE)
zz = trim_df(df_all,df_tfidf)

#transpose for clustering (terms are columns)
z = transpose_df(zz)

#replace dsi numbers with titles for plotting
z = replace_dsi_numbers(z)

# Compute hierarchical clustering and cut into 3 clusters
res <- hcut(z, k = 3, stand = TRUE)

jpeg(file = paste(outdir,"fig_dend.jpg",sep=""), width = 960, height = 960)
fviz_dend(res, rect = TRUE, cex = 1.8, horiz=TRUE, labels_track_height = 30,
          k_colors = c("red","blue", "darkgreen", "purple"))
dev.off()

jpeg(file = paste(outdir,"fig_phylo.jpg",sep=""), width = 960, height = 960)
fviz_dend(res, rect = TRUE, cex = 1.8, labels_track_height = 32, type="phylogenic", repel=TRUE,
          k_colors = c("red","blue", "darkgreen", "purple"))
dev.off()

# Compute hierarchical clustering and cut into 4 clusters
res <- hcut(z, k = 4, stand = TRUE)

jpeg(file = paste(outdir,"fig_dend4.jpg",sep=""), width = 960, height = 960)
fviz_dend(res, rect = TRUE, cex = 1.8, horiz=TRUE, labels_track_height = 30,
          k_colors = c("red","blue", "darkgreen", "purple"))
dev.off()

jpeg(file = paste(outdir,"fig_phylo4.jpg",sep=""), width = 960, height = 960)
fviz_dend(res, rect = TRUE, cex = 1.8, labels_track_height = 32, type="phylogenic", repel=TRUE,
          k_colors = c("red","blue", "darkgreen", "purple"))
dev.off()

# Compute hierarchical clustering and cut into 5 clusters
res <- hcut(z, k = 5, stand = TRUE)

jpeg(file = paste(outdir,"fig_dend5.jpg",sep=""), width = 960, height = 960)
fviz_dend(res, rect = TRUE, cex = 1.8, horiz=TRUE, labels_track_height = 30,
          k_colors = c("red","blue", "darkgreen", "purple","orange"))
dev.off()

jpeg(file = paste(outdir,"fig_phylo5.jpg",sep=""), width = 960, height = 960)
fviz_dend(res, rect = TRUE, cex = 1.8, labels_track_height = 32, type="phylogenic", repel=TRUE,
          k_colors = c("red","blue", "darkgreen", "purple","orange"))
dev.off()

# PCA on RTV, should be same as kmeans plot?
colnames(z) = zz$Term
z[,1:10]
z.pca <- PCA(z[,-1], graph = TRUE)
z.pca$var

#this shows the %contribution of each component
z.pca$eig


# 2D cluster
jpeg(file = paste(outdir,"fig_clust2d.jpg",sep=""), width = 720, height = 720)
set.seed(123)
km.res <- kmeans(z, 3, nstart = 8)
fviz_cluster(km.res, data = z,
             #palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             palette = c("red","blue", "darkgreen", "purple"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot",
             labelsize=18,
             repel=TRUE
)
dev.off()


jpeg(file = paste(outdir,"fig_clust2d4.jpg",sep=""), width = 720, height = 720)
set.seed(123)
km.res <- kmeans(z, 4, nstart = 8)
fviz_cluster(km.res, data = z,
             #palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             palette = c("red","blue", "darkgreen", "purple"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot",
             labelsize=18,
             repel=TRUE
)
dev.off()

jpeg(file = paste(outdir,"fig_clust2d5.jpg",sep=""), width = 720, height = 720)
set.seed(123)
km.res <- kmeans(z, 5, nstart = 1)
fviz_cluster(km.res, data = z,
             #palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             palette = c("red","blue", "darkgreen", "purple","orange"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot",
             labelsize=18,
             repel=TRUE
)
dev.off()

##### create stacked bar chart of term frequencies per cluster #########

#these are the columns that will be used to create each bar
c1 = c(1:4)
c2 = c(5:8)
c2a = c(5,6)
c2b = c(7,8)
c3 = c(9:12)
c3a = c(9,10)
c4 = c(11,12)

clusters = list(c1,c2,c2a,c2b,c3,c3a,c4)

#need to go back to original number of occurrences, not frequencies
df = df_all
df = trim_df(df_all, df)
terms = df$Term

# calculate term frequences of each term, but across the clusters defined above
tf =
  lapply(clusters, function(x) {
    
    #get the columns in this cluster (defined by x)
    z = df[x+1]
    
    #total number of terms in this cluster
    total_sum = sum(z)  
    
    #sum the term occurrences across the cluster
    term_sums = apply(z, MARGIN=1, sum)
    
    #calculate term frequency
    tf = term_sums/total_sum
    return(tf)
  })

#add the Term column back in
x = data.frame(Terms=terms,tf)

#name of each cluster
colnames(x) = c("Term","Freq_Border Security","Freq_North Korea","Freq_NK/War","Freq_NK/Diplomacy","Freq_Russia","Freq_Rus/Election","Freq_Rus/Expulsions")

#create a 'long' format data.frame, required by ggplot for plotting bar charts.
#note that colnames all start with "Freq_". reshape() uses this to name values in the new data.frame
# varying: all of the columns that will be changed from 'wide' to 'long'
# direction: 'long' to convert from wide to long
# idvar: The column that identifies the 'group', which will have multiple rows in the long format
# timevar: new column name that identifies which element of the group.
#          The part of column name after the separater character (sep="_") goes here
# sep:  the separator character.
#
# Example: x has a column "Freq_Border Security". the long format will create rows with 
#  Cluster = "Border Security"
#  Freq = original "Freq_Border Security" value
xx = reshape(data = x, 
             varying = colnames(x)[-1],
             direction="long",
             idvar="Term",
             timevar="Cluster",
             sep="_"
)
rownames(xx)=NULL

#add columns for sorting by freq and getting top 10

#The bar plot created bars in an unexpected order, create this vector to match, use it in ggplot x axis label
cluster_names = c("Border Security","North Korea","NK/War","NK/Diplomacy","Russia","Rus/Expulsions", "Rus/Election")

# create long data frame that has only the top 12 terms for each cluster
# variable is called top10, because that was what I did originally, I decided to add more
df_long = data.frame()
for( cc in cluster_names) {
  
  #get only rows for 1 cluster
  xxx = xx[which(xx$Cluster==cc),]
  
  #sort them in descending order
  o = order(xxx$Freq,decreasing=TRUE)
  top10 = xxx[o,]
  top10 = top10[1:12,]
  
  #cum sum is used to place labels in the correct spot in barachart
  topcum = cumsum(rev(top10$Freq))
  
  #create data.frame
  #col is the color used to plot the bar segment. ggplot will sort the bar segments by color.
  #using this instead of the term name because ggplot sorts segments by color
  df_top = data.frame(top10, col = seq(nrow(top10)), label_ypos = topcum, stringsAsFactors = FALSE)
  
  #add it
  if(nrow(df_long) == 0) {
    df_long = df_top
  } else {
    df_long = rbind(df_long, df_top)
  }
}

#save ggplot
jpeg(file = paste(outdir,"fig_termfreq.jpg",sep=""), width = 960, height = 960)

ggplot(data=df_long, aes(x=rev(Cluster),y=Freq,fill=as.factor(col))) +
  geom_bar(stat="identity")+
  guides(fill=FALSE)+   #remove legend for bar
  theme(legend.position="none")+
  geom_text(aes(y=label_ypos, 
                label=rev(Term)), 
            vjust=1.6, 
            color="black", size=6.0)+
  scale_fill_brewer(palette="Paired")+
  ylab("Frequency")+
  
  #not sure why I have to reverse the labels, but bars are in the wrong order
  scale_x_discrete("Clusters",labels=cluster_names)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold")
        #panel.background = element_rect(colour="black",fill="white")
  )
#theme_minimal()

dev.off()


#examples for determining the best number of clusters
#http://www.sthda.com/english/wiki/print.php?id=239

##### get silhoutte for each individual DSI ####

library(cluster)
k.max <- 15

#z is the transposed version of df_tfidf, from above
data <- z
sil <- rep(0, k.max)

# Compute the average silhouette width for 
# k = 2 to k = 15
for(i in 2:k.max){
  km.res <- kmeans(data, centers = i, nstart = 6)
  ss <- silhouette(km.res$cluster, dist(data))
  
  hist(ss[,3])
  sil[i] <- mean(ss[, 3])
}

# Plot the  average silhouette width
plot(1:k.max, sil, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k")
abline(v = which.max(sil), lty = 2)
##############################################


#elbow method for kmeans
jpeg(file = paste(outdir,"fig_elbow.jpg",sep=""), width = 960, height = 960)
fviz_nbclust(z, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)
dev.off()

library(NbClust)
library(cluster)
#elbow method for PAM
jpeg(file = paste(outdir,"fig_elbow_kmeans.jpg",sep=""), width = 960, height = 960)
fviz_nbclust(z, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)
dev.off()

jpeg(file = paste(outdir,"fig_elbow_pam.jpg",sep=""), width = 960, height = 960)
fviz_nbclust(z, pam, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)
dev.off()

jpeg(file = paste(outdir,"fig_elbow_hclust.jpg",sep=""), width = 960, height = 960)
fviz_nbclust(z, hcut, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)
dev.off()


#Average silhouette method for k-means clustering
jpeg(file = paste(outdir,"fig_silhouette_kmeans.jpg",sep=""), width = 960, height = 960)
fviz_nbclust(z, kmeans, method = "silhouette")
dev.off()

#Average silhouette method for PAM clustering
jpeg(file = paste(outdir,"fig_silhouette_pam.jpg",sep=""), width = 960, height = 960)
fviz_nbclust(z, pam, method = "silhouette")
dev.off()

#Average silhouette method for hierarchical clustering
jpeg(file = paste(outdir,"fig_silhouette_hclust.jpg",sep=""), width = 960, height = 960)
fviz_nbclust(z, hcut, method = "silhouette",
             hc_method = "complete")
dev.off()

