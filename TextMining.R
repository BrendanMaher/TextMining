libs=c("readr","dplyr","tidytext","tm","SnowballC","wordcloud","cluster","tidyr","widyr","igraph","ggraph");
rbind(t(lapply(libs,require,character.only=TRUE)),libs);                              #libraries used
References=read_csv("***/CSV_All_Texts.csv");                                         #load data, so change file directory accordingly
ref=data_frame(id=References$id,abs=References$Abstract);                             #only the abstracts are being studied
refs=unnest_tokens(ref,abs,abs);                                                      #for single terms
refs$abs=sub("[^\x20-\x7E]","",refs$abs);                                             #remove Unicode
refs=anti_join(refs,data_frame(abs=stopwords("en")));                                 #remove common English
refs=filter(refs,!grepl("\\d",refs$abs));                                             #remove numbers
refs=filter(refs,nchar(refs$abs)>2);                                                  #remove words less then 3 letters
refs$abs=wordStem(refs$abs);                                                          #remove prefixes and suffixes
with(count(refs,abs),wordcloud(abs,n,max.words=50,colors=c(2,3,4,5,6,9)));            #word cloud of terms over all documents
refc=count(refs,id,abs,sort=TRUE);                                                    #term counts per documents
refc=arrange(bind_tf_idf(refc,abs,id,n),desc(tf_idf));View(filter(refc,tf_idf>0.5));  #calculate tf, idf, tf*idf
plot(refc$tf_idf,ylab="tf*idf",main="Term Frequency x Inverse Document Frequency");   #plot tf*idf and view tf*idf over 0.5
plot(refc$tf,refc$idf,main="terms vs inverse of documents");                          #tf vs idf plot
refr=pairwise_cor(filter(refc,tf_idf>0.5),abs,id,n,sort=TRUE);                        #term correlations from values over 0.5
refr=graph_from_data_frame(filter(refr,correlation>0.15));                            #graph correlations within docs over 0.15 
ggraph(refr,layout="fr")+geom_edge_link(aes(edge_alpha=correlation),show.legend=FALSE)+geom_node_point(color="lightblue",size=5)+geom_node_text(aes(label=name),repel=TRUE)+theme_void();
refm=removeSparseTerms(cast_dtm(refc,id,abs,tf_idf),0.7);                             #make document term matrix of values tf*idf
reft=dist(t(refm));refth=hclust(reft,method="ward.D");reftk=kmeans(reft,3);           #cluster by term differences
plot(refth,hang=-1,xlab="terms");rect.hclust(refth,k=7,border="red");                 #plot bottom up clusters of terms
clusplot(as.matrix(reft),reftk$cluster,lables=2,main="clusplot terms");               #plot top down clusters of terms
refd=dist(refm);refdh=hclust(refd,method="ward.D");refdk=kmeans(refd,3);              #cluster by document differences
plot(refdh,hang=-1,xlab="docs");rect.hclust(refdh,k=7,border="red");                  #plot bottom up clusters of documents
clusplot(as.matrix(refd),refdk$cluster,lables=2,main="clusplot docs");                #plot top down clusters of documents
refo=top_n(group_by(tidy(refm),document),1,count);                                    #get the top dtm terms of each doc cluster
refo=inner_join(refo,data_frame(document=refo$document,journal=References$Journal[as.numeric(refo$document)]));
View(top_n(group_by(inner_join(refo,data_frame(clus=refdk$cluster,document=names(refdk$cluster))),clus),1,count));
View(cbind(References[1:10,2:4],refdk$cluster)[1:10,]);                               #A sample of document cluster designations
refp=unnest_tokens(ref,abs,abs,"ngrams",n=2);                                         #for term pairs
refp=separate(refp,abs,c("a1","a2"),sep=" ");                                         #split for removals
refp$a1=sub("[^\x20-\x7E]","",refp$a1);                                               #remove Unicode of leading terms
refp$a2=sub("[^\x20-\x7E]","",refp$a2);                                               #remove Unicode of following terms
refp=anti_join(refp,data_frame(a1=stopwords("en")));                                  #remove common English of leading
refp=anti_join(refp,data_frame(a2=stopwords("en")));                                  #remove common English of following
refp=filter(refp,!grepl("\\d",refp$a1)&!grepl("\\d",refp$a2));                        #remove numbers
refp=filter(refp,(nchar(refp$a1)>2)&(nchar(refp$a2)>2));                              #remove words less then 3 letters
refp$a1=wordStem(refp$a1);refp$a2=wordStem(refp$a2);                                  #remove prefixes and suffixes
refg=graph_from_data_frame(filter(count(refp,a1,a2),n>10));                           #graph term relations
ggraph(refg,layout="fr")+geom_edge_link()+geom_node_point()+geom_node_text(aes(label=name),vjust=1,hjust=1)
