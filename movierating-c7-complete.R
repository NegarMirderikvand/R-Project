#This code gives the correlation and Euclidean distance between the ratings of User 817 and those of other users.
rm(list=ls(all.names=T))
ratings=read.csv('')
meta=read.csv('')
meta= meta[c('id','vote_average','title','popularity','vote_count')]
ratings= ratings[c('userId','rating','movieId')]
colnames(meta)[1]="movieId"
merged=merge.data.frame(ratings,meta,"movieId")
head(merged)
u817<-merged[merged['userId']=="817",]
u817m<-merged[merged['userId']=="817",'movieId']

#u7199<-merged[merged['userId']=="7199",]
#u7199m<-merged[merged['userId']=="7199",'movieId']
#common<-Reduce(intersect, list(u817m, u7199m))
#findcom1<-function(x){avg<-u7199[u7199['movieId']==x,'rating']
#return(avg)}
#findcom2<-function(x){avg<-u817[u817['movieId']==x,'rating']
#return(avg)}
#ra7199<-apply(as.matrix(common),1,findcom1)
#ra817<-apply(as.matrix(common),1,findcom2)
#cor(ra7199, ra817)
#norm(as.matrix(ra7199-ra817),"f")/length(common)


finduser=function(x){
  u<-NA
  y<-NA
  z<-NA
  rau<-NA
  ra817<-NA
  Z<-NA
  corr<-NA
  Euclid<-NA
  u<-merged[merged['userId']==x,]   
  y<-merged[merged['userId']==x,'movieId'] 
  z<-Reduce(intersect, list(u817m,y)) 
  findcom1<-function(m){avg<-u[u['movieId']==m,'rating']
  return(avg)}
  findcom2<-function(m){avg<-u817[u817['movieId']==m,'rating']
  return(avg)}  
  rau<-apply(as.matrix(z),1,findcom1)
  ra817<-apply(as.matrix(z),1,findcom2)
  if (length(rau)>=2 & length(ra817)>=2){
    if(sd(rau)==0 || sd(ra817)==0){corr=NaN}else{corr=cor(rau, ra817)}}else{corr=NaN}
  if (length(z)<10){corr=0;Euclid=Inf}else{Euclid=norm(as.matrix(rau-ra817),"f")/length(z)}
  Z=list(corr,Euclid,x,rau,ra817,z)
  names(Z)=c("Correlation","Distance","User","Ratings","User 817's Ratings","Common")
  return(Z)
}

Numusers=1000
Allids=unique(merged$userId)
Usableids=Allids[Allids!=817]
info=apply(as.matrix(Usableids[1:Numusers]),1,finduser)
names(info)=c(sprintf("User% .1d",Usableids[1:Numusers]))
extractcor=function(x){return(info[x][[1]]$Correlation)}
extractdist=function(x){return(info[x][[1]]$Distance)}
corr=apply(as.matrix(1:Numusers),1,extractcor)
dist=apply(as.matrix(1:Numusers),1,extractdist)
F1=data.frame("User ID"=Usableids[order(corr,decreasing=T)],"Correlation"=corr[order(corr,decreasing =T)],"Distance"=dist[order(corr,decreasing =T)])
F2=data.frame("User ID"=Usableids[order(dist)],"Correlation"=corr[order(dist)],"Distance"=dist[order(dist)])



#plot(F1[F1[,3]!=Inf,3],F1[F1[,3]!=Inf,2])
#hist(F1[F1[,3]!=Inf,3])
#sum(F1[F1[,3]!=Inf,3]<0.268)/Numusers


