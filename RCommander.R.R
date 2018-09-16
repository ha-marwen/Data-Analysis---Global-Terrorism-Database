
DataAttacks <-read.table("C:/Users/pc hp/Desktop/globalterrorism/Number of attacks - Copie.txt",header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE,row.names=1)
DataAttacks <- local({
  .Z <- scale(DataAttacks[,c("X2001","X2002","X2003","X2004","X2005",
  "X2006","X2007","X2008","X2009","X2010","X2011","X2012","X2013","X2014",
  "X2015","X2016")])
  within(DataAttacks, {
    Z.X2016 <- .Z[,16]
    Z.X2015 <- .Z[,15]
    Z.X2014 <- .Z[,14]
    Z.X2013 <- .Z[,13]
    Z.X2012 <- .Z[,12]
    Z.X2011 <- .Z[,11]
    Z.X2010 <- .Z[,10]
    Z.X2009 <- .Z[,9]
    Z.X2008 <- .Z[,8]
    Z.X2007 <- .Z[,7]
    Z.X2006 <- .Z[,6]
    Z.X2005 <- .Z[,5]
    Z.X2004 <- .Z[,4]
    Z.X2003 <- .Z[,3]
    Z.X2002 <- .Z[,2]
    Z.X2001 <- .Z[,1] 
  })
})
DataAttacks <- within(DataAttacks, {
  Z.X2001 <- NULL
  Z.X2002 <- NULL
  Z.X2003 <- NULL
  Z.X2004 <- NULL
  Z.X2005 <- NULL
  Z.X2006 <- NULL
  Z.X2007 <- NULL
  Z.X2008 <- NULL
  Z.X2009 <- NULL
  Z.X2010 <- NULL
  Z.X2011 <- NULL
  Z.X2012 <- NULL
  Z.X2013 <- NULL
  Z.X2014 <- NULL
  Z.X2015 <- NULL
  Z.X2016 <- NULL 
})
fviz_pca_ind(PCA(DataAttacks), col.ind = "cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
fviz_pca_var(PCA(DataAttacks), col.var = "cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
round(PCA(DataAttacks)$ind$contrib[,1:2],2)          
res.pca<-PCA(DataAttacks)
round(res.pca$eig,2)
round(res.pca$var$contrib[,1:2],2)

