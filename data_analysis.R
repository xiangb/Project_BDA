dataset_p <- read.csv('data/processed_data.csv',sep=',',header = T)

### Corrplot of numerical variables
corrplot(cor(dataset_p[, sapply(dataset_p, is.numeric)]))



### Distribution of target among categorical variables
plot_dist <- function(column,inch,cexnames=1.3){
  options(digits = 2)
  co = 100*prop.table(table(dataset_p[,'target'],dataset_p[,column]),2)
  plt <- barplot(co, 
       col= c("yellowgreen","skyblue"), las=1, main = column, yaxt= 'n',font=2,
       cex.lab=2, cex.axis=2, cex.main=1.5, cex.sub=1.5, cex.names = cexnames)
  co2 = format(round(cbind(co[1,], co[2,]),2))
  text(x= plt+inch, y= c(as.integer(co2[,1])-5,as.integer(co2[,1])+2), labels=co2, pos = 2,
       font=2, cex=1)
}

plot_dist('race', 0.1)
legend('bottomright', c("<50k", ">50k"), fill=c("yellowgreen","skyblue"),
        cex = 1.3)
plot_dist('workclass',0.2,1.2)
plot_dist('sex',0)
plot_dist('relationship',0)
plot_dist('mstatus',0.3,0.7)
plot_dist('nativecountry',0.5,1.2)
plot_dist('education',0.5,0.6)

#TEST OF INDEPENDANCE
chisq.test(table(dataset_p$target, dataset_p$race))
chisq.test(table(dataset_p$target, dataset_p$workclass))
chisq.test(table(dataset_p$target, dataset_p$sex))
chisq.test(table(dataset_p$target, dataset_p$occupation))
chisq.test(table(dataset_p$target, dataset_p$nativecountry))
chisq.test(table(dataset_p$target, dataset_p$relationship))
chisq.test(table(dataset_p$target, dataset_p$mstatus))
chisq.test(table(dataset_p$target, dataset_p$education))
