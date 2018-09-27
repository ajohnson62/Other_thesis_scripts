##### MI #####

#Setup
library(psych)
library(mice)
library(VIM)
library(ggplot2)
library(miceadds)
library(abind)

#Set working directory
setwd("U:/Documents/ACE_Data/Thesis_Analysis/MI/")

#Import the data from paste
data <- read.csv('data.csv', header = TRUE)
domains <- t(read.clipboard.tab(header=FALSE)) #These are the environment and cognitive domains for the FA

#Take a look at the data
#Get a table with the proportion of missing data in each variable
propmiss <- function(dataframe) {
  m <- sapply(dataframe, function(x) {
    data.frame(
      nmiss=sum(is.na(x)), 
      n=length(x), 
      propmiss=sum(is.na(x))/length(x)
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$variable <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  d[,4] <- round(d[,4],3)
  #return(d[order(d$propmiss), ]) #Toggle comment to get the variables ordered by proportion missing
  return(d)
}
prop_missing <- propmiss(data)
prop_missing$domain <- domains
write.csv(prop_missing, file='prop_missing.csv')

#Plot
prop_missing$variable <- factor(prop_missing$variable, levels = rev(prop_missing$variable)) #maintains order
prop_missing$domain <- factor(prop_missing$domain, levels = rev(prop_missing$domain)) #maintains order

ggplot(prop_missing[3:nrow(prop_missing),], aes(variable, propmiss, fill=domain)) + 
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  xlab("Proportion of missing data") 
  theme_bw(base_size=15)
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank())
ggsave('prop_missing_not_ordered.png', width = 30, height = 60, units = "cm")
  

#Drop variables with >15% missing- note, this is arbitrary and based on the distribution as we don't loose much by this cut off
data$bmi.age <- NULL
data$Rulesaboutplayingongamesconsoles.Timeortypeofgame <- NULL

#Check percentage of missing data for participants
miss_part <- data.frame(cbind(data$ID, round(rowSums(is.na(data))/ncol(data),3)))
colnames(miss_part) <- c('ID', 'propmiss')
miss_part <- miss_part[order(miss_part$propmiss), ]
ggplot(miss_part,aes(propmiss)) + 
  geom_bar() + #make the bars
  xlab("Proportion of missing data") 
theme_bw(base_size=15)
theme(axis.text.y = element_blank(), 
      axis.title.y = element_blank())
ggsave('prop_missing_in_participants.png', width = 10, height = 10, units = "cm")

#Drop participants with >15% missing data- note, this is arbitrary and based on the distribution as we don't lookse much by this cut off
set_NA <- which(rowSums(is.na(data))/ncol(data)>0.15)
data <- data[rowSums(is.na(data))/ncol(data)<0.15, ]

#Check which variables have over 90% in one response and remove (poorly formed) Keep in for imputation, may drop for FA
#Note, I actually didn't do this here but after the imputation
j <- 1
freq_table <- data.frame(question = character(), Most_frequent=integer(), stringsAsFactors = FALSE)
for(i in names(data)){
  freq_table[j, 1] <- i
  freq_table[j, 2] <- head(sort(as.numeric(prop.table(table(data[i]))), decreasing=TRUE),1)
  if(freq_table[j,2] > 0.9 ){
    print(freq_table[j,1])
    data[i] <- NULL
  }
  j <- j+1
}
write.csv(freq_table, file= "freq_variables.csv")

#Note, saved this new dataset for ease... load 'All_data_for_MI_removed'
data <- read.clipboard.tab()
domains <- t(read.clipboard.tab(header=FALSE))

#Set the variable types (coded as 1 for binary or non-ordered, 2 for ordinal, 3 for continous)
#This is important for the multiple imputation method
types <- read.clipboard.tab(header=FALSE) #This needs to be row vector list of column types
colnames(types) <- colnames(data)
data_temp <- NULL
data_temp <- rbind(types, data)
for(i in 3:ncol(data_temp)){
  if(data_temp[1,i]==1){
    data_temp[,i] <- as.factor(data_temp[,i])
  }
  if(data_temp[1,i]==2){
    data_temp[,i] <- as.ordered(data_temp[,i])
  }
  if(data_temp[1,i]==3){
    data_temp[,i] <- as.numeric(data_temp[,i])
  }
}
data <- data_temp[2:nrow(data_temp),]
data <- droplevels(data)

#Impute the data using MICE with the random forest method. Note, several other methods availabale for different data types
m=5
data_imp <- mice(data[,3:ncol(data)],m=m,meth='rf', maxit=50,seed=500)
write.mice.imputation(data_imp, name='MI_data', include.varnames = TRUE, long = TRUE, dattype = 'csv')


#Get the imputed datasets and scale
#First set to numeric to allow scale to work
var_names <- t(read.clipboard.tab(header=FALSE)) #Correct names as I changed them
var_names <- var_names[3:nrow(var_names),]
mydata <- NULL
for(i in 1:m){
  mydata[[i]] <- complete(data_imp, i)
  mydata[[i]] <- data.frame(lapply(mydata[[i]], as.numeric))
  #mydata[[i]] <- scale(mydata[[i]],center=TRUE, scale=TRUE)
  colnames(mydata[[i]]) <- var_names
}

#Remove variables with >90% in one answer
#Note check that the same variables are dropped based on 90% cut off- they are the same in each imputation
for(imp in 1:m){
  j <- 1
  data_temp <- NULL
  freq_table <- data.frame(question = character(), Most_frequent=integer(), stringsAsFactors = FALSE)
  data_temp <- data.frame(mydata[[imp]])
  for(i in colnames(mydata[[imp]])){
    freq_table[j, 1] <- i
    freq_table[j, 2] <- head(sort(as.numeric(prop.table(table(data.frame(mydata[[imp]])[i]))), decreasing=TRUE),1)
    if(freq_table[j,2] > 0.9 ){
      print(freq_table[j,1])
      data_temp[i] <- NULL
    }
    j <- j+1
  }
  mydata[[imp]] <- data_temp
}
j <- 1
freq_table <- data.frame(question = character(), Most_frequent=integer(), stringsAsFactors = FALSE)
for(i in names(data)){
  freq_table[j, 1] <- i
  freq_table[j, 2] <- head(sort(as.numeric(prop.table(table(data[i]))), decreasing=TRUE),1)
  if(freq_table[j,2] > 0.9 ){
    print(freq_table[j,1])
    data[i] <- NULL
  }
  j <- j+1
}
write.csv(freq_table, file= "freq_variables.csv")

#Check which have very extreme outliers
j <- 1
skew_table <- data.frame(question = character(), max=integer(), mean_100=integer(), stringsAsFactors = FALSE)
for(i in 1:ncol(data)){
  A <- (data[,i]-median(data[,i],na.rm=TRUE))^2
  skew_table[j, 1] <- colnames(data)[i]
  skew_table[j, 2] <- max(A, na.rm=TRUE) 
  skew_table[j, 3] <- 100*mean(A, na.rm=TRUE)
  if(max(A, na.rm=TRUE) > 100*mean(A, na.rm=TRUE)){
    print(colnames(data)[i])
  }
  j <- j+1
}
write.csv(skew_table, file= "skew_variables.csv")

#Save dataset
saveRDS(mydata, file='mydata.RData') #Save dataset
mydata <- readRDS(mydata, file='U:/Documents/ACE_Data/Thesis_Analysis/MI/mydata.RData') #To read back in

#Put back into mids object form for using MICE packages
mydata_long <- abind(mydata, along=1)
mydata_long <- rbind(scale(data[,3:ncol(data)], center=TRUE, scale=TRUE), mydata_long) #add non-imputed set
stacked <- complete(data_imp, "long", include=TRUE) #to get .id and .imp coulmns
stacked <- cbind(stacked[,1:2], mydata_long)
data_imp_scaled <- as.mids(stacked, .imp=1)
saveRDS(data_imp_scaled, file='data_imp_scaled.RData') #Save dataset
example <- readRDS(data_imp_scaled, file='U:/Documents/ACE_Data/Thesis_Analysis/MI/data_imp_scaled.RData') #To read back in





