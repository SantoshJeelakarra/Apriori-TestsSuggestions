source("~/Documents/DataCleansing/Functions.R")
invokeLibraries()

#Step1: Finding the support value for individual tests

###### Note: this data is from prod db 
tests_billID<-read.csv("~/Documents/AssociationRules/prodTrx_wrt_billID.csv",header = FALSE,sep=",",quote = '"',dec=".")
tests_billID<-set_names(tests_billID, c("billId_id","name_dictionary","dictionaryId_id"))

#remove unnecessary columns
tests_billID$dictionaryId_id<-NULL

#selecting only the distinct elements
tests_billID<-tests_billID%>% distinct(billId_id,name_dictionary)

#-------------------------------------------------------
# Step2: Consolidate transactions based on the billId_id

#detaching the dplyr package before using the plyr package to avoid error.
detach(package:dplyr)
library(plyr)

tests_billID$billId_id<-as.factor(tests_billID$billId_id)

#creating a new 'timevar' column for applying reshape function
tests_billID$uniqid<-with(tests_billID,ave(as.character(billId_id),billId_id,FUN = seq_along))

#reshaping long to wide format
wide_tests_billID<-reshape(tests_billID,idvar = "billId_id",timevar = "uniqid", direction="wide")
wide_tests_billID$billId_id<-NULL
setwd("~/Documents/AssociationRules")
write.csv(wide_tests_billID,file="wide_tests_billID_24Oct_prod.csv")

#-------------------------------------------------------
# Step3: Building Apriori algorithm and Finding association rules

#install.packages("arules")
library(arules)

trans = read.transactions(file="wide_tests_billID_24Oct_prod.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1);
class(trans)
summary(trans)
inspect(head(trans,3))

#remove quotes from the transactions
trans@itemInfo$labels <- gsub("\"","",trans@itemInfo$labels)

#finding frequent items set for a given support value
frequentItems <- eclat(trans, parameter = list(supp = 0.01, maxlen = 3)) # supp = 0.01 Atleast 1% of the time the item is present in a transactions;
                                                                         # means, atleast 26857 times an item is occuring in the transactions.  
inspect(frequentItems)
itemFrequencyPlot(trans,topN=10,type="absolute",main="Frequency")

#applying apriori algorithm and finding the association rules
rules<- apriori(trans,parameter = list(support = 0.02, conf = 0.5,maxlen=5,minlen=5,target="rules"))
rules_sort<-sort(rules,by="lift",decreasing=TRUE)
inspect(head(rules_sort,100))


rules_select <- apriori (trans, parameter=list(supp=0.05,conf = 0.5,maxlen=2,target="rules"), appearance = list(default="lhs",rhs="Complete Blood Count"), control = list (verbose=F))
rules_select_sort<-sort(rules_select,by="lift",decreasing=TRUE)
inspect(head(rules_select_sort,90))

summary(trans)
association_rules_allTests_df<-as(association_rules_allTests,"data.frame")

rules_quad<-as(rules_sort,"data.frame")

