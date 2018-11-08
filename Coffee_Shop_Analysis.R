#Read files: Customer_Level Data, Tranaction Level Data, Item Level Data

Customer<- read.csv('C:/Users/HARI/Desktop/Neelima/Analytical Vidhya/Coffee Shop/Customer_level_data.csv', na.strings=c("","NA"))
Transaction <- read.csv('C:/Users/HARI/Desktop/Neelima/Analytical Vidhya/Coffee Shop/Transaction_level_data.csv', na.strings=c("","NA"))
Itemlevel <- read.csv('C:/Users/HARI/Desktop/Neelima/Analytical Vidhya/Coffee Shop/Item_level_data.csv', na.strings=c("","NA"))

# Business Problem : How has the cafe been performing so far? 
#(decide what metrics would you build to track app performance)


#(b) Provide suggestions to improve cafe sales, based on your analysis of the data.

# Preliminary Data Analysis on Customer Data:
head(Customer)
str(Customer)
summary(Customer)

dups1 <- duplicated(Customer)
head(dups1 == 'TRUE')


# Abservations 210 People not entered Gender and Date _of _birth values: Impute the values 
# Only 2 persons are confident about their marital status == drop this field

Customer1<- Customer[-4]
head(Customer1)

# Preliminary Data Analysis on Transaction Data:
head(Transaction)
str(Transaction)
summary(Transaction)

#Check any duplicate records are there
nrow(Customer1) - nrow(unique(Customer1))
nrow(Transaction) - nrow(unique(Transaction))
nrow(Itemlevel) - nrow(unique(Itemlevel))
nrow(unique(Transaction$store_name))

# Display duplicate records 
Itemlevel[duplicated(Itemlevel) | duplicated(Itemlevel, fromLast = TRUE), ]

#Do the Univarient analysis on customer data 
library(ggplot2)
qplot(x=Transaction$store_name,data= Transaction)
qplot(x=Customer1$gender,data= Customer1)
barplot(Customer$gender)
?qplot

library(plyr)
Store_Analysis <-ddply(Transaction,~store_name,summarise,no_transactions=length(unique(transaction_number)),trans_amount=sum(sale_amount_including_loyalty_points_used),
                       Loyalty_amount=sum(loyalty_points_used_by_customer),No_Customers = length(unique(customer_number)) )


itemlevel_Analysis <-ddply(it,~store_name,summarise,Loyalty_amount=sum(loyalty_points_used_by_customer))

Store_Analysis$After_Loyalty <- Store_Analysis$trans_amount - Store_Analysis$Loyalty_amount

head(Store_Analysis)

nrow(ddply(Transaction,~store_name,summarise))
nrow(Transaction)
?ddply
nrow(ddply(Transaction,~store_name,summarise,transaction_number=length(unique(transaction_number))))

library(dplyr)
top5_stories_NoTrans <- Store_Analysis %>%  arrange(-no_transactions,-trans_amount) %>% head(5)
Bottom_stories_NoTrans <- Store_Analysis %>%  arrange(-no_transactions,-trans_amount) %>% tail(5)

top5_trans_Amount <- Store_Analysis %>%  arrange(-trans_amount,-After_Loyalty) %>% head(5)
Bottom5_trans_amount <- Store_Analysis %>%  arrange(-trans_amount,-After_Loyalty) %>% tail(5)

qplot(x=top5_stories_NoTrans$store_name,y=top5_stories_NoTrans$no_transactions
      ,data= top5_stories_NoTrans)




#Sort and get the top Customers 
summary(Store_Analysis)
tail(sort(Store_Analysis$no_transactions),10,index.)
tail(sort(Store_Analysis$trans_amount))
tail(sort(Store_Analysis$After_Loyalty))
?tail


