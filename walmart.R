library(stringr)
library(dplyr)

train1 <- read.delim("train.txt",header = TRUE,sep = ",")
#8523
test <- read.delim("test.txt",header = TRUE,sep = ",")
#5681
test$Item_Outlet_Sales <- NA

train <- rbind(train1,test)
rm(train1,test)
colSums(is.na(train))
str(train)
summary(train)

##CLEANING##

##  Item_fat_content value corrections   ##
train$Item_Fat_Content <- str_replace(train$Item_Fat_Content,"LF|low fat","Low Fat")
train$Item_Fat_Content <- str_replace(train$Item_Fat_Content,"reg","Regular")

# Quantity Sold and Year 
train$Item_Quantity_Sold <- signif(train$Item_Outlet_Sales/train$Item_MRP , digits = 2)
train$Freshness <- (2013-train$Outlet_Establishment_Year)

# Outlet size has 4016 blank values
summary(train$Outlet_Identifier[train$Item_Visibility==0])

# there are only 10 unique outlet identifiers of which outlet 10,17 and 45 has ouletsize "missing"
# from the Quantity sold Outlets 10 is a grocery store.

x <- train[1:8523,] %>%
  group_by(Outlet_Identifier) %>%
  summarise(sum(Item_Outlet_Sales))

y <- train[1:8523,] %>%
  group_by(Outlet_Identifier) %>%
  summarise(mean(Item_Outlet_Sales))

#outlets <- unique(train[,c("Outlet_Identifier","Outlet_Type","Outlet_Location_Type","Outlet_Size","Freshness")])
outlets <- left_join(train[,c(1,7)],x,by="Outlet_Identifier")
outlets <- left_join(outlets,y,by="Outlet_Identifier")

train$total_outlet_sales <- outlets$`sum(Item_Outlet_Sales)`
train$mean_outlet_sales <- outlets$`mean(Item_Outlet_Sales)`


train$Outlet_Size[(train$Outlet_Size=="")] <- "Small"



table(train$Outlet_Type,train$Outlet_Size)

#coord_flip()
#facet_grid(.~interest_level)
str(train)

summary(train$Outlet_Identifier[is.na(train$New_Item_Weight)])

##   Item Weight needs to be addressed 1463 NA values   ##
# Outlets 19 and 27 are missing all the Item_Weight values

Missing_Items <- train[is.na(train$Item_Weight),1:2]
All_items <- distinct(train[!is.na(train$Item_Weight),1:2])
Missing_Items <- left_join(Missing_Items,All_items,by="Item_Identifier")

train$New_Item_Weights <- train$Item_Weight
train$New_Item_Weights[is.na(train$Item_Weight)] <- Missing_Items$Item_Weight.y

summary(train$Outlet_Identifier[is.na(train$New_Item_Weight)])

All_items <- train[,c("Item_Identifier","Item_MRP")]

#Whether price of each Item affects its sales
avg_price <- train %>%
  group_by(Item_Identifier) %>%
  summarise(median(Item_MRP))

avg_sales <- train[!is.na(train$Item_Outlet_Sales),] %>%
  group_by(Item_Identifier) %>%
  summarise(mean(Item_Outlet_Sales))

avg_quantity_sold <- train[!is.na(train$Item_Outlet_Sales),] %>%
  group_by(Item_Identifier) %>%
  summarise(mean(Item_Quantity_Sold))

avg_visibility <- train[!(train$Item_Visibility==0),] %>%
  group_by(Item_Identifier) %>%
  summarise(mean(Item_Visibility))

All_items <- left_join(All_items,avg_price,by="Item_Identifier")
All_items <- left_join(All_items,avg_sales,by="Item_Identifier")
All_items <- left_join(All_items,avg_quantity_sold,by="Item_Identifier")

All_items$Visibility <- train$Item_Visibility
All_items <- left_join(All_items,avg_visibility,by="Item_Identifier")

All_items$Visibility[All_items$Visibility==0] <- All_items$`mean(Item_Visibility)`[All_items$Visibility==0]
All_items$Costly <- as.factor(
  ifelse(train$Item_MRP < train$mednMRP, "cheap",
         ifelse(train$Item_MRP >train$mednMRP, "costly","avg"
                )
  )
)

train$Costly <- All_items$Costly
train$Avg_Sales <- All_items$`mean(Item_Outlet_Sales)`
train$mednMRP <- All_items$`median(Item_MRP)`
train$Item_Visibility  <- All_items$Visibility
train$Avg_Quantity_sold <- All_items$`sum(Item_Quantity_Sold)`
train$total_quantity_sold <- All_items$`mean(Item_Quantity_Sold)`

cor(train[,c("Item_Visibility","Item_Outlet_Sales")],use = "na.or.complete")

summary(train$Outlet_Identifier[train$Item_Visibility==0])
str(train)
#ITEM TYPE 2
train$Item_Type2 <- substr(train$Item_Identifier,start = 1,stop = 2)
train$Item_Type2 <- as.factor(train$Item_Type2)

train$Item_Type3 <- substr(train$Item_Identifier, start = 3,stop = 3)
train$Item_Type3 <- as.factor(train$Item_Type3)


# # Item MRP size ##
summary(train$Item_MRP)

ggplot2::ggplot(data = train[1:8523,],aes(Item_MRP,Avg_Sales,color=MRP_Type))+geom_point()

train$MRP_Type <- as.factor(
  ifelse(train$Item_MRP < 69, "Low",
         ifelse(train$Item_MRP < 136, "Medium",
                ifelse(train$Item_MRP < 203, "High", "Very_High")))
)

## Hard drinks obviusly doesn't have low fat, its actually no fat!
train$Fat_Content <- as.factor(
  train$Fate_Content[(train$Item_Type2=="NC"|train$Item_Type=="Hard Drinks")] <- "No Fat"
)

#processed_new_raw created


train <- train[,-c(2,8)]

write.csv(train,"processed_new_final.csv",row.names = FALSE)
          
character_variables <- c("Item_Type","Outlet_Identifier","Item_Identifier",
                         "Outlet_Size","Outlet_Type","Item_Type2","Item_Type3","Outlet_Location_Type")

train[,character_variables]<- train[,character_variables] %>%
  lapply(., as.numeric)

train$Item_Quantity_Sold <- NULL
colSums(is.na(train))
train1 <- train[1:8523,]

test <- train[8524:14204,]

y <- train1$Item_Outlet_Sales

library(xgboost)
set.seed(123)
xgb <- xgboost(data = data.matrix(train1[,-10]),
               missing = NA,
               eta= 0.1,
               label = train1$Item_Outlet_Sales, 
               max_depth =5, 
               nround=30, 
               seed = 1,
               subsample=0.8,
               objective = "reg:linear",
               nthread = -1
)
importance_matrix <- xgb.importance(model = xgb)
xgb.plot.importance(importance_matrix = importance_matrix)

test <- train[8524:14204,]


pred_test <- predict(xgb,data.matrix(test[,-10]))

test <- read.delim("test.txt",header = TRUE,sep = ",")

output <- test[,c("Item_Identifier","Outlet_Identifier")]
output$Item_Outlet_Sales <- pred_test

write.csv(output,"output27-5308.csv",row.names = FALSE)
