
library(caret)


train <- read.csv("processed_new_final.csv",header = TRUE)

table(train$Item_Type3)
combi <- train

combi$Costly <- as.factor(
  ifelse(train$Item_MRP < train$mednMRP, "cheap",
         ifelse(train$Item_MRP >train$mednMRP, "costly","avg"
         )
  )
)


train$MRP_Type <- as.factor(
  ifelse(train$Item_MRP < 69, "Low",
         ifelse(train$Item_MRP < 136, "Medium",
                ifelse(train$Item_MRP < 203, "High", "Very_High")))
)

table(combi$Item_Fat_Content)
combi$Item_Fat_Content <- as.character(combi$Item_Fat_Content)
combi$Item_Fat_Content[combi$Item_Type2=="NC"|combi$Item_Type=="Hard Drinks"] <- "No Fat"
combi$Item_Fat_Content <- as.factor(combi$Item_Fat_Content)

dummies <- dummyVars(~Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+
                       Outlet_Identifier+Outlet_Size+Outlet_Type+
                       Outlet_Location_Type+Freshness+total_outlet_sales+mean_outlet_sales+
                       New_Item_Weights+Costly+ Avg_Sales+mednMRP+total_quantity_sold+
                       Item_Type2+Item_Type3+MRP_Type,
                     data = combi )
combi_new <- as.data.frame(predict(dummies, newdata = combi))

combi_new$Item_Quantity_Sold <- NULL
combi_new$Item_Identifier <- as.numeric(combi$Item_Identifier)


train1 <- combi_new[1:8523,]

test <- combi_new[8524:14204,]

y <- combi$Item_Outlet_Sales[1:8523]

library(xgboost)
set.seed(123)
xgb <- xgboost(data = data.matrix(train1),
               missing = NA,
               eta= 0.1,
               label = y, 
               max_depth =4, 
               nround=15, 
               seed = 1,
               subsample=0.7,
               objective = "reg:linear",
               nthread = -1
)
importance_matrix <- xgb.importance(model = xgb)
xgb.plot.importance(importance_matrix = importance_matrix)
coord_flip()
test <- combi_new[8524:14204,]

test$Item_
pred_test <- predict(xgb,data.matrix(test))

test <- read.delim("test.txt",header = TRUE,sep = ",")

output <- test[,c("Item_Identifier","Outlet_Identifier")]
output$Item_Outlet_Sales <- pred_test

write.csv(output,"output38-xgb01-4157.csv",row.names = FALSE)

library(ggplot2)
ggplot(train, aes(Costly, Item_Outlet_Sales, fill = Item_Type2)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  #xlab("Item Type") + 
  #ylab("Item Visibility") + 
  ggtitle("Item visibility vs Item Type")

library(party)
library(randomForest)
fit <- randomForest(Item_Outlet_Sales~Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+
                 Outlet_Identifier+Outlet_Size+Outlet_Type+
                 Outlet_Location_Type+Freshness+total_outlet_sales+mean_outlet_sales+
                 New_Item_Weights+Costly+ Avg_Sales+mednMRP+total_quantity_sold+
                 Item_Type2+Item_Type3+MRP_Type,
                data=train[1:8523,], importance=TRUE, ntree=2000)
library(rpart)

combi$Item_Quantity_Sold <- NULL
fit <- rpart(combi$Item_Outlet_Sales[1:8523]~.,data = combi[1:8523,],method = "anova")
pred <- predict(fit,combi[8524:14204,-10])

output$Item_Outlet_Sales <- pred
write.csv(output,"rpart-baseline3.csv",row.names = FALSE)
