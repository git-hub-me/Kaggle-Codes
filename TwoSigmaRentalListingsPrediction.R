library(jsonlite)
library(ggplot2)
library(stringr)
library(tm)
library(dplyr)
library(purrr)
library(RecordLinkage)

training <- fromJSON("train.json")
test <- fromJSON("test.json")
test$interest_level <- NA

vector.variables <- setdiff(names(test), c("photos", "features"))
test <-map_at(test, vector.variables, unlist) %>% tibble::as_tibble(.)

vector.variables <- setdiff(names(training), c("photos", "features"))
train <-map_at(training, vector.variables, unlist) %>% tibble::as_tibble(.)

train <- rbind(train, test)

remove(test)
# Add removed variables
train$features -> features
train$photos -> photos

train$features <- NULL
train$photos <- NULL

numerical_variables <- c("bathrooms", "bedrooms","longitude", "latitude", "price")

train[, numerical_variables]<- train[, numerical_variables] %>%
  lapply(., as.numeric)


train['interest_level'] <- factor(train[['interest_level']], levels = c("low", "medium", "high"))


#FEATURE 1 : Display_address -Display

train$display <- NULL
train$display <- train$display_address
train$display[which(train$display=="")] <- train$street_address[which(train$display=="")]

#correcting the direction"s abbreviations
train$display <- str_replace(train$display,"[N+\\s] | [n] ", " north ")
train$display <- str_replace(train$display,"[W+\\s] | [n] ", " west ")
train$display <- str_replace(train$display,"[S+\\s] | [n] ", " south ")
train$display <- str_replace(train$display,"[E+\\s] | [n] ", " east ")

train$display <- tolower(train$display) #all in lower form

train$display<- str_replace(train$display,"\\s+(([(s+t)]+\\.)|([(s+t)]+\\b))" , " street ")
train$display<- str_replace(train$display,"\\s+(([(p+l)]+\\.)|([(p+l)]+\\b))" , " place ")
train$display<- str_replace(train$display,"\\s+(([a+v+e+]+\\.)|([a+v+e+]+\\b))" , " avenue ")
train$display<- str_replace(train$display,"\\s+(([(r+d)]+\\.)|([(r+d)]+\\b))" , " road ")

train$display <- stripWhitespace(train$display) #extra whitespace

train$display <- str_trim(train$display, side = "both") # trimming leading and trailing spaces

str(as.factor(train$display_address)) #8826
str(as.factor(train$display)) #8430 after directions change 

#FEATURE 2 : listing date freshness -Days,weekdays,month

train$created <- as.POSIXct(train$created)
train$Days <- as.numeric((difftime(max(train$created)+1,train$created,units='days')))
train$month <- months(train$created)
train$weekday <- weekdays(train$created)

table(train$interest_level,train$days)
ggplot(train, aes(cap, fill = interest_level,alpha = 0.2)) +
  geom_histogram(alpha = 0.8, binwidth = 1, size = 0.5)+
  scale_x_continuous(limits = c(1,50))
  #coord_flip()
  #facet_grid(.~interest_level)

#FEATURE 2 : Street Addresses
similiarity <- levenshteinSim(tolower(train$street_address),tolower(train$display_address))
train$similarity <- NULL
train$similarity <- similiarity

#FEATURE 3 : Total rooms
train$total_rooms <- NA
train$total_rooms <- as.numeric(train$bathrooms + train$bedrooms)


#ggplot(train, aes(interest_level,len, fill = interest_level,alpha = 0.2)) +
 # scale_y_continuous(limits=c(0,500))+
 # geom_violin(scale = "area",alpha = 0.8)
  #coord_flip()



#FEATURE 4 : description length, CAPITAL LETTERS matter?, contact no,email id matter? 

train$len <- str_count(train$description, "\\S+")

email_pat = "([a-z0-9_\\.-]+)@([\\da-z\\.-]+)\\.([a-z\\.]{2,6})"
phone_pat = "([0-9])[- .]([0-9])|([0-9])[- .]([0-9])[- .]([0-9])|[0-9]{4,10}"
cap_pat = "[A-Z]+\\s+[A-Z+\\w]"

train$mail <- grepl(pattern = email_pat,train$description)
train$phone <- grepl(pattern = phone_pat,train$description)
train$cap <- str_count(train$description, cap_pat)


#FEATURE 5 Road, avenue, Street, place? does it matter?

train$sappr <- str_extract(train$display,pattern = "(street)|(avenue)|(place)|(plaza)|(road)")
train$dir <- str_extract(train$display,pattern = "(east)|(west)|(north)|(south)|(center)")

#train$snum <- str_extract(train$display,pattern = "([0-9]*[0-9]+(st|rd|nd|th))|([0-9]*[0-9] street)")
#train$snum <- str_extract(train$snum,pattern = "[0-9]*[0-9]")


new <- train[,c("bathrooms","bedrooms", "sappr","dir","latitude","similarity",
                "longitude","price","phone","cap","mail","len",
                "total_rooms","Days","weekday")]
library(caret)
library(xgboost)
dummies <- dummyVars(~ mail+phone+cap+
                       weekday+dir+sappr+similarity+bathrooms+bedrooms+
                       longitude+latitude+price+total_rooms+
                       len+Days, data = train)
full <- as.data.frame(predict(dummies, newdata = train))
test_new <- full[49353:124011,]
train_new <- full[1:49352,]
xgb <- xgboost(data = data.matrix(train_new),
               missing = NA,
               label = (as.numeric(train$interest_level[1:49352])-1), 
               eta = 0.1,
               max_depth = 15, 
               nround=20, 
               seed = 1,
               num_class = 3,
               objective = "multi:softprob",
               nthread = -1
)

pred_test <- predict(xgb,data.matrix(test_new))
z <- data.frame(unlist(pred_test),by_sli)
out <- data.frame(listing_id=test$listing_id,prediction)



importance_matrix <- xgb.importance(model = xgb)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
 
output <- data.frame(matrix(unlist(pred_test),ncol = 3, byrow=TRUE))                 
output <- setNames(output, c("low","medium","high"))
output$listing_id <- test$listing_id
output <- setNames(output, "listing_id","low","medium","high")
write.csv(output[,c("listing_id","high","medium","low")],"output.csv",row.names=FALSE)
