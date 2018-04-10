cat("\014")
rm(list = ls())

#libraries("aws.s3", "data.table", "polycor", "ggplot2", "monomvn", "randomForest", "xgboost", "doSNOW", "caret",
  #        "plyr", "e1071", "bst")

library(aws.s3)
library(data.table)
library(polycor)
library(ggplot2)
library(monomvn)
library(randomForest)
library(xgboost)
library(doSNOW)
library(caret)
library(plyr)
library(e1071)
library(bst)
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAIBWN7DL2EYGJCQMA", 
           "AWS_SECRET_ACCESS_KEY" = "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")
bucketlist()
list_the_available_dataset = get_bucket("earthquake-project-bucket")
raw_train_values = get_object(bucket = "earthquake-project-bucket", object = "train_values.csv")
raw_train_label = get_object(bucket = "earthquake-project-bucket", object = "train_labels.csv")
EQ_Train_Values = read.csv(text = rawToChar(raw_train_values))
EQ_Train_Labels = read.csv(text = rawToChar(raw_train_label))
EQ_Train_Dataset = merge(EQ_Train_Values, EQ_Train_Labels, by = "building_id")

aggregate(EQ_Train_Dataset["age"], list(EQ_Train_Dataset$damage_grade), mean) #this command is used to find the mean age of different damage grades

# dt1 = EQ_Train_Dataset[EQ_Train_Dataset$height > 4.653,] this is used to split the dataset above average height
# dt2 = EQ_Train_Dataset[EQ_Train_Dataset$height < 4.653,] this is used to split the dataset below average height
# dt1 = EQ_Train_Dataset[EQ_Train_Dataset$area > 38.4381,] this is used to split the dataset above average area
# dt2 = EQ_Train_Dataset[EQ_Train_Dataset$area < 38.4381,] this is used to split the dataset above average area

# sapply(EQ_Train_Dataset, class) this function is used to find the class of all columns in the table

# created numeric values for the categories available in categorical columns to make the analysis easy
EQ_Train_Dataset$land_surface_condition_numeric_values = factor(EQ_Train_Dataset$land_surface_condition, labels = c("1", "2", "3"))
EQ_Train_Dataset$foundation_type_numeric_values = factor(EQ_Train_Dataset$foundation_type, labels = c("1", "2", "3", "4", "5"))
EQ_Train_Dataset$roof_type_numeric_values = factor(EQ_Train_Dataset$roof_type, labels = c("1", "2", "3"))
EQ_Train_Dataset$ground_floor_type_numeric_values = factor(EQ_Train_Dataset$ground_floor_type, labels = c("1", "2", "3", "4", "5"))
EQ_Train_Dataset$other_floor_type_numeric_values = factor(EQ_Train_Dataset$other_floor_type, labels = c("1", "2", "3", "4"))
EQ_Train_Dataset$position_numeric_values = factor(EQ_Train_Dataset$position, labels = c("1", "2", "3", "4"))
EQ_Train_Dataset$plan_configuration_numeric_values = factor(EQ_Train_Dataset$plan_configuration, labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))

# re-arranged the columns to make it meaningful
EQ_Train_Dataset = EQ_Train_Dataset[,c(1,2,3,4,5,6,7,8,9,41,10,42,11,43,12,44,13,45,14,46,15,47,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40)]

# convert all the categorical values into numerical values to find the correlation with the damage_grade
EQ_Train_Dataset$land_surface_condition_numeric_values = as.numeric(EQ_Train_Dataset$land_surface_condition_numeric_values)
EQ_Train_Dataset$foundation_type_numeric_values = as.numeric(EQ_Train_Dataset$foundation_type_numeric_values)
EQ_Train_Dataset$roof_type_numeric_values = as.numeric(EQ_Train_Dataset$roof_type_numeric_values)
EQ_Train_Dataset$ground_floor_type_numeric_values = as.numeric(EQ_Train_Dataset$ground_floor_type_numeric_values)
EQ_Train_Dataset$other_floor_type_numeric_values = as.numeric(EQ_Train_Dataset$other_floor_type_numeric_values)
EQ_Train_Dataset$position_numeric_values = as.numeric(EQ_Train_Dataset$position_numeric_values)
EQ_Train_Dataset$plan_configuration_numeric_values = as.numeric(EQ_Train_Dataset$plan_configuration_numeric_values)

EQ_Train_Dataset$superstructure_material_type = ifelse(EQ_Train_Dataset$has_superstructure_adobe_mud == "1", "Adobe and Mud",
                                                ifelse(EQ_Train_Dataset$has_superstructure_mud_mortar_stone == "1", "Mud, Mortar and Stone",
                                                ifelse(EQ_Train_Dataset$has_superstructure_stone_flag == "1", "Stone",
                                                ifelse(EQ_Train_Dataset$has_superstructure_cement_mortar_stone == "1", "Cement, Mortar and Stone",
                                                ifelse(EQ_Train_Dataset$has_superstructure_bamboo == "1", "Bamboo",
                                                ifelse(EQ_Train_Dataset$has_superstructure_cement_mortar_brick == "1", "Cement, Mortar and Brick",
                                                ifelse(EQ_Train_Dataset$has_superstructure_mud_mortar_brick == "1", "Mud, Mortar and Brick",
                                                ifelse(EQ_Train_Dataset$has_superstructure_timber == "1", "Timber",
                                                ifelse(EQ_Train_Dataset$has_superstructure_rc_engineered == "1", "RC Engineered Reinforced Steel",
                                                ifelse(EQ_Train_Dataset$has_superstructure_rc_non_engineered == "1", "RC Non-Engineered Reinforced Steel","Other Material Type"))))))))))

EQ_Train_Dataset$has_superstructure_adobe_mud = NULL
EQ_Train_Dataset$has_superstructure_bamboo = NULL
EQ_Train_Dataset$has_superstructure_cement_mortar_brick = NULL
EQ_Train_Dataset$has_superstructure_cement_mortar_stone = NULL
EQ_Train_Dataset$has_superstructure_mud_mortar_brick = NULL
EQ_Train_Dataset$has_superstructure_mud_mortar_stone = NULL
EQ_Train_Dataset$has_superstructure_other = NULL
EQ_Train_Dataset$has_superstructure_rc_engineered = NULL
EQ_Train_Dataset$has_superstructure_rc_non_engineered = NULL
EQ_Train_Dataset$has_superstructure_stone_flag = NULL
EQ_Train_Dataset$has_superstructure_timber = NULL

EQ_Train_Dataset$Secondary_use = ifelse(EQ_Train_Dataset$has_secondary_use == "1", 
                                 ifelse(EQ_Train_Dataset$has_secondary_use_agriculture == "1", "Agriculture",
                                 ifelse(EQ_Train_Dataset$has_secondary_use_gov_office == "1", "Government office",
                                 ifelse(EQ_Train_Dataset$has_secondary_use_health_post == "1", "Health Post",
                                 ifelse(EQ_Train_Dataset$has_secondary_use_hotel == "1", "Hotel",
                                 ifelse(EQ_Train_Dataset$has_secondary_use_industry == "1", "Industry",
                                 ifelse(EQ_Train_Dataset$has_secondary_use_institution == "1", "Institution",
                                 ifelse(EQ_Train_Dataset$has_secondary_use_rental == "1", "Rental",
                                 ifelse(EQ_Train_Dataset$has_secondary_use_school == "1", "School",
                                 ifelse(EQ_Train_Dataset$has_secondary_use_use_police == "1", "Police Station", "Other uses"))))))))),"No Secondary use at all")

EQ_Train_Dataset$has_secondary_use = NULL
EQ_Train_Dataset$has_secondary_use_agriculture = NULL
EQ_Train_Dataset$has_secondary_use_gov_office = NULL
EQ_Train_Dataset$has_secondary_use_health_post = NULL
EQ_Train_Dataset$has_secondary_use_hotel = NULL
EQ_Train_Dataset$has_secondary_use_industry = NULL
EQ_Train_Dataset$has_secondary_use_institution = NULL
EQ_Train_Dataset$has_secondary_use_other = NULL
EQ_Train_Dataset$has_secondary_use_rental = NULL
EQ_Train_Dataset$has_secondary_use_school = NULL
EQ_Train_Dataset$has_secondary_use_use_police = NULL

EQ_Train_Dataset$Secondary_use = factor(EQ_Train_Dataset$Secondary_use)
EQ_Train_Dataset$superstructure_material_type = factor(EQ_Train_Dataset$superstructure_material_type)
EQ_Train_Dataset$Secondary_use_numeric_values = factor(EQ_Train_Dataset$Secondary_use, labels = c("1","2","3","4","5","6","7","8","9","10","11"))
EQ_Train_Dataset$superstructure_material_type_numeric_values = factor(EQ_Train_Dataset$superstructure_material_type, labels = c("1","2","3","4","5","6","7","8","9","10","11"))
EQ_Train_Dataset$Secondary_use_numeric_values = as.numeric(EQ_Train_Dataset$Secondary_use_numeric_values)
EQ_Train_Dataset$superstructure_material_type_numeric_values = as.numeric(EQ_Train_Dataset$superstructure_material_type_numeric_values)

EQ_Train_Dataset = EQ_Train_Dataset[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,26,29,27,28,23,24,25)]

x = EQ_Train_Dataset[,c("superstructure_material_type_numeric_values","Secondary_use_numeric_values","count_floors_pre_eq","area","height","land_surface_condition_numeric_values", "foundation_type_numeric_values", "roof_type_numeric_values", "ground_floor_type_numeric_values", "other_floor_type_numeric_values", "position_numeric_values", "plan_configuration_numeric_values")]
y = EQ_Train_Dataset[,c("damage_grade")]
cor(x, y)

EQ_Train_Dataset$damage_level_name = ifelse(EQ_Train_Dataset$damage_grade == "1", "Lowest Damage",
                                     ifelse(EQ_Train_Dataset$damage_grade == "2", "Medium Damage", "Almost Destroyed"))
#ggplot(data = EQ_Train_Dataset) + geom_bar(mapping = aes(x = superstructure_material_type, fill = damage_level_name)) + ggtitle("Building Material vs Damage level") + xlab("Material Type") + ylab("Count") + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

Final_dataset = subset(EQ_Train_Dataset, select = c(area, height, age, land_surface_condition, foundation_type, damage_grade))
#Percentage_distribution_damage_level = prop.table(table(Final_dataset$damage_level_name))*100
#summary(Final_dataset)

split_index = createDataPartition(y = Final_dataset$damage_grade, p = 0.7, list = FALSE)
train_data = Final_dataset[split_index,]
test_data = Final_dataset[-split_index,]

set.seed(54321)
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3033)
# model = caret::train(damage_grade ~ ., data = train_data, method = "svmLinear", trControl = trctrl, preProcess = c("center","scale"), tuneLength = 10)
# testing_prediction = round(predict(model, newdata = test_data))
# #testing_prediction[testing_prediction==-1] = 0
# confusionMatrix(testing_prediction, test_data$damage_grade)
# dtree_fit = caret::train(damage_grade ~ foundation_type + age + area, data = train_data, method = "rpart", parms = list(split = "information"), trControl = trctrl, tuneLength = 10)
# prediction = round(predict(dtree_fit, newdata = test_data))
# confusionMatrix(prediction, test_data$damage_grade)
dtree_fit = caret::train(damage_grade ~ area + age + height, data = train_data, method = "M5", trControl = trctrl, tuneLength = 10)
prediction = round(predict(dtree_fit, newdata = test_data))
confusionMatrix(prediction, test_data$damage_grade)