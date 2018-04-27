cat("\014")
rm(list = ls())

setwd("/home/kishore/Documents/Earth_quake_project/")
#install.packages("aws.s3", "RCurl", "data.table", "polycor", "party", "ggplot2", "doSNOW", "plyr","dplyr", "tidyr",
# "arules", "arulesViz", "caret", "foreign", "nnet")
Packages_tobe_loaded = c("aws.s3", "RCurl", "data.table", "polycor", "party", "ggplot2", "doSNOW", "plyr", "dplyr", "tidyr",
                         "arules", "arulesViz", "caret", "foreign", "nnet", "party")
lapply(Packages_tobe_loaded, library, character.only = TRUE)
EQ_Train_Values = read.csv(text = getURL("https://raw.githubusercontent.com/kishore4394/Nepal_EarthQuake/master/train_values.csv"), header = TRUE)
EQ_Train_Labels = read.csv(text = getURL("https://raw.githubusercontent.com/kishore4394/Nepal_EarthQuake/master/train_labels.csv"), header = TRUE)
EQ_Train_Dataset = merge(EQ_Train_Values, EQ_Train_Labels, by = "building_id")
EQ_Train_Dataset$rowID = seq.int(nrow(EQ_Train_Dataset))
aggregate(EQ_Train_Dataset["age"], list(EQ_Train_Dataset$damage_grade), mean) #this command is used to find the mean age of different damage grades

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
EQ_Train_Dataset$rowID = NULL
EQ_Train_Dataset$Secondary_use_numeric_values = NULL

# EQ_Train_Dataset$damage_level_name = ifelse(EQ_Train_Dataset$damage_grade == "1", "Lowest Damage",
#                                      ifelse(EQ_Train_Dataset$damage_grade == "2", "Medium Damage", "Almost Destroyed"))
#ggplot(data = EQ_Train_Dataset) + geom_bar(mapping = aes(x = superstructure_material_type, fill = damage_level_name)) + ggtitle("Building Material vs Damage level") + xlab("Material Type") + ylab("Count") + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
cat("\014")

################                    Association Analysis                    ###############################

EQ_Train_Dataset$damage_grade = as.factor(EQ_Train_Dataset$damage_grade)
EQ_Train_Dataset$geo_level_1_id = as.factor(EQ_Train_Dataset$geo_level_1_id)
EQ_Train_Dataset$age = as.factor(EQ_Train_Dataset$age)
EQ_Train_Dataset$height = as.factor(EQ_Train_Dataset$height)
EQ_Train_Dataset$area = as.factor(EQ_Train_Dataset$area)
EQ_Train_Dataset$roof_type = as.factor(EQ_Train_Dataset$roof_type)
EQ_Train_Dataset$other_floor_type = as.factor(EQ_Train_Dataset$other_floor_type)
EQ_Train_Dataset$position = as.factor(EQ_Train_Dataset$position)
# ref_data = subset(EQ_Train_Dataset, EQ_Train_Dataset$damage_grade == "3")
#categorical_columns = EQ_Train_Dataset[,c("land_surface_condition", "foundation_type", "roof_type", "position","damage_grade")] 
# Association_between_columns = eclat(categorical_columns, parameter = list(supp = 0.3, maxlen = 100))
# inspect(Association_between_columns)

####################################           Prediction       #################################################
# 
# ref_dataset = EQ_Train_Dataset[,c("building_id", "geo_level_1_id", "age", "height", "land_surface_condition",
#                                   "ground_floor_type", "plan_configuration", "has_superstructure_adobe_mud", 
#                                   "has_superstructure_mud_mortar_stone", "has_superstructure_stone_flag",         
#                                   "has_superstructure_cement_mortar_stone", "has_superstructure_mud_mortar_brick",
#                                   "has_superstructure_cement_mortar_brick", "has_superstructure_timber",
#                                   "has_superstructure_bamboo",  "has_superstructure_rc_non_engineered",
#                                   "has_superstructure_rc_engineered", "has_superstructure_other", "Secondary_use", "damage_grade")]
 split_index = createDataPartition(y = EQ_Train_Dataset$damage_grade, p = 0.7, list = FALSE)
 train_data = EQ_Train_Dataset[split_index,]
 test_data = EQ_Train_Dataset[-split_index,]
# 
 set.seed(54321)
 trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
 set.seed(3033)

  model = multinom(damage_grade ~ ., data = train_data)
 prediction_model = predict(model, test_data)
 confusionMatrix(prediction_model, test_data$damage_grade)