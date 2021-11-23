#Importing dependencies 
library(dplyr)
library(tidyverse)
library(ggplot2)
#Setting Directory
getwd()
setwd("D:\\R Assignment")
#Importing dataset
data <- read.csv("Types of vehicle data.csv")
#Viewing dataset
view(data)
#Grouping dataset in region wise 
North_region <- filter(data, States.UTs %in% c("Delh", "Haryana", "Himachal Pradesh", "Jammu & Kashmir", "Punjab", "Rajasthan", "Uttarakhand"))
Central_region <- filter(data, States.UTs %in% c("Chandigarh", "Madhya Pradesh", "Uttar Pradesh"))
East_region <- filter(data, States.UTs %in% c("Bihar", "Jharkhand", "Odisha", "West Bengal"))
Northeast_region <- filter(data, States.UTs %in% c("Arunachal Pradesh", "Assam", "Assam", "
Meghalaya", "Mizoram", "Nagaland", "Sikkim", "Tripura"))
West_region <- filter(data, States.UTs %in% c("Goa", "Gujarat", "Maharashtra"))
South_region <- filter(data, States.UTs %in% c("Andhra Pradesh", "Karnataka", "Kerala", "Tamil Nadu"))
#Splitting in Male & female category
##North_male
North_region_male <- select(North_region, States.UTs, Pedestrian...Male, Bycycles...Male, Two.Wheelers...Male, Auto.Rickshaws...Male, Cars..taxies.Vans...LMV...Male, Trucks.Lorries...Male, Buses...Male)
##North_female
North_region_Female <- select(North_region, States.UTs, Pedestrian...Female, Bycycles...Female, Two.Wheelers...Female, Auto.Rickshaws...Female, Cars..taxies.Vans...LMV...Female, Trucks.Lorries...Female, Buses...Female)
##South_male
South_region_male <- select(South_region, States.UTs, Pedestrian...Male, Bycycles...Male, Two.Wheelers...Male, Auto.Rickshaws...Male, Cars..taxies.Vans...LMV...Male, Trucks.Lorries...Male, Buses...Male)
##South_female
South_region_Female <- select(South_region, States.UTs, Pedestrian...Female, Bycycles...Female, Two.Wheelers...Female, Auto.Rickshaws...Female, Cars..taxies.Vans...LMV...Female, Trucks.Lorries...Female, Buses...Female)
##Central_male
Central_region_male <- select(Central_region, States.UTs, Pedestrian...Male, Bycycles...Male, Two.Wheelers...Male, Auto.Rickshaws...Male, Cars..taxies.Vans...LMV...Male, Trucks.Lorries...Male, Buses...Male)
##Central_female
Central_region_Female <- select(Central_region, States.UTs, Pedestrian...Female, Bycycles...Female, Two.Wheelers...Female, Auto.Rickshaws...Female, Cars..taxies.Vans...LMV...Female, Trucks.Lorries...Female, Buses...Female)
##East_male
East_region_male <- select(East_region, States.UTs, Pedestrian...Male, Bycycles...Male, Two.Wheelers...Male, Auto.Rickshaws...Male, Cars..taxies.Vans...LMV...Male, Trucks.Lorries...Male, Buses...Male)
##East_female
East_region_Female <- select(East_region, States.UTs, Pedestrian...Female, Bycycles...Female, Two.Wheelers...Female, Auto.Rickshaws...Female, Cars..taxies.Vans...LMV...Female, Trucks.Lorries...Female, Buses...Female)
##North_male
Northeast_region_male <- select(Northeast_region, States.UTs, Pedestrian...Male, Bycycles...Male, Two.Wheelers...Male, Auto.Rickshaws...Male, Cars..taxies.Vans...LMV...Male, Trucks.Lorries...Male, Buses...Male)
##North_female
Northeast_region_Female <- select(Northeast_region, States.UTs, Pedestrian...Female, Bycycles...Female, Two.Wheelers...Female, Auto.Rickshaws...Female, Cars..taxies.Vans...LMV...Female, Trucks.Lorries...Female, Buses...Female)
##West_male
West_region_male <- select(West_region, States.UTs, Pedestrian...Male, Bycycles...Male, Two.Wheelers...Male, Auto.Rickshaws...Male, Cars..taxies.Vans...LMV...Male, Trucks.Lorries...Male, Buses...Male)
##West_female
West_region_Female <- select(West_region, States.UTs, Pedestrian...Female, Bycycles...Female, Two.Wheelers...Female, Auto.Rickshaws...Female, Cars..taxies.Vans...LMV...Female, Trucks.Lorries...Female, Buses...Female)
##South_male
South_region_male <- select(South_region, States.UTs, Pedestrian...Male, Bycycles...Male, Two.Wheelers...Male, Auto.Rickshaws...Male, Cars..taxies.Vans...LMV...Male, Trucks.Lorries...Male, Buses...Male)
##South_female
South_region_Female <- select(South_region, States.UTs, Pedestrian...Female, Bycycles...Female, Two.Wheelers...Female, Auto.Rickshaws...Female, Cars..taxies.Vans...LMV...Female, Trucks.Lorries...Female, Buses...Female)
#Plotting
North_region_male <- North_region_male %>% select(States.UTs, Pedestrian...Male, Bycycles...Male, Two.Wheelers...Male, Auto.Rickshaws...Male, Cars..taxies.Vans...LMV...Male, Trucks.Lorries...Male, Buses...Male) %>% gather(key = "variable", value = "value", -States.UTs)
ggplot(North_region_male, aes(fill=variable, y=value, x=States.UTs)) + geom_bar(position="dodge", stat="identity") + labs(title = "Accidents done By male in north_region")
North_region_Female <- North_region_Female %>% select(States.UTs, Pedestrian...Female, Bycycles...Female, Two.Wheelers...Female, Auto.Rickshaws...Female, Cars..taxies.Vans...LMV...Female, Trucks.Lorries...Female, Buses...Female) %>% gather(key = "variable", value = "value", -States.UTs)
ggplot(North_region_Female, aes(fill=variable, y=value, x=States.UTs)) + geom_bar(position="dodge", stat="identity") + labs(title = "Accidents done By Female in north_region")
Central_region_male <- Central_region_male %>% select(States.UTs, Pedestrian...Male, Bycycles...Male, Two.Wheelers...Male, Auto.Rickshaws...Male, Cars..taxies.Vans...LMV...Male, Trucks.Lorries...Male, Buses...Male) %>% gather(key = "variable", value = "value", -States.UTs)
ggplot(Central_region_male, aes(fill=variable, y=value, x=States.UTs)) + geom_bar(position="dodge", stat="identity") + labs(title = "Accidents done By male in Central_region")
Central_region_Female <- Central_region_Female %>% select(States.UTs, Pedestrian...Female, Bycycles...Female, Two.Wheelers...Female, Auto.Rickshaws...Female, Cars..taxies.Vans...LMV...Female, Trucks.Lorries...Female, Buses...Female) %>% gather(key = "variable", value = "value", -States.UTs)
ggplot(Central_region_Female, aes(fill=variable, y=value, x=States.UTs)) + geom_bar(position="dodge", stat="identity") + labs(title = "Accidents done By female in Central_region")
Northeast_region_male <- Northeast_region_male %>% select(States.UTs, Pedestrian...Male, Bycycles...Male, Two.Wheelers...Male, Auto.Rickshaws...Male, Cars..taxies.Vans...LMV...Male, Trucks.Lorries...Male, Buses...Male) %>% gather(key = "variable", value = "value", -States.UTs)
ggplot(Northeast_region_male, aes(fill=variable, y=value, x=States.UTs)) + geom_bar(position="dodge", stat="identity") + labs(title = "Accidents done By male in Northeast_region")
Northeast_region_Female <- Northeast_region_Female %>% select(States.UTs, Pedestrian...Female, Bycycles...Female, Two.Wheelers...Female, Auto.Rickshaws...Female, Cars..taxies.Vans...LMV...Female, Trucks.Lorries...Female, Buses...Female) %>% gather(key = "variable", value = "value", -States.UTs)
ggplot(Northeast_region_Female, aes(fill=variable, y=value, x=States.UTs)) + geom_bar(position="dodge", stat="identity") + labs(title = "Accidents done By female in Northeast_region")
West_region_male <- West_region_male %>% select(States.UTs, Pedestrian...Male, Bycycles...Male, Two.Wheelers...Male, Auto.Rickshaws...Male, Cars..taxies.Vans...LMV...Male, Trucks.Lorries...Male, Buses...Male) %>% gather(key = "variable", value = "value", -States.UTs)
ggplot(West_region_male, aes(fill=variable, y=value, x=States.UTs)) + geom_bar(position="dodge", stat="identity") + labs(title = "Accidents done By male in west_region")
West_region_Female <- West_region_Female %>% select(States.UTs, Pedestrian...Female, Bycycles...Female, Two.Wheelers...Female, Auto.Rickshaws...Female, Cars..taxies.Vans...LMV...Female, Trucks.Lorries...Female, Buses...Female) %>% gather(key = "variable", value = "value", -States.UTs)
ggplot(West_region_Female, aes(fill=variable, y=value, x=States.UTs)) + geom_bar(position="dodge", stat="identity") + labs(title = "Accidents done By female in west_region")
South_region_male <- South_region_male %>% select(States.UTs, Pedestrian...Male, Bycycles...Male, Two.Wheelers...Male, Auto.Rickshaws...Male, Cars..taxies.Vans...LMV...Male, Trucks.Lorries...Male, Buses...Male) %>% gather(key = "variable", value = "value", -States.UTs)
ggplot(South_region_male, aes(fill=variable, y=value, x=States.UTs)) + geom_bar(position="dodge", stat="identity") + labs(title = "Accidents done By male in south_region")
South_region_Female <- South_region_Female %>% select(States.UTs, Pedestrian...Female, Bycycles...Female, Two.Wheelers...Female, Auto.Rickshaws...Female, Cars..taxies.Vans...LMV...Female, Trucks.Lorries...Female, Buses...Female) %>% gather(key = "variable", value = "value", -States.UTs)
ggplot(South_region_Female, aes(fill=variable, y=value, x=States.UTs)) + geom_bar(position="dodge", stat="identity") + labs(title = "Accidents done By female in south_region")

