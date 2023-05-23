#Name: Roshan Baniya
#Roll Number: NPI000108
#Data Import
EmployeeAttrition = read.csv("C:\\Users\\hp\\Desktop\\R programme\\employee_attrition.csv")
print(EmployeeAttrition)
View(EmployeeAttrition)


#Data pre processing
EmployeeAttrition$EmployeeID <- as.factor(EmployeeAttrition$EmployeeID)
EmployeeAttrition$age <- as.factor(EmployeeAttrition$age)
EmployeeAttrition$length_of_service <- as.factor(EmployeeAttrition$length_of_service)
EmployeeAttrition$city_name <- as.factor(EmployeeAttrition$city_name)
EmployeeAttrition$department_name <- as.factor(EmployeeAttrition$department_name)
EmployeeAttrition$job_title <- as.factor(EmployeeAttrition$job_title)
EmployeeAttrition$store_name <- as.factor(EmployeeAttrition$store_name)
EmployeeAttrition$gender_full <- as.factor(EmployeeAttrition$gender_full)
EmployeeAttrition$termreason_desc <- as.factor(EmployeeAttrition$termreason_desc)
EmployeeAttrition$termtype_desc <- as.factor(EmployeeAttrition$termtype_desc)
EmployeeAttrition$STATUS_YEAR <- as.factor(EmployeeAttrition$STATUS_YEAR)
EmployeeAttrition$STATUS <- as.factor(EmployeeAttrition$STATUS)
EmployeeAttrition$BUSINESS_UNIT <- as.factor(EmployeeAttrition$BUSINESS_UNIT)
#Name: Roshan Baniya
#Roll Number: NPI000108
#Data Cleaning
#EmployeeID
names(EmployeeAttrition)[names(EmployeeAttrition) == 'EmployeeID'] <- 'employee_id'
#StatusYear
names(EmployeeAttrition)[names(EmployeeAttrition) == 'STATUS_YEAR'] <- 'status_year'
#Status
names(EmployeeAttrition)[names(EmployeeAttrition) == 'STATUS'] <- 'status'
#Business Unit
names(EmployeeAttrition)[names(EmployeeAttrition) == 'BUSINESS_UNIT'] <- 'business_unit'
#Gender short
EmployeeAttrition <- select(EmployeeAttrition, -c(gender_short))
#Origin hire date
EmployeeAttrition <- select(EmployeeAttrition, -c(orighiredate_key))
#Birthdate
EmployeeAttrition <- select(EmployeeAttrition, -c(birthdate_key))

#Name: Roshan Baniya
#Roll Number: NPI000108
#Data Exploration
#Display the attributes
str(EmployeeAttrition)
#List the name of the variable
names(EmployeeAttrition)
#Number of row calculation
nrow(EmployeeAttrition)
#Number of column calculation
ncol(EmployeeAttrition)
#Show summary of CSV file
summary(EmployeeAttrition)
#Name: Roshan Baniya Tp: NPI000108
#Question number 1 (First Analysis)
EmployeeAttrition %>%
  filter(!(termtype_desc %in% "Not Applicable")) %>% 
  ggplot() +
  aes(y = age, fill = termtype_desc) +
  geom_bar() +
  labs(
    title = "Relationship between employee age and job position with attrition",
    caption = "Roshan Baniya TP: NPI000108"
  ) +
  theme_minimal()
#Name: Roshan Baniya Tp: NPI000108
#Question number 1 (Second Analysis)
EmployeeAttrition %>%
  filter(!(termtype_desc %in% "Not Applicable")) %>% 
  ggplot() +
  aes(x=length_of_service, fill = termtype_desc) +
  geom_histogram( stat = "count") +
  labs(
    title = "Relationship between length of service and job position",
    caption = "Roshan Baniya TP: NPI000108"
  ) +
  theme_minimal()

#Name: Roshan Baniya Tp: NPI000108
#Question number 1 (Third Analysis)
EmployeeAttrition %>%
  filter(!(termtype_desc %in% "Not Applicable")) %>% 
  ggplot() +
  aes(x=gender_full, fill = termtype_desc) +
  geom_bar() +
  labs(
    title = "Attrition by gender",
    caption = "Roshan Baniya TP: NPI000108"
  ) +
  theme_minimal()

#Name: Roshan Baniya Tp: NPI000108
#Question number 1 (Fourth Analysis)
EmployeeAttrition %>%
  filter(!(termtype_desc %in% "Not Applicable")) %>% 
  ggplot() +
  aes(y=job_title, fill = termtype_desc) +
  geom_bar() +
  labs(
    title = "Relation between job position and attrition",
    caption = "Roshan Baniya TP: NPI000108"
  ) +
  theme_minimal()

#Name: Roshan Baniya Tp: NPI000108
#Question number 2 (First Analysis)
ggplot(EmployeeAttrition, aes(x = age, fill = gender_full)) +
  geom_bar(aplha=0.7, position = "identity", stat = "count") +
  ggtitle("Distribution of Ages for Male and Female Employees") +
  xlab("Age") +
  labs(
    caption = "Roshan Baniya TP: NPI000108"
    ) +
    scale_fill_manual(values =  c ("red","green"), labels = c ("Male", "Female"))
#Name: Roshan Baniya Tp: NPI000108
#Question number 2 (Second Analysis)
ggplot(EmployeeAttrition, aes(x = employee_id, y =age, color = gender_full)) +
  geom_point(aplha=0.7) +
  ggtitle("Relation between employee id and age") +
  labs(
    caption = "Roshan Baniya TP: NPI000108"
  ) +
  xlab("Employee ID") +
  ylab("Age") +
  scale_color_manual(values =  c ("red","green"), labels = c ("Male", "Female"))
#Name: Roshan Baniya Tp: NPI000108
#Question number 2 (Third Analysis)
ggplot(EmployeeAttrition, aes(x = age, y =status, color = gender_full)) +
  geom_line(aplha=0.7) +
  ggtitle("Relation between employee age and their business unit") +
  labs(
    caption = "Roshan Baniya TP: NPI000108"
  ) +
  xlab("Age") +
  ylab("Status") +
  scale_color_manual(values =  c ("red","green"), labels = c ("Male", "Female"))
#Name: Roshan Baniya Tp: NPI000108
#Question number 3 (First Analysis)
ggplot(EmployeeAttrition, aes(x = age, fill = gender_full)) +
  geom_histogram(alpha = 0.7, position = "identity", stat = "count") +
  ggtitle("Histogram of age group") +
  labs(
    caption = "Roshan Baniya TP: NPI000108"
  ) +
  xlab("Age") +
  scale_fill_manual(values = c("red", "green"), labels = c("Male", "Female"))
#Name: Roshan Baniya Tp: NPI000108
#Question number 3 (Second Analysis)
ggplot(EmployeeAttrition, aes(y = length_of_service, x = employee_id, color = gender_full)) +
  geom_line(alpha = 0.7) +
  ggtitle("Line graph to show relation between length of service and employee id") +
  labs(
    caption = "Roshan Baniya TP: NPI000108"
  ) +
  ylab("Length of service") +
  xlab("Employee ID") +
  scale_color_manual(values = c("red", "green"), labels = c("Male", "Female"))

#Name: Roshan Baniya Tp: NPI000108
#Question number 4 (First Analysis)
EmployeeAttrition %>%
  ggplot() +
  aes(x = store_name, y = city_name) + 
  geom_point() +
  labs(
    title = "Relationship between city name and store name",
    caption = "Roshan Baniya TP:NPI000108"
  ) +
  theme_minimal()
#Name: Roshan Baniya Tp: NPI000108
#Question number 4 (Second Analysis)
EmployeeAttrition %>%
  group_by(city_name, status) %>%
  summarize(n = n()) %>%
  ggplot() +
  aes(x = "", y = n, fill = status) +
  geom_bar(stat="identity", width = 1, position = "fill") +
  coord_polar("y", start = 0) +
  labs(
    title = "Relationship between city name and status",
    caption = "Roshan Baniya TP: NPI000108"
  ) +
  theme_minimal() +
  guides(fill=guide_legend(title="Status"))
  
 
  
