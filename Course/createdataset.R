### SIMULATE FAKE DATA FOR R COURSE
# Data based on students at university

# clear all
rm(list=ls())

# install and load packages (if not already done so)
package <- c("tidyverse", "msm", "MASS", "simstudy", "scales", "fabricatr")

# for installing new packages
new.packages <- package[!(package %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load packages
lapply(package, library , character.only = TRUE)


###############################################################
########## simulate fake student data at individual data
###############################################################


# set seed so you get same values everytime you run this
set.seed(10)

### First create categorical variables variables

# faculty
faculty <- sample(c("Business","Economics", "Sociology", "Political Science"), size = 1000,replace=TRUE, prob = c(0.35,0.23,0.15,0.27))
data <- as.data.frame(faculty)

# Sex

data <- data %>%
  mutate(sex =
          case_when(
            faculty=="Business"~ sample(c("Male","Female"), size=1000, replace=TRUE, prob = c(0.65,0.35)),
            TRUE ~ NA_character_),
        sex =
            case_when(
              faculty=="Economics"~ sample(c("Male","Female"), size=1000, replace=TRUE, prob = c(0.75,0.25)),
              TRUE ~ sex),
        sex =
          case_when(
            faculty=="Sociology"~ sample(c("Male","Female"), size=1000, replace=TRUE, prob = c(0.50,0.50)),
            TRUE ~ sex),
        sex =
          case_when(
            faculty=="Political Science"~ sample(c("Male","Female"), size=1000, replace=TRUE, prob = c(0.40,0.60)),
            TRUE ~ sex))



# relationship status

data <- data %>%
  mutate(relationship =
           case_when(sex=="Male" ~  sample(c("Single","In a relationship"), size=1000, replace=TRUE, prob = c(0.70,0.30)),
                                           TRUE ~ NA_character_),
        relationship =
           case_when(sex=="Female" ~  sample(c("Single","In a relationship"), size=1000, replace=TRUE, prob = c(0.75,0.25)),
                                                                             TRUE ~ relationship))

# semester
data$term <- sample(0:14,1000, replace=TRUE)

# country of birth
data$cob <- factor(sample(c("Germany","France", "UK", "Austria", "Netherlands", "Spain", "Italy"), size=1000, replace=TRUE, prob = c(0.30,0.15,0.15,0.20,0.10,0.5,0.5 )))
data$university <- rep(c("Berlin"), length.out = 1000)

# parents'
data$workingclass <- factor(rep(c("yes","no"), each=500))

# student job
data <- data %>%
  mutate(job =
        case_when(workingclass=="yes" ~ sample(c("yes","no"), size=1000, replace=TRUE, prob = c(0.65,0.35)),
                    TRUE ~ NA_character_),
    job = 
      case_when(workingclass=="no" ~ sample(c("yes","no"), size=1000, replace=TRUE, prob = c(0.20,0.80)),
                    TRUE ~ job))


# age
data$age <- fabricatr::correlate(given = data$term, rho = 0.8,
                                rnorm, mean = 23, sd=6)
data$age <- data$age-5
data$age <- rescale(data$age, to= c(16,35))



## Life satisfaction (0-100)
data$lifesat <- fabricatr::correlate(given = data$age, rho = 0.6,
                               rnorm, mean = 60, sd=30)

data <- data %>% mutate(lifesat = case_when(sex=="Male" ~ fabricatr::correlate(given = age, rho = 0.8,
                                     rnorm, mean = 60, sd=20),
                                     TRUE ~ fabricatr::correlate(given = age, rho = 0.4,
                                                                 rnorm, mean = 40, sd=30)))

data$lifesat <- (data$lifesat - min(data$lifesat)) / (max(data$lifesat) - min(data$lifesat)) * 100 

# create some age outliers
data[888, 9] <- 80
data[887, 9] <- 77
data[666, 9] <- 45
data[667, 9] <- 40
data[555, 9] <- 36
data[444, 9] <- 32
data[445, 9] <- 33
data[333, 9] <- 17
data[333, 9] <- 10

# check relationship
ggplot(data, aes(y=age, x=lifesat)) + 
  geom_point()

# Grade Point Average
data <- data %>% mutate(gpa_2010 = rtnorm(1000, 1.9, lower=0, upper=6),
                        gpa_2011 = rtnorm(1000, 2.2, lower=0, upper=6),
                        gpa_2012 = rtnorm(1000, 2.4, lower=0, upper=6),
                        gpa_2013 = rtnorm(1000, 2.7, lower=0, upper=6),
                        gpa_2014 = rtnorm(1000, 2.8, lower=0, upper=6),
                        gpa_2015 = rtnorm(1000, 2.8, lower=0, upper=6),
                        gpa_2016 = rtnorm(1000, 2.0, lower=0, upper=6),
                        gpa_2017 = rtnorm(1000, 1.5, lower=0, upper=6), 
                        gpa_2018 = rtnorm(1000, 3.2, lower=0, upper=6),
                        gpa_2019 = rtnorm(1000, 3.4, lower=0, upper=6), 
                        gpa_2020 = rtnorm(1000, 4.0, lower=0, upper=6)) %>%
                        mutate(across(matches("gpa"), ~round(.,digits = 1))) 



# liking the university/ study programme
data <- data %>% mutate(
  like = floor(case_when(faculty=="Sociology" ~ rtnorm(n(), 1, lower=0, upper=6),
                         faculty=="Business" ~ rtnorm(n(), 3, lower=0, upper=6),
                         faculty=="Economics" ~ rtnorm(n(), 2, lower=0, upper=6),
                         faculty=="Political Science" ~ rtnorm(n(), 4, lower=0, upper=6),
                         TRUE ~ NA_real_))) 

data$like <- rescale(data$like, c(1,6))
     
  table(data$like)
  
## course  
  
data <- data %>%
  mutate(
    course =
      ifelse(
        faculty=="Political Science",
        rep(c("institutions", "election", "int. organizations", "EU"), length=n()), 
        ifelse(
          faculty=="Sociology",
          rep(c("inequality", "education", "migration", "theory", "discourse"), length= n()),
          ifelse(
            faculty=="Business",
            rep(c("micro", "accounting", "marketing", "strategy", "leadership", "statistics"), length=n()),
            ifelse(
              faculty=="Economics",
              rep(c("micro I", "micro II", "macro I", "macro II", "math", "central banks", "employment"), length=n()),
              "")))))



# create missing values for 10% of sample
data <- data %>% 
  mutate(randvar = runif(1000,0,1)) %>%
  mutate_all(as.character) %>%
  mutate(faculty = case_when(randvar<0.01  ~ NA_character_,
                             TRUE ~ faculty),
         course = case_when(randvar<0.015 & randvar>0.005   ~ NA_character_,
                            TRUE ~ course)) %>%
  gather(2:22, key= "variables", value= "values") %>%
  mutate(values = case_when(randvar<1 & randvar>0.99 & !variables %in% c("faculty", "course") ~ NA_character_,
                   TRUE  ~ values)) %>%
  spread(key = "variables", value= "values") 


# data by faculty
faculty <- data %>% group_by(faculty) %>% filter(faculty!= "NA") %>% count() %>% 
  mutate(profs =  floor(runif(1, 50, 100)),
         salary = floor(runif(1,50000,100000)),
         costs = floor(runif(1,20000,40000))) %>%
  rename(students=n)


# data by course
course <- data %>% group_by(course) %>% filter(course!= "NA") %>% count() %>%
  mutate(profexp =  floor(runif(1, 1, 30)),
         profsex = sample(c("Male","Female"), size=1, replace=TRUE, prob = c(0.65,0.35)),
         deliverable = sample(c("paper", "exam", "presentation + paper"), size=1, replace=TRUE, prob = c(0.30,0.50, 0.20)),
         timing = sample(c("8 am", "10 am", "noon", "2 pm", "4 pm", "6 pm", "8 pm"), size=1, replace=TRUE, prob = c(0.15,0.30, 0.05, 0.15, 0.25, 0.05, 0.05))) %>%
  rename(students=n) %>%
  mutate(profexp= as.double(profexp))
  

# vary the types
str(data)
order(data$faculty)

data <- data %>%   
  mutate(across(c("like", "term"), as.factor)) %>%
  mutate(across(c("age"), as.numeric)) 

# let's create another student dataset to append later
students2 <- data %>% filter(between(randvar,0.45, 0.55)) %>% dplyr::select(everything(), -randvar, -relationship)

# kick out aux vars
data <- data %>% dplyr::select(everything(), -randvar)

table(data$gpa_2010)

# save as excel file

# set working directory:
setwd("C:/Users/Jasper Tjaden/OneDrive/work/Teaching/Courses/Potsdam/2_SoSe2021/Intro to R for Social Scientists/materials/")

openxlsx::write.xlsx(data,"students.xlsx", asTable=TRUE)
openxlsx::write.xlsx(students2,"students2.xlsx", asTable=TRUE)
openxlsx::write.xlsx(course,"course.xlsx")
openxlsx::write.xlsx(faculty,"faculty.xlsx")


###### end





































