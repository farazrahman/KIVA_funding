#KIVA Funding

# 1.1- Load libraries and data files
library("readr") 
library('tidyr') 
library('dplyr') 
library('stringr') 
library('corrplot') 
library('purrr')
library('caret')
library('ggplot2')
library('cowplot')
library('caTools')
library('ggthemes')

options(scipen = 999)
#1.2- Read the training data files

kiva_loans <- read_csv("kiva_loans.csv")
kiva_locations <- read_csv("kiva_mpi_region_locations.csv")
loan_theme_ids <- read_csv("loan_theme_ids.csv")
loan_theme_region <- read_csv("loan_themes_by_region.csv")

#1.3- View the summary and have a glimpse of your data

summary(kiva_loans)
str(kiva_loans) #671205 obs. of  20 variables

summary(kiva_locations)
str(kiva_locations) #2772 obs. of  9 variables

summary(loan_theme_ids)
str(loan_theme_ids) #779092 obs. of  4 variables

summary(loan_theme_region)
str(loan_theme_region) #15736 obs. of  20 variables

###Analysing the KIVA loans data set first

#Checking for duplicate ids and there are none
sum(duplicated(kiva_loans$id))
length(unique(kiva_loans$id))

##Checking the missing data

theme1<- theme(axis.text.y = element_text(angle = 45, hjust = 1, vjust = 0.5), 
                   legend.position="none")
theme2<- theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5), 
               legend.position="none")

missing_data <- kiva_loans %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red")+coord_flip()+ theme_few()

##Loan Amount

plot_grid(ggplot(kiva_loans, aes(x = loan_amount)) + 
            geom_histogram(bins = 50,aes(color = loan_amount), fill = "blue") + 
            theme1 + scale_x_continuous(breaks = pretty(kiva_loans$loan_amount, n = 20))+
            xlab('Loan Amount') + 
            ylab('Count'),
            ggplot(kiva_loans, aes(x = funded_amount)) + 
            geom_histogram(bins = 50,aes(color = funded_amount), fill = "blue") + 
            theme1 + scale_x_continuous(breaks = pretty(kiva_loans$funded_amount, n = 20))+
            xlab('Funded Amount') +
            ylab('Count'), align = "h") 


##sector wise loan amount

loan_by_sector <- kiva_loans %>%
  group_by(sector) %>% 
  summarise(Total_loan_amount =sum(loan_amount))

ggplot(loan_by_sector, aes(x = reorder(sector, Total_loan_amount), y = Average_loan_amount, fill=Total_loan_amount )) + 
  geom_bar(stat = 'identity')+scale_color_gradient(low="yellow", high ="green")+coord_flip()+xlab("Sectors")


##Activity wise loan amount


unique(kiva_loans$activity)

loan_by_activity <- kiva_loans %>%
  group_by(activity) %>% 
  summarise(Total_loan_amount = sum(loan_amount)) %>%
  top_n(25, wt = Total_loan_amount)

p3 <- ggplot(loan_by_activity, aes(x = reorder(activity, Total_loan_amount), y = Total_loan_amount, fill=Total_loan_amount )) + 
  geom_bar(stat = 'identity')+scale_color_gradient(low="yellow", high ="green")+coord_flip()+xlab("Activity")

  
  Avgloan_by_activity <- kiva_loans %>%
    group_by(activity) %>% 
    summarise(Avg_loan_amount = mean(loan_amount)) %>%
    top_n(25, wt = Avg_loan_amount)
  
 p4<- ggplot(Avgloan_by_activity, aes(x = reorder(activity, Avg_loan_amount), y = Avg_loan_amount, fill=Avg_loan_amount )) + 
  geom_bar(stat = 'identity')+scale_color_gradient(low="yellow", high ="green")+coord_flip()+xlab("Activity")

 
 plot_grid(p3,p4, align = "h")
 

##Countrywise loan_amount
 
 loan_by_country <- kiva_loans %>%
   group_by(country) %>% 
   summarise(Total_loan_amount = sum(loan_amount))%>%
   top_n(25, wt = Total_loan_amount)
 
 p5 <- ggplot(loan_by_country, aes(x = reorder(country, Total_loan_amount), y = Total_loan_amount, fill=Total_loan_amount )) + 
   geom_bar(stat = 'identity')+scale_color_gradient(low="yellow", high ="green")+coord_flip()+xlab("Country")+theme1
 
 
 Avgloan_by_country <- kiva_loans %>%
   group_by(country) %>% 
   summarise(Avg_loan_amount = mean(loan_amount)) %>%
   top_n(25, wt = Avg_loan_amount)
 
 p6 <- ggplot(Avgloan_by_country, aes(x = reorder(country, Avg_loan_amount), y = Avg_loan_amount, fill=Avg_loan_amount )) + 
   geom_bar(stat = 'identity')+scale_color_gradient(low="yellow", high ="green")+coord_flip()+xlab("Country")+theme1
 
 
 plot_grid(p5,p6, align = "h")
 
##Regionwise loan_amount
 
 unique(kiva_loans$region)
 
 loan_by_region <- kiva_loans %>%
   group_by(region) %>% na.omit %>%
   summarise(Total_loan_amount = sum(loan_amount))%>%
   top_n(25, wt = Total_loan_amount)
 
 p7 <- ggplot(loan_by_region, aes(x = reorder(region, Total_loan_amount), y = Total_loan_amount, fill=Total_loan_amount )) + 
   geom_bar(stat = 'identity')+scale_color_gradient(low="yellow", high ="green")+coord_flip()+xlab("Region")+theme1
 
 
 

 
 

 