##this is for installing package to read STATA files
library(foreign)

##ethnicdensity.df is name of my ethnic density database
##read dta is the package that will read dta file. 
ethnicdensity.df = read.dta("//Users//andreafernandes//Google Drive//Springboard_Data_Science_Course_2016//RLearning_Springboard_ProjectED//For_Excel.dta")

##selecting a first three rows and first four columns of the data
str(ethnicdensity.df)

#install.packages("dplyr")
library("dplyr")

ED <-tbl_df(ethnicdensity.df)
dim(ED)

#ED
#47581 X 67

#print(ED, n =20)
data.frame(head(ED))

#filter
#filter(ED, Suicide==1)


#count of distinct IDs
#ED %>%
#  summarise(n_distinct(JunkID))


#count gender
ED %>% 
      select (Gender_Cleaned) %>%
      group_by(Gender_Cleaned) %>%
      summarise(GenderCount = n())


#count gender and suicide

ED %>% 
  select (Gender_Cleaned, Suicide) %>%
  group_by(Gender_Cleaned) %>%
  summarise(GenderCount = n(), sum = sum(Suicide))



#creating "current date"
ED %>%
    mutate(currentdate = Sys.Date()) %>%
      select (DOB_Cleaned, currentdate, Suicide) %>%
        mutate(ceiling( (ageINyears = (currentdate - DOB_Cleaned)/365.35)))
      
#assigning age to each ID
ED %>%
  mutate(currentdate = Sys.Date()) %>%
    select (DOB_Cleaned, currentdate, Suicide) %>%
      mutate(age = (currentdate - DOB_Cleaned)/365.35)

#to store new variable use "<-"
ED <- ED %>%
    mutate(currentdate = Sys.Date()) %>%
        mutate(age = ceiling((currentdate - DOB_Cleaned)/365.25))

#data.frame(tail(ED))
ED <- ED %>% mutate(agecategory = 
                        ifelse(age %in% 0:15, "Children_Adolescents", 
                        ifelse(age %in% 16:25, "Youth",
                        ifelse(age %in% 26:50, "Adult", 
                        ifelse(age %in% 51:200, "OlderAdult", "unknown")))))

#select age groups and suicide
ED %>%
  select(agecategory, Suicide) %>%
    group_by(agecategory) %>%
    tally(Suicide)

# i can save results of my table
res <- ED %>%
  select(Suicide, ethnicitycleaned) %>%
    group_by (ethnicitycleaned) %>%
      summarise(EthnicityCount = n(), sum = sum(Suicide))
#save results as csv
#write.csv(res, file="res.csv")

#to view results in the browser tab
#View(res)
      
#to view results in console 
#res

#to view results in console; another way
print(res)


 # this bit does counts of Suicide by each ethnic group
  ED %>%
  select(Suicide, ethnicitycleaned) %>%
  group_by (ethnicitycleaned) %>%
  tally()

  
#ETHNIC density scores
ED <- ED %>% 
  #Rename each ethnic density column
      rename(WhiteBrit_ED  = G , WhiteIrish_ED = White_Irish_Percentage, OtherWhite_ED = White_Other_White_GypsyIrishTrav, 
             WhiteBlackCaribbean_ED = P, WhiteBlackAfrican_ED = R, WhiteAsian_ED = T, OtherMixed_ED = V, BritIndian_ED = Asian_Asian_British_Indian_Perce, 
             BritPakistani_ED = Asian_Asian_British_Pakistani_Pe, BritBangladeshi_ED = AB, BritChinese_ED = Asian_Asian_British_Chinese_Perc, 
             OtherAsian_ED = Asian_Asian_British_OtherAsian_P, African_ED = AH, Caribbean_ED =  AJ, OtherBlack_ED = AL) %>%
  #get rid of ethnic density counts columns
            select(-All_Usual_Residents, -White_English_Welsh_Scottish_Nor, -White_Irish_Count, -White_Gypsy_Irish_Traveller_Coun, 
                   -White_Gypsy_Irish_Traveller_Perc, -White_Other_White_Count, -White_Other_White_Percentage, -Mixed_Multiple_Ethnic_Groups_Whi, 
                   -S, -Mixed_Multiple_Ethnic_Groups_Oth,
                   -Asian_Asian_British_Indian_Count, -Asian_Asian_British_Pakistani_Co, 
                   -Asian_Asian_British_Bangladeshi_, -Asian_Asian_British_Chinese_Coun, 
                   -Asian_Asian_British_OtherAsian_C,
                   -Black_African_Caribbean_BlackBr, -Black_African_Caribbean_Black_Br,  -AK, 
                   -Other_Ethnic_Group_Arab_Count, -Other_Ethnic_Group_Arab_Percenta, -Other_Ethnic_Group_AnyOtherEthni, -AP) 

#checking new vars
#data.frame(tail(ED))


#assign (and store) each ID with their own ethnic density percentage based on their ethnicity
#e.g. if ID is caribbean, then assign Caribbean_ED value.
ED <- ED %>% mutate(ethnicdensityscore = ifelse(ethnicitycleaned == "British (A)", WhiteBrit_ED, 
                          ifelse(ethnicitycleaned == "African (N)", African_ED, 
                          ifelse(ethnicitycleaned == "Irish (B)", WhiteIrish_ED, 
                          ifelse(ethnicitycleaned == "Any other Asian background (L)", OtherAsian_ED, 
                          ifelse(ethnicitycleaned == "Any other black background (P)", OtherBlack_ED,
                          ifelse(ethnicitycleaned == "Any other mixed background (G)", OtherMixed_ED,
                          ifelse(ethnicitycleaned == "Any other white background (C)", OtherWhite_ED,
                          ifelse(ethnicitycleaned == "Bangladeshi (K)", BritBangladeshi_ED, 
                          ifelse(ethnicitycleaned == "Caribbean (M)", Caribbean_ED,
                          ifelse(ethnicitycleaned == "Chinese (R)", BritChinese_ED,
                          ifelse(ethnicitycleaned == "Indian (H)", BritIndian_ED,
                          ifelse(ethnicitycleaned == "Pakistani (J)", BritPakistani_ED,
                          ifelse(ethnicitycleaned == "White and Asian (F)", WhiteAsian_ED,
                          ifelse(ethnicitycleaned == "White and Black African (E)", WhiteBlackAfrican_ED,
                          ifelse(ethnicitycleaned == "White and black Caribbean (D)", WhiteBlackCaribbean_ED,  " " )))))))))))))))) %>%
              mutate(ethnicdensityscore = as.numeric(ethnicdensityscore)) %>%
             group_by(ethnicitycleaned)
  
#to summarise ethnic density(ED) scores  
  ED_summarise_output <- ED %>% summarise(Min = min(ethnicdensityscore, na.rm=TRUE),
            Median = median(ethnicdensityscore, na.rm=TRUE),
            Mean = mean(ethnicdensityscore, na.rm=TRUE),
            Var = var(ethnicdensityscore, na.rm=TRUE),
            SD = sd(ethnicdensityscore, na.rm=TRUE),
            Max = max(ethnicdensityscore, na.rm=TRUE),
            N = n())
  
print(ED_summarise_output)
