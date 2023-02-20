library(tidyverse)
library(dplyr)
df <- read_delim("data/gapminder.csv")



#2. (2pt) Load data. How many rows/columns do we have?
df <- read_delim("data/gapminder.csv")
x <- nrow(df)
y <- ncol(df)

cat("It has", x, "rows, and",y, "columns")

#3. (2pt) Print a small sample of data. Does it look OK?
df %>% 
  sample_n(10)

#2 Descriptive statistics (15pt)

#1. (3pt) How many countries are there in the dataset? Analyze all three: iso3, iso2 and name.
count(unique(df['iso3']))
count(unique(df['iso2']))
count(unique(df['name']))

#2. If you did this correctly, you saw that there are more names than iso-2 codes, and there are
#even more iso3 -codes. What is going on? Can you find it out?


#(a) (5pt) Find how many names are there for each iso-2 code. Are there any iso-2 codes that
#correspond to more than one name? What are these countries?
df %>% 
   group_by(iso2) %>% 
   summarise(number_count = n_distinct(name)) %>%
   select(iso2,number_count) %>%
   arrange(desc(number_count)) %>%
   print()

df %>% 
   filter(is.na(name)) %>% 
   distinct(iso2)
#make it display name count per iso2

#(b) (5pt) Now repeat the same for name and iso3-code. Are there country names that have
#more than one iso3-code? What are these countries?
#Hint: two of these entitites are CHANISL and NLD CURACAO.

df %>% 
   group_by(name) %>% 
   summarise(name_count = n_distinct(iso3)) %>%
   select(name,name_count) %>%
   arrange(desc(name_count)) %>%
   print()
   
df %>% 
   filter(is.na(name)) %>% 
   distinct(iso3)
#There four contries names that are repeated more than once and they are :
#CHANISL    
#GBM        
#KOS        
#NLD_CURACAO

#3. (2pt) What is the minimum and maximum year in these data?
df %>% 
  filter(!is.na(time),!is.na(time)) %>% 
  summarise(maximum_year = max(time), minimum_year = min(time)) 

#3 CO2 emissions (30pt)
#Next, let’s analyze CO2 emissions.

#1. (2pt) How many missing co2 emissions are there for each year? Analyze both missing CO2
#and co2_PC. Which years have most missing data?
df %>%
    filter(is.na(co2),is.na(co2_PC)) %>%
    group_by(time) %>% 
    summarise(years= n()) %>%
    arrange(-years) %>% 
    head(3)

#The three years with the most missing values are 2017, 2018, 2019. 

#2. (5pt) Make a plot of total CO2 emissions over time for the U.S, China, and India. Add a few
#more countries of your choice. Explain what do you see.
#xxxx

country <- c("United States of America", "China")

df %>% 
    #filter(name == "United States of America") %>%
    #filter(name == "Austria") %>%
    #filter(name %in%("Austria")) %>% 
    filter(name %in% country %>%
    group_by(name, time) %>% 
    summarise(t_co2 = sum(co2)) %>% 
    ggplot(aes(x = time, y = t_co2, color = name)) +
    geom_line() +
    labs(title = "Total CO2 Emissions Over Time", x = "Year", y = "Total CO2 Emissions", color = "Country")
    
 ggplot(aes(x = GDP_PC, y = lifeExpectancy, col= totalPopulation) +
     geom_point()
#3. (5pt) Now let’s analyze the CO2 emissions per capita (co2_PC ). Make a similar plot of the
#same countries. What does this figure suggest?


#4. (6pt) Compute average CO2 emissions per capita across the continents (assume region is the
#same as continent). Comment what do you see.
#Note: just compute averages over countries and ignore the fact that countries are of different
#size.
#Hint: Americas 2016 should be 4.80.

df %>% 
   filter(!is.na(co2_PC)) %>%
   group_by(region) %>% 
   summarise(avg_co2_PC = mean(co2_PC)) %>% 
   print()
 




#5. (7pt) Make a barplot where you show the previous results–average CO2 emissions per capita
#across continents in 1960 and 2016.
#Hint: it should look something along these lines:



#6. Which countries are the three largest, and three smallest CO2 emitters (in terms of CO2 per
#capita) in 2019 for each continent? (Assume region is continent).

df %>% 
    filter(is.na(co2_PC)) %>%
    filter(time == "2019") %>%
    group_by(region) %>% 
    mutate(rank = rank(co2)) %>% 
    ungroup()%>% 
    filter(rank <= 3 | rank >= n() - 2) %>% 
    arrange(region, rank)
    
#America:
df %>% 
   filter(region == "Americas") %>% 
   filter(time == "2019") %>% 
   group_by(region) %>% 
   mutate(rank = rank(co2_PC)) %>%
   filter(rank(rank) <= 3 | rank >= n() -2) %>% 
   select(name, region, rank) %>% 
   arrange(rank)  


#take 2   
df %>% 
    filter(time == "2019") %>% 
    group_by(region) %>% 
    arrange(co2_PC) %>% 
    slice_head(n = 3) %>% 
    select(region, name, co2_PC)


df %>% 
    filter(time == "2019") %>% 
    group_by(region) %>% 
    arrange(co2_PC) %>% 
    slice_tail(n = 3) %>% 
    select(region, name, co2_PC)
#Asia:
df %>% 
   filter(is.na(co2_PC)) %>%
   filter(region == "Asia") %>% 
   filter(time == "2019") %>% 
   group_by(name) %>% 
   summarise(avg_co2_PC = mean(co2_PC)) %>% 
   arrange(avg_co2_PC) %>% 
   head(3) %>%
   pull(name)
#Oceania
df %>% 
   filter(is.na(co2_PC)) %>%
   filter(region == "Oceania") %>% 
   filter(time == "2019") %>% 
   group_by(name) %>% 
   summarise(avg_co2_PC = mean(co2_PC)) %>% 
   arrange(avg_co2_PC) %>% 
   head(3) %>%
   pull(name)
#Europe
df %>% 
   filter(is.na(co2_PC)) %>%
   filter(region == "Europe") %>% 
   filter(time == "2019") %>% 
   group_by(name) %>% 
   summarise(avg_co2_PC = mean(co2_PC)) %>% 
   arrange(avg_co2_PC) %>% 
   head(3) %>%
   pull(name)
#Africa
df %>% 
   filter(is.na(co2_PC)) %>%
   filter(region == "Africa") %>% 
   filter(time == "2019") %>% 
   group_by(name) %>% 
   summarise(avg_co2_PC = mean(co2_PC)) %>% 
   arrange(avg_co2_PC) %>% 
   head(3) %>%
   pull(name)
   
#smallest

df %>% 
   filter(is.na(co2_PC)) %>%
   filter(region == "Americas") %>% 
   filter(time == "2019") %>% 
   group_by(name) %>% 
   summarise(avg_co2_PC = mean(co2_PC)) %>% 
   arrange(avg_co2_PC) %>% 
   tail(3) %>%
   pull(name)
#Asia:
df %>% 
   filter(is.na(co2_PC)) %>%
   filter(region == "Asia") %>% 
   filter(time == "2019") %>% 
   group_by(name) %>% 
   summarise(avg_co2_PC = mean(co2_PC)) %>% 
   arrange(avg_co2_PC) %>% 
   tail(3) %>%
   pull(name)
#Oceania
df %>% 
   filter(is.na(co2_PC)) %>%
   filter(region == "Oceania") %>% 
   filter(time == "2019") %>% 
   group_by(name) %>% 
   summarise(avg_co2_PC = mean(co2_PC)) %>% 
   arrange(avg_co2_PC) %>% 
   tail(3) %>%
   pull(name)
#Europe
df %>% 
   filter(is.na(co2_PC)) %>%
   filter(region == "Europe") %>% 
   filter(time == "2019") %>% 
   group_by(name) %>% 
   summarise(avg_co2_PC = mean(co2_PC)) %>% 
   arrange(avg_co2_PC) %>% 
   tail(3) %>%
   pull(name)
#Africa
df %>% 
   filter(is.na(co2_PC)) %>%
   filter(region == "Africa") %>% 
   filter(time == "2019") %>% 
   group_by(name) %>% 
   summarise(avg_co2_PC = mean(co2_PC)) %>% 
   arrange(avg_co2_PC) %>% 
   tail(3) %>%
   pull(name)
   
   
   
   
   
   
   df %>% 
   filter(!is.na(co2_PC)) %>% 
   group_by(region) %>% 
   summarise(avg_co2_PC = mean(co2_PC)) %>% 
   arrange(-avg_co2_PC) %>% 
   tail(3) %>% 
   pull(region)
   
   
#Let’s look at GDP per capita (GDP_PC ).
#1. (8pt) Make a scatterplot of GDP per capita versus life expectancy by country, using data for
#1960. Make the point size dependent on the country size, and color those according to the
#continent. Feel free to adjust the plot in other ways to make it better.
#Comment what do you see there.
df %>% 
     filter(!is.na(GDP_PC)) %>% 
     ggplot(aes(x = GDP_PC, y = lifeExpectancy, col= totalPopulation) +
     geom_point()
    
    
    
    ggplot(df, aes(x=df$GDP_PC, y = lifeExpectancy)) +geom_point()
#2. (4pt) Make a similar plot, but this time use 2019 data only.

#3. (6pt) Compare these two plots and comment what do you see. How has world developed
#through the last 60 years?

#4. (6pt) Compute the average life expectancy for each continent in 1960 and 2019. Do the results
#fit with what do you see on the figures?
#Note: here as average I mean just average over countries, ignore the fact that countries are of
#different size.

#1960
df %>% 
   filter(!is.na(lifeExpectancy)) %>% 
   filter(!grepl('1960', time)) %>% 
   group_by(region) %>% 
   summarise(avg_le = mean(lifeExpectancy))

#2019   
df %>% 
   filter(!is.na(lifeExpectancy)) %>% 
   filter(!grepl('2019', time)) %>% 
   group_by(region) %>% 
   summarise(avg_le = mean(lifeExpectancy))   


#5. (8pt) Compute the average LE growth from 1960-2019 across the continents. Show the results
#in the order of growth. Explain what do you see.
#Hint: these data (data in long form) is not the simplest to compute growth. But you may
#want to check out the lag() function. And do not forget to group data by continent when
#using lag(), otherwise your results will be messed up! See https://faculty.washington.
#edu/otoomet/info201-book/dplyr.html#dplyr-helpers-compute.
 #calc mean value of life ex
   #group_by region and time
   #summrise mean value
   #group_by region 
   #summarize growth
   #arrange the order

df %>% 
   filter(!is.na(lifeExpectancy)) %>%
   filter(!is.na(region)) %>% 
   group_by(region) %>%
   summarise(avgle = mean(lifeExpectancy)) %>%
   mutate(prev = lag(avgle), growth = avgle - prev) %>%
   #filter(!is.na(growth)) %>% 
   arrange(-growth)
   

   
   df %>% 
   filter(!is.na(lifeExpectancy)) %>%
   filter(time == "1960"| time == "2019" ) %>% 
   filter(!is.na(region)) %>% 
   group_by(region, time) %>%
   summarise(avgle = mean(lifeExpectancy)) %>%
   mutate(prev = lag(avgle), growth = avgle - prev) %>%
   arrange(-growth)
   
   
   








#6. (6pt) Show the histogram of GDP per capita for years of 1960 and 2019. Try to put both
#histograms on the same graph, see how well you can do it!




#7. (6pt) What was the ranking of US in terms of life expectancy in 1960 and in 2019? (When
#counting from top.)
#Hint: check out the function rank()!
#Hint2: 17 for 1960.
df %>% 
    filter(time == "1960") %>% 
    filter(!is.na(lifeExpectancy)) %>%
    filter(!is.na(region)) %>% 
    mutate(le_rank = rank(desc(lifeExpectancy))) %>% 
    select(name, le_rank, time,region) %>% 
    filter(name == "United States of America") %>% 
    print()
    

df %>% 
    filter(time == "2019") %>% 
    filter(!is.na(lifeExpectancy)) %>%
    filter(!is.na(region)) %>% 
    mutate(le_rank = rank(desc(lifeExpectancy))) %>% 
    select(name, le_rank, time, region) %>% 
    filter(name == "United States of America") %>% 
    print()    
   


#8. (6pt) If you did this correctly, then you noticed that US ranking has been falling quite a
#bit. But we also have more countries in 2019–what about the relative rank divided by the
#corresponding number of countries that have LE data in the corresponding year?
#Hint: 0.0904 for 1960.

df %>% 
    filter(time == "1960") %>% 
    filter(!is.na(lifeExpectancy)) %>%
    filter(!is.na(region)) %>% 
    mutate(le_rank = rank(desc(lifeExpectancy)), nc = n(), r_rank = le_rank/nc) %>% 
    select(name, le_rank, time, nc, r_rank) %>% 
    filter(name == "United States of America") %>% 
    print()



    
    
df %>% 
    filter(time == "2019") %>% 
    filter(!is.na(lifeExpectancy)) %>%
    filter(!is.na(region)) %>% 
    mutate(le_rank = rank(desc(lifeExpectancy)), nc = n(), r_rank = le_rank/nc) %>% 
    select(name, le_rank, time, nc, r_rank) %>% 
    filter(name == "United States of America") %>% 
    print()




