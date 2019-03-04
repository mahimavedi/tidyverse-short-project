#%% Change working directory from the workspace root to the ipynb file location. Turn this addition off with the DataScience.changeDirOnImportExport setting
import os
try:
	os.chdir(os.path.join(os.getcwd(), '../../../../../Downloads'))
	print(os.getcwd())
except:
	pass

#%%
# Loading in required libraries
library(tidyverse)

# Reading in the Nobel Prize data with read_csv

# Taking a look at the first couple of winners
head(nobel)

# Counting the number of (possibly shared) Nobel Prizes handed
# out between 1901 and 2016
nobel %>% count(prize)

# Counting the number of prizes won by male and female recipients.
nobel %>%
    group_by(sex) %>%
count()

# Counting the number of prizes won by different nationalities.
nobel %>%
    group_by(birth_country) %>%
count() %>%
arrange(desc(n)) %>%
head(20)

# Calculating the proportion of USA born winners per decade
prop_usa_winners <- nobel %>% 
   mutate(usa_born_winner = birth_country == "United States of America") %>%
mutate(decade = floor(year/10)*10) %>%
group_by(decade)%>%
summarize(proportion = mean(usa_born_winner, na.rm = TRUE)) %>%
          print


#%%
# Setting the size of plots in this notebook
options(repr.plot.width=7, repr.plot.height=4)
ggplot(prop_usa_winners, aes(x = decade, y = proportion)) + geom_line() + 
geom_point()+ scale_y_continuous()
# Calculating the proportion of female laureates per decade
prop_female_winners <- nobel %>%
    mutate(female_winner = sex == "Female") %>%
mutate(decade = floor(year/10)*10) %>% 
group_by(decade, category) %>%
summarize(proportion = mean(female_winner, na.rm = TRUE)) 
 
ggplot(prop_female_winners, aes(x = decade, y = proportion, color = category)) + geom_line() + 
geom_point()+ scale_y_continuous()

# Picking out the first woman to win a Nobel Prize
nobel %>%
   filter(sex == "Female") %>%
top_n(1, desc(year))


#%%
# Selecting the laureates that have received 2 or more prizes.
nobel %>%
group_by(full_name) %>%
    count() %>%
    filter(n > 1)
    
    # Loading the lubridate package
library(lubridate)
# Calculating the age of Nobel Prize winners
nobel_age <- nobel %>%
    mutate(age = year - year(birth_date))
#dates <- as.Date(c("1985-04-02",  "1988-07-25"))
# year(dates)
## [1] 1985 1988

# Plotting the age of Nobel Prize winners
ggplot(nobel_age, aes(x = age, y = year)) + geom_point() + geom_smooth()
# Same plot as above, but faceted by the category of the Nobel Prize
ggplot(nobel_age, aes(x = age, y = year)) + geom_point() + geom_smooth(se = FALSE) + facet_wrap(~category)
# The oldest winner of a Nobel Prize as of 2016
nobel_age %>% top_n(1, age)

# The youngest winner of a Nobel Prize as of 2016
nobel_age %>% top_n(1, desc(age))


