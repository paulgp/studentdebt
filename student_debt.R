library(tidyverse)

library(readxl)



data = read_excel("~/Downloads/student_debt_age_income.xlsx")

data = data %>% 
  gather(key = "age", value = "student_debt_avg", -income_quartile, -race, -variable) %>%
  filter(age == "age_all")

ggplot(data = data %>% filter(variable == "avg")) + 
  geom_col(aes(y=student_debt_avg, x = income_quartile, 
               group = race, fill = race), position="dodge") +
  theme_minimal() +
  scale_x_discrete(labels = c("Bottom 25%", "Second 25%",
                              "Third 25%", "Top 25%", "Overall")) +
  scale_fill_viridis_d() + 
  labs(x = "Income Distribution",
       y = "",
       subtitle = "Average Student Debt",
       fill = "Race of debt holder",
       caption = "Source: Survey of Consumer Finances (2016,2019 data)") +
  theme(plot.title.position = "plot",
        text = element_text(size=30),
        legend.position = c(0.3, 0.8)) +
  annotate("text", x = 4.25, y=25000,label = "Black Americans\nin top 25% of income\nhave an average of \n27k in student debt", 
           color = viridis::viridis(3)[2], hjust = 0)

ggplot(data = data %>% filter(variable == "share_above_10k")) + 
  geom_col(aes(y=student_debt_avg, x = income_quartile, 
               group = race, fill = race), position="dodge") +
  theme_minimal() +
  scale_x_discrete(labels = c("Bottom 25%", "Second 25%",
                              "Third 25%", "Top 25%", "Overall")) +
  scale_fill_viridis_d() + 
  labs(x = "Income Distribution",
       y = "",
       subtitle = "Share with Student Debt greater than 10k",
       fill = "Race of debt holder",
       caption = "Source: Survey of Consumer Finances (2016,2019 data)") +
  theme(plot.title.position = "plot",
        text = element_text(size=30),
        legend.position = c(0.3, 0.8)) 


ggplot(data = data %>% filter(variable == "share_above_20k")) + 
  geom_col(aes(y=student_debt_avg, x = income_quartile, 
               group = race, fill = race), position="dodge") +
  theme_minimal() +
  scale_x_discrete(labels = c("Bottom 25%", "Second 25%",
                              "Third 25%", "Top 25%", "Overall")) +
  scale_fill_viridis_d() + 
  labs(x = "Income Distribution",
       y = "",
       subtitle = "Share with Student Debt greater than 20k",
       fill = "Race of debt holder",
       caption = "Source: Survey of Consumer Finances (2016,2019 data)") +
  theme(plot.title.position = "plot",
        text = element_text(size=30),
        legend.position = c(0.3, 0.8)) 



ggplot(data = data %>% filter(variable == "share_above_50k")) + 
  geom_col(aes(y=student_debt_avg, x = income_quartile, 
               group = race, fill = race), position="dodge") +
  theme_minimal() +
  scale_x_discrete(labels = c("Bottom 25%", "Second 25%",
                              "Third 25%", "Top 25%", "Overall")) +
  scale_fill_viridis_d() + 
  labs(x = "Income Distribution",
       y = "",
       subtitle = "Share with Student Debt greater than 50k",
       fill = "Race of debt holder",
       caption = "Source: Survey of Consumer Finances (2016,2019 data)") +
  theme(plot.title.position = "plot",
        text = element_text(size=30),
        legend.position = c(0.3, 0.8)) 

data$income_quartile = factor(data$income_quartile, labels = c("Bottom 25%", "Second 25%",
                                                               "Third 25%", "Top 25%", "Overall"))

ggplot(data = data %>% filter(income_quartile == "Overall" & variable != "avg" & variable != "N") %>%
         mutate(xvar = case_when(variable == "share_above_0" ~ 0,
                                 variable == "share_above_10k" ~ 10000,
                                 variable == "share_above_20k" ~ 20000,
                                 variable == "share_above_50k" ~ 50000))) + 
  geom_line(aes(y=student_debt_avg, x = xvar, 
               color = race)) +
  geom_point(aes(y=student_debt_avg, x = xvar, 
                 color = race)) +
  theme_minimal() +
  scale_color_viridis_d() + 
  scale_x_continuous(labels=scales::dollar_format()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2), limits = c(0,0.45)) +
  labs(x = "Student Debt Amount",
       y = "",
       subtitle = "Share with Student Debt greater than X",
       color = "Race of debt holder",
       caption = "Source: Survey of Consumer Finances (2016,2019 data)") +
  theme(plot.title.position = "plot",
        text = element_text(size=30),
        legend.position = c(0.7, 0.8)) 



ggplot(data = data %>% filter(income_quartile != "Overall" & variable != "avg" & variable != "N") %>%
         mutate(xvar = case_when(variable == "share_above_0" ~ 0,
                                 variable == "share_above_10k" ~ 10000,
                                 variable == "share_above_20k" ~ 20000,
                                 variable == "share_above_50k" ~ 50000))) + 
  geom_line(aes(y=student_debt_avg, x = xvar, 
                color = race)) +
  geom_point(aes(y=student_debt_avg, x = xvar, 
                color = race)) +
  theme_minimal() +
  scale_color_viridis_d() + 
  scale_x_continuous(labels=scales::dollar_format()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2), limits = c(0,0.45)) +
  facet_wrap(~income_quartile) +
  labs(x = "Student Debt Amount",
       y = "",
       subtitle = "Share with Student Debt greater than X, by income quartile",
       color = "Race of debt holder",
       caption = "Source: Survey of Consumer Finances (2016,2019 data)") +
  theme(plot.title.position = "plot",
        text = element_text(size=30),
        legend.text = element_text(size=22),
        legend.title = element_text(size=22),
        legend.position = c(0.2, 0.9)) 


#Dollars given
N_var = data %>% filter(variable == "N") %>%
  rename(N = student_debt_avg) %>% select(-variable)
share_pos = data %>% filter(variable == "share_above_0") %>%
  rename(share_pos = student_debt_avg) %>% select(-variable)

share_forgive = data %>% filter( variable != "avg" & variable != "N") %>%
  left_join(N_var) %>%
  left_join(share_pos) %>%
  group_by(race, income_quartile) %>%
  mutate(dollars_spent = 10000* (share_pos-student_debt_avg)*N) %>% ungroup()

share_forgive = share_forgive %>% left_join(share_forgive %>% filter(race == "all")  %>% 
                                              select(-race, -N, -share_pos,-student_debt_avg) %>%
                                              rename(total_dollars = dollars_spent )) %>%
  mutate(share_overall = dollars_spent / total_dollars)

ggplot(data = share_forgive %>% filter(income_quartile != "Overall") %>%
                                         filter(race == "black" | race == "white") %>%
         mutate(xvar = case_when(variable == "share_above_0" ~ 0,
                                 variable == "share_above_10k" ~ 10000,
                                 variable == "share_above_20k" ~ 20000,
                                 variable == "share_above_50k" ~ 50000)) %>%
         filter(xvar != 0)) + 
  geom_line(aes(y=share_overall, x = xvar, 
                color = race)) +
  geom_point(aes(y=share_overall, x = xvar, 
                 color = race)) +
  theme_minimal() +
  scale_color_viridis_d() + 
  scale_x_continuous(labels=scales::dollar_format()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
  facet_wrap(~income_quartile) +
  labs(x = "Student Debt Amount",
       y = "",
       subtitle = "Share of Forgiveness dollars for Student Debt cutoffs of X",
       color = "Race of debt holder",
       caption = "Source: Survey of Consumer Finances (2016,2019 data)") +
  theme(plot.title.position = "plot",
        text = element_text(size=30),
        legend.position = c(0.2, 0.9)) 



