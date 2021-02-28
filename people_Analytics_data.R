
library(peopleanalyticsdata)
library(dplyr)
library(ggplot2)




data(package = "peopleanalyticsdata")

head(salespeople)
summary(salespeople)
# reduce to complete cases to remove NAs
salespeople <- salespeople %>% 
  dplyr::filter(complete.cases(.))


# high performer sales
high <- salespeople %>% 
  dplyr::filter(performance == 4) %>% 
  dplyr::pull(sales)
# low performer sales
low <- salespeople %>% 
  dplyr::filter(performance == 1) %>% 
  dplyr::pull(sales)
# mean difference
(mean_diff <- mean(high) - mean(low))

# standard error
(se <- sqrt(sd(high)^2/length(high) + sd(low)^2/length(low)))

(g <- ggplot(data.frame(x = c(-5, 5)), aes(x = x)) +
    stat_function(fun = dt, args = list(df = 100.98), color = "blue") +
    geom_vline(xintercept = -mean_diff/se, linetype = "dashed", color = "red") +
    labs(x = "Standard errors around sample mean difference", y = "Density") +
    theme_minimal())


### Confidence intervals
# get se multiple for 95% confidence
(conf_mult <- qt(p = 0.05, df = 100.98))
# now we can create a lower bound for 95% confidence interval
(lower_bound <- mean_diff + conf_mult*se)


## We can be 95% confident that high performers generate higher sales



