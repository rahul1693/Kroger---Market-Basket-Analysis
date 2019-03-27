
######################
# Importing Packages #
######################

library(haven)
library(dplyr)
library(tidyr)
library(fBasics)
library(ggplot2)
library(gridExtra)
library(knitr)
library(kableExtra)
library(wordcloud)


##################
# Importing Data #
##################

files <- list.files(path = "data",pattern = ".sas7bdat")

for (i in seq_along(files)) {
  
  df <- read_sas(paste0("data/",files[i]))
  assign(paste0(unlist(strsplit(files[i], "[.]"))[[1]]),df)
  rm(df)
  
}

########################
# Cleaning and Joining #
########################


causal_lookup$upc <- as.numeric(causal_lookup$upc)
product_lookup$upc <- as.numeric(product_lookup$upc)
transactions$upc <- as.numeric(transactions$upc)
causal_lookup$geography <- NULL

trx_prod <- merge(transactions, product_lookup, by = 'upc',
                  all.x = T)
trx_activity_product_lookup_store <- merge(trx_prod, store_lookup,
                                           by = 'store', all.x = T)

############################
# Imputing abnormal values #
############################

product_lookup_price <- trx_activity_product_lookup_store %>%
  group_by(commodity) %>%
  summarize(product_lookup_avg_price =
              sum(dollar_sales) / sum(units) )

trx_activity_product_lookup_store <- 
  trx_activity_product_lookup_store %>%
  left_join(product_lookup_price, by = "commodity") %>%
  mutate(imputed_dollar_sales = case_when(
    dollar_sales > 0 ~ dollar_sales,
    dollar_sales < 0 ~ units * product_lookup_avg_price
  ))

trx_activity_product_lookup_store$dollar_sales <- NULL

###############################
# Summary - numeric variables #
###############################


basic_stats <- basicStats(trx_activity_product_lookup_store[ , c("units",
                                                                 "imputed_dollar_sales") ])[
                                                                   c("Mean",
                                                                     "Stdev",
                                                                     "Median",
                                                                     "Minimum",
                                                                     "Maximum"),
                                                                   ]
#################################
# Summary - character variables #
#################################
char_var <- trx_activity_product_lookup_store %>%
  summarize(store = n_distinct(store),
            product_lookup = n_distinct(upc),
            household = n_distinct(household),
            basket = n_distinct(basket),
            coupon = n_distinct(coupon),
            commodity = n_distinct(commodity),
            brand = n_distinct(brand)) %>%
  gather(variable, number_of_levels)

#######################################
# Number of brands per each commodity #
#######################################

num_brands <- trx_activity_product_lookup_store %>%
  group_by(commodity) %>%
  summarize(no_distinct_brand = n_distinct(brand))

#######################
# Feature Engineering #
#######################

#######################
# Support Calculation #
#######################

trx_prod_support <-  as.data.frame(trx_prod %>%
                                     group_by(basket, commodity, brand) %>%
                                     summarize(num_units = sum(units)) %>%
                                     select( basket, commodity, brand,
                                             num_units) %>%
                                     group_by(commodity, brand) %>%
                                     summarize(sum = sum(num_units))) %>%
  mutate('support' = sum / sum(sum))

trx_prod1 <- left_join(trx_prod, trx_prod_support, by = c('commodity','brand') )


trx_prod1 <- trx_prod1[,c('basket','commodity','brand','support', 'units')]

#############################
# Creating Associated Pairs #
#############################

j = 1
name = c('pancake' , 'syrup' , 'pasta' , 'sauce')

for (i in c('pancake mixes' , 'syrups' , 'pasta' , 'pasta sauce'))
{
  assign(paste0(name[j]), trx_prod1[trx_prod1$commodity == i, ])
  
  j = j + 1
}

pancake2 <- inner_join(pancake, syrup, by = 'basket')
pasta2 <- inner_join(pasta, sauce, by = 'basket')

full <- rbind(pancake2, pasta2)

full <- mutate(full,
               'commoditybrand.x' = paste(commodity.x,brand.x,sep = "_"),
               'commoditybrand.y' = paste(commodity.y,brand.y,sep = "_"),
               'commoditybrand.xy' =
                 paste(commoditybrand.x, commoditybrand.y, sep = "_"),
               'units.xy' = min(units.x, units.y)
)



xy_sumofunits <- as.data.frame(full %>% group_by(commoditybrand.xy) %>%
                                 summarize(xUy = sum(units.xy)))

association <- left_join(full, xy_sumofunits, by = 'commoditybrand.xy')

trx_prod_double <- as.data.frame(inner_join(trx_prod, trx_prod, by = 'basket'))

trx_prod_double <-  trx_prod_double[trx_prod_double$product_description.x !=
                                      trx_prod_double$product_description.y, ]

########
# Lift #
########

association['support.xy'] <- association$xUy / (nrow(trx_prod_double))
association['lift'] <- association$support.xy / (association$support.x * 
                                                   association$support.y)

lift_ass <- unique(association[ ,c('commoditybrand.xy', 'lift')])

lift <- lift_ass[order(lift_ass$lift, decreasing = TRUE), ]

####################
# Popularity Index #
####################

popularity <- as.data.frame(trx_prod %>%
                              group_by(commodity, brand) %>%
                              summarize(total_sales = sum(dollar_sales), 
                                        num_house = 
                                          n_distinct(factor(household))) %>%
                              mutate('popularity.score' = 
                                       (num_house * 
                                          (total_sales ^ (1 / 3)) / 10 ^ 4))  %>% 
                              select(commodity, brand, num_house, 
                                     total_sales, popularity.score)) %>%
  group_by(commodity) %>%
  mutate(rank = 
           order(order(popularity.score, 
                       decreasing = TRUE)))


lift_table <- lift %>% separate(commoditybrand.xy, into = 
                                  c('commodity.x', 'brand.x',
                                    'commodity.y', 'brand.y'), sep = '_' ) %>% 
  mutate('brand_ass' = paste(brand.x, brand.y, sep = "&"))

##################
# Data Filtering #
##################


### Filter out rows with low sales


aux <- left_join(lift_table, popularity, by = 
                   c('commodity.x' = 'commodity', 'brand.x' = 'brand'))

lift_popularity <- left_join(aux, popularity, by =
                               c('commodity.y' = 'commodity', 
                                 'brand.y' = 'brand'))

lift_popularity_v2 <- as.data.frame(lift_popularity %>% 
                                      dplyr::filter(total_sales.x > 10000, 
                                                    total_sales.y > 10000, 
                                                    num_house.x > 1000, 
                                                    num_house.y > 1000))


lift_popularity_list <- lift_popularity_v2 %>% 
  select(commodity.x, 
         brand.x,
         commodity.y,
         brand.y,
         lift, 
         rank.x,
         rank.y,
         total_sales.x, 
         total_sales.y,
         num_house.x,
         num_house.y,
         brand_ass)

lift_popularity_list_2 <- lift_popularity_list %>% 
  select(-c(brand_ass))

##################
# Visualization #
##################

#### Bar Charts 

top10 <- as.data.frame(trx_prod_support %>%
                         arrange_( ~ desc(support)) %>%
                         group_by_( ~ commodity) %>%
                         do(head( ., n = 10)) )

mycols <- c("#D92121", "#21D921", "#FF9326", "#53cfff")

plot_bar <- function(i,k){
  top10 %>% 
    dplyr::filter(commodity == paste0(i)) %>%  
    ggplot(aes(x = reorder(brand, -support), y = support)) +
    geom_bar(stat = "identity", fill = mycols[k]) +
    xlab("Brand") +
    ylab("Support") +
    ggtitle(i) +
    theme(axis.text.x = element_text(angle = 90))
}


k = 1
brands <- c('pancake mixes','syrups', 'pasta', 'pasta sauce')

for (i in seq_along(brands)) {
  assign(paste0('p', k), plot_bar(brands[i], k))
  k = k + 1  
}

grid.arrange(arrangeGrob(p1, p2, p3, p4, ncol = 2, nrow = 2))

#### Word Cloud

pancake_cloud <- lift_popularity_list %>%  
  dplyr::filter(commodity.x == "pancake mixes") %>% 
  mutate(lift_aug = lift * 10000)

pasta_cloud <-  lift_popularity_list %>%
  dplyr::filter(commodity.x == "pasta") %>% 
  mutate(lift_aug = lift * 10000)



wordcloud(words = pancake_cloud$brand_ass, freq = pancake_cloud$lift_aug,
          scale = c(1, .5),
          max.words = 15, random.order = FALSE,
          rot.per = 0.35,colors = c("indianred1", "indianred2", 
                                    "indianred3", "indianred"))

wordcloud(words = pasta_cloud$brand_ass, freq = pasta_cloud$lift_aug,
          scale = c(1, .5),
          max.words = 15, random.order = FALSE,
          rot.per = 0.35,
          colors = c("lightsteelblue1","lightsteelblue2",
                     "lightsteelblue3","lightsteelblue"))

#### Heat Matrix 

ggplot(pancake_cloud, aes(brand.x, brand.y)) + 
  geom_tile(aes(fill = lift),  colour = "white") +
  scale_fill_gradient(low = "white",high = "steelblue") +
  geom_text(aes(label = round(lift, 1))) + 
  ggtitle('Pancake mix & Syrup') +
  xlab('Pancake Mix Brands') + 
  ylab('Syrup Brands') +
  theme(axis.text.x = element_text(angle = 90))


ggplot(pasta_cloud, aes(brand.x, brand.y)) + 
  geom_tile(aes(fill = lift),  colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  geom_text(aes(label = round(lift, 1))) + 
  ggtitle('Pasta & Pasta Sauce') +
  ylab('Pasta Brands') + 
  xlab('Pasta Sauce Brands') +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip()

