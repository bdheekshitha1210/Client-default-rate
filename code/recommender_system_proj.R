# Marketing Recommender Systems Code
# Team 3

library(data.table)
library(reshape2)

# loading data ------------------------------------------------------------

transactions <- fread('Pernalonga/transaction_table.csv')
products <- fread('Pernalonga/product_table.csv')
clusters <- fread('customer_cluster.csv')

# merging tables together -------------------------------------------------

data <- merge(transactions, products, by='prod_id', all=TRUE)
data <- merge(data, clusters, by = 'cust_id', all = TRUE)


# data exploration for heineken and super bock -----------------------------
brands <- unique(data$brand_desc)

#category level profit
category_profit <- data[,sum(tran_prod_sale_amt), by = category_desc]
category_profit$V1[category_profit$category_desc =='CERVEJA C/ ALCOOL']/ sum(category_profit$V1)

# on all data
brands_profit <- data[,sum(tran_prod_sale_amt), by = brand_desc]
brands_profit$V1[brands_profit$brand_desc=='SUPER BOCK']
brands_profit$V1[brands_profit$brand_desc=='HEINEKEN']
brands_profit$V1[brands_profit$brand_desc=='SUPER BOCK'] - brands_profit$V1[brands_profit$brand_desc=='HEINEKEN']
brands_profit$V1[brands_profit$brand_desc=='SUPER BOCK'] / brands_profit$V1[brands_profit$brand_desc=='HEINEKEN']

brands_quantity <- data[,sum(tran_prod_sale_qty), by = brand_desc]
brands_quantity$V1[brands_quantity$brand_desc=='SUPER BOCK']
brands_quantity$V1[brands_profit$brand_desc=='HEINEKEN']
brands_quantity$V1[brands_quantity$brand_desc=='SUPER BOCK'] - brands_quantity$V1[brands_profit$brand_desc=='HEINEKEN']
brands_quantity$V1[brands_quantity$brand_desc=='SUPER BOCK'] / brands_quantity$V1[brands_profit$brand_desc=='HEINEKEN']

tot_customers <- data[,length(unique(cust_id)), by = brand_desc]
tot_customers$V1[tot_customers$brand_desc=='SUPER BOCK']
tot_customers$V1[tot_customers$brand_desc=='HEINEKEN']

discount_freq <- data[,sum(tran_prod_discount_amt != 0)/.N, by = brand_desc]
discount_freq$V1[discount_freq$brand_desc=='SUPER BOCK']
discount_freq$V1[discount_freq$brand_desc=='HEINEKEN']

discount_amt_per_sale <- data[,-1*sum(tran_prod_discount_amt)/sum(tran_prod_sale_amt), by = brand_desc]
discount_amt_per_sale$V1[discount_amt_per_sale$brand_desc=='SUPER BOCK']
discount_amt_per_sale$V1[discount_amt_per_sale$brand_desc=='HEINEKEN']

avg_price <- data[,sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty), by = brand_desc]
avg_price$V1[avg_price$brand_desc=='SUPER BOCK']
avg_price$V1[avg_price$brand_desc=='HEINEKEN']



# cluster 1 data below
brands_profit_c1 <- data[cluster==1,sum(tran_prod_sale_amt), by = brand_desc]
brands_profit_c1$V1[brands_profit_c1$brand_desc=='SUPER BOCK']
brands_profit_c1$V1[brands_profit_c1$brand_desc=='HEINEKEN']
brands_profit_c1$V1[brands_profit_c1$brand_desc=='SUPER BOCK'] - brands_profit_c1$V1[brands_profit_c1$brand_desc=='HEINEKEN']
brands_profit_c1$V1[brands_profit_c1$brand_desc=='SUPER BOCK'] / brands_profit_c1$V1[brands_profit_c1$brand_desc=='HEINEKEN']

brands_quantity_c1 <- data[cluster==1,sum(tran_prod_sale_qty), by = brand_desc]
brands_quantity_c1$V1[brands_quantity_c1$brand_desc=='SUPER BOCK']
brands_quantity_c1$V1[brands_quantity_c1$brand_desc=='HEINEKEN']
brands_quantity_c1$V1[brands_quantity_c1$brand_desc=='SUPER BOCK'] - brands_quantity_c1$V1[brands_profit_c1$brand_desc=='HEINEKEN']
brands_quantity_c1$V1[brands_quantity_c1$brand_desc=='SUPER BOCK'] / brands_quantity_c1$V1[brands_profit_c1$brand_desc=='HEINEKEN']

tot_customers_c1 <- data[cluster==1,length(unique(cust_id)), by = brand_desc]
tot_customers_c1$V1[tot_customers_c1$brand_desc=='SUPER BOCK']
tot_customers_c1$V1[tot_customers_c1$brand_desc=='HEINEKEN']

discount_freq_c1 <- data[cluster==1,sum(tran_prod_discount_amt != 0)/.N, by = brand_desc]
discount_freq_c1$V1[discount_freq_c1$brand_desc=='SUPER BOCK']
discount_freq_c1$V1[discount_freq_c1$brand_desc=='HEINEKEN']

discount_amt_per_sale_c1 <- data[cluster==1,-1*sum(tran_prod_discount_amt)/sum(tran_prod_sale_amt), by = brand_desc]
discount_amt_per_sale_c1$V1[discount_amt_per_sale_c1$brand_desc=='SUPER BOCK']
discount_amt_per_sale_c1$V1[discount_amt_per_sale_c1$brand_desc=='HEINEKEN']


avg_price_c1 <- data[cluster==1,sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty), by = brand_desc]
avg_price_c1$V1[avg_price_c1$brand_desc=='SUPER BOCK']
avg_price_c1$V1[avg_price_c1$brand_desc=='HEINEKEN']



# cluster 2 data below
brands_profit_c2 <- data[cluster==2,sum(tran_prod_sale_amt), by = brand_desc]
brands_profit_c2$V1[brands_profit_c2$brand_desc=='SUPER BOCK']
brands_profit_c2$V1[brands_profit_c2$brand_desc=='HEINEKEN']
brands_profit_c2$V1[brands_profit_c2$brand_desc=='SUPER BOCK'] - brands_profit_c2$V1[brands_profit_c2$brand_desc=='HEINEKEN']
brands_profit_c2$V1[brands_profit_c2$brand_desc=='SUPER BOCK'] / brands_profit_c2$V1[brands_profit_c2$brand_desc=='HEINEKEN']

brands_quantity_c2 <- data[cluster==2,sum(tran_prod_sale_qty), by = brand_desc]
brands_quantity_c2$V1[brands_quantity_c2$brand_desc=='SUPER BOCK']
brands_quantity_c2$V1[brands_quantity_c2$brand_desc=='HEINEKEN']
brands_quantity_c2$V1[brands_quantity_c2$brand_desc=='SUPER BOCK'] - brands_quantity_c2$V1[brands_profit_c2$brand_desc=='HEINEKEN']
brands_quantity_c2$V1[brands_quantity_c2$brand_desc=='SUPER BOCK'] / brands_quantity_c2$V1[brands_profit_c2$brand_desc=='HEINEKEN']

tot_customers_c2 <- data[cluster==2,length(unique(cust_id)), by = brand_desc]
tot_customers_c2$V1[tot_customers_c2$brand_desc=='SUPER BOCK']
tot_customers_c2$V1[tot_customers_c2$brand_desc=='HEINEKEN']

discount_freq_c2 <- data[cluster==2,sum(tran_prod_discount_amt != 0)/.N, by = brand_desc]
discount_freq_c2$V1[discount_freq_c2$brand_desc=='SUPER BOCK']
discount_freq_c2$V1[discount_freq_c2$brand_desc=='HEINEKEN']

discount_amt_per_sale_c2 <- data[cluster==2,-1*sum(tran_prod_discount_amt)/sum(tran_prod_sale_amt), by = brand_desc]
discount_amt_per_sale_c2$V1[discount_amt_per_sale_c2$brand_desc=='SUPER BOCK']
discount_amt_per_sale_c2$V1[discount_amt_per_sale_c2$brand_desc=='HEINEKEN']


avg_price_c2 <- data[cluster==2,sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty), by = brand_desc]
avg_price_c2$V1[avg_price_c2$brand_desc=='SUPER BOCK']
avg_price_c2$V1[avg_price_c2$brand_desc=='HEINEKEN']


# cluster 3 data below
brands_profit_c3 <- data[cluster==3,sum(tran_prod_sale_amt), by = brand_desc]
brands_profit_c3$V1[brands_profit_c3$brand_desc=='SUPER BOCK']
brands_profit_c3$V1[brands_profit_c3$brand_desc=='HEINEKEN']
brands_profit_c3$V1[brands_profit_c3$brand_desc=='SUPER BOCK'] - brands_profit_c3$V1[brands_profit_c3$brand_desc=='HEINEKEN']
brands_profit_c3$V1[brands_profit_c3$brand_desc=='SUPER BOCK'] / brands_profit_c3$V1[brands_profit_c3$brand_desc=='HEINEKEN']

brands_quantity_c3 <- data[cluster==3,sum(tran_prod_sale_qty), by = brand_desc]
brands_quantity_c3$V1[brands_quantity_c3$brand_desc=='SUPER BOCK']
brands_quantity_c3$V1[brands_quantity_c3$brand_desc=='HEINEKEN']
brands_quantity_c3$V1[brands_quantity_c3$brand_desc=='SUPER BOCK'] - brands_quantity_c3$V1[brands_profit_c3$brand_desc=='HEINEKEN']
brands_quantity_c3$V1[brands_quantity_c3$brand_desc=='SUPER BOCK'] / brands_quantity_c3$V1[brands_profit_c3$brand_desc=='HEINEKEN']

tot_customers_c3 <- data[cluster==3,length(unique(cust_id)), by = brand_desc]
tot_customers_c3$V1[tot_customers_c3$brand_desc=='SUPER BOCK']
tot_customers_c3$V1[tot_customers_c3$brand_desc=='HEINEKEN']

discount_freq_c3 <- data[cluster==3,sum(tran_prod_discount_amt != 0)/.N, by = brand_desc]
discount_freq_c3$V1[discount_freq_c3$brand_desc=='SUPER BOCK']
discount_freq_c3$V1[discount_freq_c3$brand_desc=='HEINEKEN']

discount_amt_per_sale_c3 <- data[cluster==3,-1*sum(tran_prod_discount_amt)/sum(tran_prod_sale_amt), by = brand_desc]
discount_amt_per_sale_c3$V1[discount_amt_per_sale_c3$brand_desc=='SUPER BOCK']
discount_amt_per_sale_c3$V1[discount_amt_per_sale_c3$brand_desc=='HEINEKEN']


avg_price_c3 <- data[cluster==3,sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty), by = brand_desc]
avg_price_c3$V1[avg_price_c3$brand_desc=='SUPER BOCK']
avg_price_c3$V1[avg_price_c3$brand_desc=='HEINEKEN']




# important stuff -----------------------------------------------------------

# getting the list of heineken products
heineken_products <- (unique(data$prod_id[data$brand_desc == 'HEINEKEN']))
heineken_products <- heineken_products[-2]

# all transactions involving super bock products
superbock <- data[data$brand_desc =='SUPER BOCK']

#list of customers who bought SB
superbock_cust <- unique(superbock$cust_id)

# filtered DF of all transactions involving customers who bought super bock
superbock_cust_purchases <- data[data$cust_id %in% superbock_cust]

# transactions of heineken bought by customers who also bought super bock
HSB <- superbock_cust_purchases[superbock_cust_purchases$brand_desc =='HEINEKEN']

# customer list of those who bought both
hsb_cust_list <- unique(HSB$cust_id)


# same as 'superbock' above
SB <- superbock_cust_purchases[superbock_cust_purchases$brand_desc =='SUPER BOCK']


# all heineken transactions
heineken <- data[data$brand_desc =='HEINEKEN']

heineken[,avg_unit_price := mean(prod_unit_price), by = prod_id]
avg_prices <- unique(heineken[,c('prod_id','avg_unit_price')])


# heineken customers broken down into predefined clusters from segmentation
heineken_cluster1<- heineken[heineken$cluster ==1]
heineken_cluster2<- heineken[heineken$cluster ==2]
heineken_cluster3<- heineken[heineken$cluster ==3]


# avg discount rate and total quantity for super bock customers
SB_discount_rates <- SB[, .(sum(tran_prod_sale_qty),(-1*sum(tran_prod_discount_amt)/sum(tran_prod_sale_amt))),by = cust_id]

# assigning buckets for SB customers 1-20 in 5 percentile buckets. e.g., 0-5% == bucket 1,...,95-100% == bucket 20
for (j in 1:dim(SB_discount_rates)[1]){
  for (i in 0:19){
    endpt1 <- 5*i/100
    endpt2 <- 5*(i+1)/100
    if ((SB_discount_rates$V2[j] >= endpt1) & (SB_discount_rates$V2[j] < endpt2)){
      SB_discount_rates$bucket[j] <- i+1
    }
  }
}

# avg discount rate and total quantity for super bock customers who also buy heineken
HSB_discount_rates <- HSB[, .(sum(tran_prod_sale_qty),(-1*sum(tran_prod_discount_amt)/sum(tran_prod_sale_amt))),by = cust_id]

# same as above for HSB customers
for (j in 1:dim(HSB_discount_rates)[1]){
  for (i in 0:19){
    endpt1 <- 5*i/100
    endpt2 <- 5*(i+1)/100
    if ((HSB_discount_rates$V2[j] >= endpt1) & (HSB_discount_rates$V2[j] < endpt2)){
      HSB_discount_rates$bucket[j] <- i+1
    }
  }
}


# adding customer cluster to HSB_discount_rates
HSB_discount_rates$cluster <- data$cluster[data$cust_id ==HSB_discount_rates$cust_id]



# not sure if needed ------------------------------------------------------

# 
# # discount_rate for super bock buyers and how many transactions at that rate
# test <- SB[, sum(tran_prod_sale_qty),by = -1*tran_prod_discount_amt/tran_prod_sale_amt]
# sum(test$V1[test$tran_prod_discount_amt >.5])
# sum(test$V1)
# test$proportion <- test$V1/sum(test$V1)
# 
# 
# # discount_rate for heineken buyers (who also bought super bock) and how many transactions at that rate
# test_h <- HSB[, sum(tran_prod_sale_qty),by = -1*tran_prod_discount_amt/tran_prod_sale_amt]
# sum(test_h$V1[test_h$tran_prod_discount_amt >.5])
# sum(test_h$V1)
# 
# superbock_cust_purchases1 <- merge(superbock_cust_purchases, test_new, by = 'cust_id')
# test_mat <- dcast(superbock_cust_purchases1, cust_id ~ bucket, value.var = 'tran_prod_sale_amt')

#superbock_cust_purchases$discount_rate <- superbock_cust_purchases$tran_prod_discount_amt/superbock_cust_purchases$tran_prod_sale_amt


# finding top copurchased products ---------------------------------------------------------


copurchase <- data.table(superbock_cust_purchases$brand_desc)
copurchase[, freq := .N, by = V1]
copurchase <- unique(copurchase)
  

# looked at copurchase df and found the products below to be top 5 copurchased brands 
# excluding private label, no label, and fruits and vegetables
top5_prod <- c('PERECÍVEIS CARNE','MIMOSA','COMPAL','TERRA NOSTRA','ACTIVIA')
# steak, milk, soft drinks, cheese, and yogurt


# cluster 1 ---------------------------------------------------------------

# initializing super bock customers in cluster 1
cluster1_sb <- superbock_cust_purchases[superbock_cust_purchases$cluster == 1]

#
cluster1_sb <- merge(cluster1_sb, SB_discount_rates, by = 'cust_id', all.x = TRUE)
c1_bucket <- data.frame(cluster1_sb$cust_id,cluster1_sb$bucket)
c1_bucket <- unique(c1_bucket)


c1_test <- cluster1_sb[cluster1_sb$brand_desc %in% top5_prod]
c1_mat <- dcast(c1_test, cust_id ~ brand_desc,fun.aggregate = sum, value.var = 'tran_prod_sale_amt')
c1_mat$bucket <- c1_bucket$cluster1_sb.bucket
c1_mat$heineken_product_1 <- ifelse(c1_mat$cust_id %in% unique(heineken_cluster1$cust_id[heineken_cluster1$prod_id == heineken_products[1]]),1,0)
c1_mat$heineken_product_2 <- ifelse(c1_mat$cust_id %in% unique(heineken_cluster1$cust_id[heineken_cluster1$prod_id == heineken_products[2]]),1,0)

c1_mat$heineken_product_3 <- ifelse(c1_mat$cust_id %in% unique(heineken_cluster1$cust_id[heineken_cluster1$prod_id == heineken_products[3]]),1,0)

# initializing different matrix for each of the three Heineken products for 
# each matrix. This will allow us to make predictions at the product level for Heineken
cp_c1_p1 <- data.frame(c1_mat$cust_id)
cp_c1_p2 <- data.frame(c1_mat$cust_id)
cp_c1_p3 <- data.frame(c1_mat$cust_id)
colnames(cp_c1_p1) <- c('cust_id')
colnames(cp_c1_p2) <- c('cust_id')
colnames(cp_c1_p3) <- c('cust_id')
prob_h_p1 <- sum(c1_mat$heineken_product_1)/length(c1_mat$heineken_product_1)
prob_h_p2 <- sum(c1_mat$heineken_product_2)/length(c1_mat$heineken_product_2)
prob_h_p3 <- sum(c1_mat$heineken_product_3)/length(c1_mat$heineken_product_3)

for (k in 1:3){
  ifelse(k==1, ((cp_c1 <- cp_c1_p1) & (prob_h <- prob_h_p1)), ifelse(k==2, ((cp_c1 <- cp_c1_p2) & (prob_h <- prob_h_p2)), ((cp_c1 <- cp_c1_p3) & (prob_h <- prob_h_p3))))
  for (i in 1:20) {
    var <- as.character(i)
    temp_mat <- c1_mat[c1_mat$bucket == i, ]
    if (dim(temp_mat)[1]!=0){
      mean_y_vec <- lapply(temp_mat[temp_mat[paste('heineken','product',k,sep='_')] == 1, ], mean)
      mean_y_vec <- mean_y_vec[-c(1, 7, 8)]
      mean_n_vec <- lapply(temp_mat[temp_mat[paste('heineken','product',k,sep='_')] == 0, ], mean)
      mean_n_vec <- mean_n_vec[-c(1, 7, 8)]
      sd_y_vec <- lapply(temp_mat[temp_mat[paste('heineken','product',k,sep='_')] == 1, ], sd)
      sd_y_vec <- sd_y_vec[-c(1, 7, 8)]
      sd_n_vec <- lapply(temp_mat[temp_mat[paste('heineken','product',k,sep='_')] == 0, ], sd)
      sd_n_vec <- sd_n_vec[-c(1, 7, 8)]
      for (j in 1:dim(temp_mat)[1]) {
        p_h_given_x <- prob_h *
          dnorm(temp_mat[[2]][j], mean = mean_y_vec[[1]], sd = sd_y_vec[[1]]) *
          dnorm(temp_mat[[3]][j], mean = mean_y_vec[[2]], sd = sd_y_vec[[2]]) *
          dnorm(temp_mat[[4]][j], mean = mean_y_vec[[3]], sd = sd_y_vec[[3]]) *
          dnorm(temp_mat[[5]][j], mean = mean_y_vec[[4]], sd = sd_y_vec[[4]]) *
          dnorm(temp_mat[[6]][j], mean = mean_y_vec[[5]], sd = sd_y_vec[[5]])
        p_no_h_given_x <- (1 - prob_h) *
          dnorm(temp_mat[[2]][j], mean = mean_n_vec[[1]], sd = sd_n_vec[[1]]) *
          dnorm(temp_mat[[3]][j], mean = mean_n_vec[[2]], sd = sd_n_vec[[2]]) *
          dnorm(temp_mat[[4]][j], mean = mean_n_vec[[3]], sd = sd_n_vec[[3]]) *
          dnorm(temp_mat[[5]][j], mean = mean_n_vec[[4]], sd = sd_n_vec[[4]]) *
          dnorm(temp_mat[[6]][j], mean = mean_n_vec[[5]], sd = sd_n_vec[[5]])
        value <- ifelse(!is.na(p_h_given_x/(p_h_given_x + p_no_h_given_x)), p_h_given_x/(p_h_given_x + p_no_h_given_x),0)
        cp_c1[cp_c1$cust_id == temp_mat[[1]][j], var] <- value
      }
    }
    else{ cp_c1[,var] = NA
    }
  }
  ifelse(k==1,cp_c1_p1 <- cp_c1, ifelse(k==2,cp_c1_p2 <- cp_c1,cp_c1_p3 <- cp_c1))
}


# cluster 2 scores --------------------------------------------------------

# same as above, but for cluster 2

cluster2_sb <- superbock_cust_purchases[superbock_cust_purchases$cluster == 2]


cluster2_sb <- merge(cluster2_sb, SB_discount_rates, by = 'cust_id', all.x = TRUE)
#cluster2_sb$heineken <- cluster2_sb[,ifelse(brand_desc =='HEINEKEN',1,0),by = cust_id]
c2_bucket <- data.frame(cluster2_sb$cust_id,cluster2_sb$bucket)
c2_bucket <- unique(c2_bucket)


c2_test <- cluster2_sb[cluster2_sb$brand_desc %in% top5_prod]
c2_mat <- dcast(c2_test, cust_id ~ brand_desc,fun.aggregate = sum, value.var = 'tran_prod_sale_amt')
c2_mat$bucket <- c2_bucket$cluster2_sb.bucket
c2_mat$heineken_product_1 <- ifelse(c2_mat$cust_id %in% unique(heineken_cluster2$cust_id[heineken_cluster2$prod_id == heineken_products[1]]),1,0)
c2_mat$heineken_product_2 <- ifelse(c2_mat$cust_id %in% unique(heineken_cluster2$cust_id[heineken_cluster2$prod_id == heineken_products[2]]),1,0)
c2_mat$heineken_product_3 <- ifelse(c2_mat$cust_id %in% unique(heineken_cluster2$cust_id[heineken_cluster2$prod_id == heineken_products[3]]),1,0)


cp_c2_p1 <- data.frame(c2_mat$cust_id)
cp_c2_p2 <- data.frame(c2_mat$cust_id)
cp_c2_p3 <- data.frame(c2_mat$cust_id)
colnames(cp_c2_p1) <- c('cust_id')
colnames(cp_c2_p2) <- c('cust_id')
colnames(cp_c2_p3) <- c('cust_id')
prob_h_p1 <- sum(c2_mat$heineken_product_1)/length(c2_mat$heineken_product_1)
prob_h_p2 <- sum(c2_mat$heineken_product_2)/length(c2_mat$heineken_product_2)
prob_h_p3 <- sum(c2_mat$heineken_product_3)/length(c2_mat$heineken_product_3)


for (k in 1:3){
  ifelse(k==1, ((cp_c2 <- cp_c2_p1) & (prob_h <- prob_h_p1)), ifelse(k==2, ((cp_c2 <- cp_c2_p2) & (prob_h <- prob_h_p2)), ((cp_c2 <- cp_c2_p3) & (prob_h <- prob_h_p3))))
  for (i in 1:20) {
    var <- as.character(i)
    temp_mat <- c2_mat[c2_mat$bucket == i, ]
    if (dim(temp_mat)[1]!=0){
      mean_y_vec <- lapply(temp_mat[temp_mat[paste('heineken','product',k,sep='_')] == 1, ], mean)
      mean_y_vec <- mean_y_vec[-c(1, 7, 8)]
      mean_n_vec <- lapply(temp_mat[temp_mat[paste('heineken','product',k,sep='_')] == 0, ], mean)
      mean_n_vec <- mean_n_vec[-c(1, 7, 8)]
      sd_y_vec <- lapply(temp_mat[temp_mat[paste('heineken','product',k,sep='_')] == 1, ], sd)
      sd_y_vec <- sd_y_vec[-c(1, 7, 8)]
      sd_n_vec <- lapply(temp_mat[temp_mat[paste('heineken','product',k,sep='_')] == 0, ], sd)
      sd_n_vec <- sd_n_vec[-c(1, 7, 8)]
      for (j in 1:dim(temp_mat)[1]) {
        p_h_given_x <- prob_h *
          dnorm(temp_mat[[2]][j], mean = mean_y_vec[[1]], sd = sd_y_vec[[1]]) *
          dnorm(temp_mat[[3]][j], mean = mean_y_vec[[2]], sd = sd_y_vec[[2]]) *
          dnorm(temp_mat[[4]][j], mean = mean_y_vec[[3]], sd = sd_y_vec[[3]]) *
          dnorm(temp_mat[[5]][j], mean = mean_y_vec[[4]], sd = sd_y_vec[[4]]) *
          dnorm(temp_mat[[6]][j], mean = mean_y_vec[[5]], sd = sd_y_vec[[5]])
        p_no_h_given_x <- (1 - prob_h) *
          dnorm(temp_mat[[2]][j], mean = mean_n_vec[[1]], sd = sd_n_vec[[1]]) *
          dnorm(temp_mat[[3]][j], mean = mean_n_vec[[2]], sd = sd_n_vec[[2]]) *
          dnorm(temp_mat[[4]][j], mean = mean_n_vec[[3]], sd = sd_n_vec[[3]]) *
          dnorm(temp_mat[[5]][j], mean = mean_n_vec[[4]], sd = sd_n_vec[[4]]) *
          dnorm(temp_mat[[6]][j], mean = mean_n_vec[[5]], sd = sd_n_vec[[5]])
        value <- ifelse(!is.na(p_h_given_x/(p_h_given_x + p_no_h_given_x)), p_h_given_x/(p_h_given_x + p_no_h_given_x),0)
        cp_c2[cp_c2$cust_id == temp_mat[[1]][j], var] <- value
      }
    }
    else{ cp_c2[,var] = NA
    }
  }
  ifelse(k==1,cp_c2_p1 <- cp_c2, ifelse(k==2,cp_c2_p2 <- cp_c2,cp_c2_p3 <- cp_c2))
}

# cluster 3 ---------------------------------------------------------------

# same as above but for cluster 3

cluster3_sb <- superbock_cust_purchases[superbock_cust_purchases$cluster == 3]


cluster3_sb <- merge(cluster3_sb, SB_discount_rates, by = 'cust_id', all.x = TRUE)
c3_bucket <- data.frame(cluster3_sb$cust_id,cluster3_sb$bucket)
c3_bucket <- unique(c3_bucket)

c3_test <- cluster3_sb[cluster3_sb$brand_desc %in% top5_prod]
c3_mat <- dcast(c3_test, cust_id ~ brand_desc,fun.aggregate = sum, value.var = 'tran_prod_sale_amt')
c3_mat<- merge(c3_mat, c3_bucket, by.x = 'cust_id',by.y = 'cluster3_sb.cust_id', all = TRUE)
colnames(c3_mat) <- c('cust_id','ACTIVIA','COMPAL','MIMOSA','PERECÍVEIS CARNE','TERRA NOSTRA','bucket')
c3_mat$heineken_product_1 <- ifelse(c3_mat$cust_id %in% unique(heineken_cluster3$cust_id[heineken_cluster3$prod_id == heineken_products[1]]),1,0)
c3_mat$heineken_product_2 <- ifelse(c3_mat$cust_id %in% unique(heineken_cluster3$cust_id[heineken_cluster3$prod_id == heineken_products[2]]),1,0)
c3_mat$heineken_product_3 <- ifelse(c3_mat$cust_id %in% unique(heineken_cluster3$cust_id[heineken_cluster3$prod_id == heineken_products[3]]),1,0)


cp_c3_p1 <- data.frame(c3_mat$cust_id)
cp_c3_p2 <- data.frame(c3_mat$cust_id)
cp_c3_p3 <- data.frame(c3_mat$cust_id)
colnames(cp_c3_p1) <- c('cust_id')
colnames(cp_c3_p2) <- c('cust_id')
colnames(cp_c3_p3) <- c('cust_id')
prob_h_p1 <- sum(c3_mat$heineken_product_1)/length(c3_mat$heineken_product_1)
prob_h_p2 <- sum(c3_mat$heineken_product_2)/length(c3_mat$heineken_product_2)
prob_h_p3 <- sum(c3_mat$heineken_product_3)/length(c3_mat$heineken_product_3)


for (k in 1:3){
  ifelse(k==1, ((cp_c3 <- cp_c3_p1) & (prob_h <- prob_h_p1)), ifelse(k==2, ((cp_c3 <- cp_c3_p2) & (prob_h <- prob_h_p2)), ((cp_c3 <- cp_c3_p3) & (prob_h <- prob_h_p3))))
  for (i in 1:20) {
    var <- as.character(i)
    temp_mat <- c3_mat[c3_mat$bucket == i, ]
    if (dim(temp_mat)[1]!=0){
      mean_y_vec <- lapply(temp_mat[temp_mat[paste('heineken','product',k,sep='_')] == 1, ], mean)
      mean_y_vec <- mean_y_vec[-c(1, 7, 8)]
      mean_n_vec <- lapply(temp_mat[temp_mat[paste('heineken','product',k,sep='_')] == 0, ], mean)
      mean_n_vec <- mean_n_vec[-c(1, 7, 8)]
      sd_y_vec <- lapply(temp_mat[temp_mat[paste('heineken','product',k,sep='_')] == 1, ], sd)
      sd_y_vec <- sd_y_vec[-c(1, 7, 8)]
      sd_n_vec <- lapply(temp_mat[temp_mat[paste('heineken','product',k,sep='_')] == 0, ], sd)
      sd_n_vec <- sd_n_vec[-c(1, 7, 8)]
      for (j in 1:dim(temp_mat)[1]) {
        p_h_given_x <- prob_h *
          dnorm(temp_mat[[2]][j], mean = mean_y_vec[[1]], sd = sd_y_vec[[1]]) *
          dnorm(temp_mat[[3]][j], mean = mean_y_vec[[2]], sd = sd_y_vec[[2]]) *
          dnorm(temp_mat[[4]][j], mean = mean_y_vec[[3]], sd = sd_y_vec[[3]]) *
          dnorm(temp_mat[[5]][j], mean = mean_y_vec[[4]], sd = sd_y_vec[[4]]) *
          dnorm(temp_mat[[6]][j], mean = mean_y_vec[[5]], sd = sd_y_vec[[5]])
        p_no_h_given_x <- (1 - prob_h) *
          dnorm(temp_mat[[2]][j], mean = mean_n_vec[[1]], sd = sd_n_vec[[1]]) *
          dnorm(temp_mat[[3]][j], mean = mean_n_vec[[2]], sd = sd_n_vec[[2]]) *
          dnorm(temp_mat[[4]][j], mean = mean_n_vec[[3]], sd = sd_n_vec[[3]]) *
          dnorm(temp_mat[[5]][j], mean = mean_n_vec[[4]], sd = sd_n_vec[[4]]) *
          dnorm(temp_mat[[6]][j], mean = mean_n_vec[[5]], sd = sd_n_vec[[5]])
        value <- ifelse(!is.na(p_h_given_x/(p_h_given_x + p_no_h_given_x)), p_h_given_x/(p_h_given_x + p_no_h_given_x),0)
        cp_c3[cp_c3$cust_id == temp_mat[[1]][j], var] <- value
      }
    }
    else{ cp_c3[,var] = NA
    }
  }
  ifelse(k==1,cp_c3_p1 <- cp_c3, ifelse(k==2,cp_c3_p2 <- cp_c3, cp_c3_p3 <- cp_c3))
}


# Writing out matrices to csv to be able to load into excel ---------------

write.csv(cp_c1_p1, 'cp_c1_p1.csv')
write.csv(cp_c1_p2, 'cp_c1_p2.csv')
write.csv(cp_c1_p3, 'cp_c1_p3.csv')
write.csv(cp_c2_p1, 'cp_c2_p1.csv')
write.csv(cp_c2_p2, 'cp_c2_p2.csv')
write.csv(cp_c2_p3, 'cp_c2_p3.csv')
write.csv(cp_c3_p1, 'cp_c3_p1.csv')
write.csv(cp_c3_p2, 'cp_c3_p2.csv')
write.csv(cp_c3_p3, 'cp_c3_p3.csv')


# histograms and other assorted explorations---------------------------------


# histograms showing discount rate for HSB and SB
hist(-1*HSB$tran_prod_discount_amt/HSB$tran_prod_sale_amt, xlim = range(0,1))
hist(-1*SB$tran_prod_discount_amt/SB$tran_prod_sale_amt)


dim(superbock_cust_purchases[superbock_cust_purchases$brand_desc =='SUPER BOCK']) #71482
dim(superbock_cust_purchases[superbock_cust_purchases$brand_desc =='HEINEKEN']) #1301

# another hist showing rate
hist(-1*superbock_cust_purchases$tran_prod_discount_amt/superbock_cust_purchases$tran_prod_sale_amt)


# histograms showing cluster level discount rates for heineken buyers
hist(-1*heineken_cluster1$tran_prod_discount_amt/heineken_cluster1$tran_prod_sale_amt)
hist(-1*heineken_cluster2$tran_prod_discount_amt/heineken_cluster2$tran_prod_sale_amt)
hist(-1*heineken_cluster3$tran_prod_discount_amt/heineken_cluster3$tran_prod_sale_amt)


# showing the discount rates for heineken transactions from customers who also bought super bock and % of those above 50%  
sort(unique(-1*HSB$tran_prod_discount_amt/HSB$tran_prod_sale_amt))
sum(unique(-1*HSB$tran_prod_discount_amt/HSB$tran_prod_sale_amt)>.5)/length(unique(-1*HSB$tran_prod_discount_amt/HSB$tran_prod_sale_amt))
length(unique(-1*HSB$tran_prod_discount_amt/HSB$tran_prod_sale_amt))

# showing the discount rates for super bock transactions and % of those above 50%  
sort(unique(-1*SB$tran_prod_discount_amt/SB$tran_prod_sale_amt))
sum(unique(-1*SB$tran_prod_discount_amt/SB$tran_prod_sale_amt)>.5)/length(unique(-1*SB$tran_prod_discount_amt/SB$tran_prod_sale_amt))
length(unique(-1*SB$tran_prod_discount_amt/SB$tran_prod_sale_amt)>.5)

# histograms showing different discount rates for SB vs HSB customers
hist(SB_discount$V2,xlim = range(0,1))
hist(HSB_discount_rates$V2, xlim =range(0,1))
