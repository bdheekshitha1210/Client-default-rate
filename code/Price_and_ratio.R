library(data.table)
library(recommenderlab)
setwd('D:/Emory/Marketing/Segmentation/Pernalonga')

transac = fread("transaction_table.csv",header=TRUE)
prod = fread("product_table.csv",header=TRUE)

data = merge(transac, prod, by='prod_id')

customer_cluster = fread("customer_cluster.csv", header=TRUE)
bigtable = merge(data, customer_cluster, by="cust_id",all.x=TRUE)

all_H = bigtable[brand_desc=="HEINEKEN",]
unique(all_H$prod_id) # 3 different products 999682638(bottle) 999159921(case) 999939106(bottle)
# each product median price
price_1 = bigtable[prod_id==999682638,]
median(price_1$prod_unit_price) #0.8316667
price_2 = bigtable[prod_id==999159921,]
median(price_2$prod_unit_price) #18.99
price_3 = bigtable[prod_id==999939106,]
median(price_3$prod_unit_price) #1.031667

# looking at customers in three segments based on previous project
segment1 = bigtable[cluster==1,]
segment2 = bigtable[cluster==2,]
segment3 = bigtable[cluster==3,]

# segment 1------------

# all transactions involving super bock products
SB <- segment1[segment1$brand_desc =='SUPER BOCK']
# total quantity of each customer
segment1_SB_quantity = SB[,sum(tran_prod_sale_qty),by=cust_id]
write.csv(segment1_SB_quantity, "segment1_SB_quantity.csv",row.names = FALSE)

# all the heineken sales in this segment right now
sum(segment1[brand_desc=="HEINEKEN"]$tran_prod_sale_amt) # 2587.82
# all the super bock sales in this segment
sum(segment1[brand_desc=="SUPER BOCK"]$tran_prod_sale_amt) # 158554

# get the purchasing profile for each customer (customer vs. brand)
customer_brand_mat1 = dcast(segment1, cust_id ~ brand_desc, fun.aggregate = function(x) 1, fill =0)
# customers who have bought Heineken and Super Bock in the pasts
both = customer_brand_mat1[ HEINEKEN ==1 & `SUPER BOCK`==1,]
# filtered DF of all transactions involving customers who bought both super bock and Heineken
both_purchase = segment1[segment1$cust_id %in% both$cust_id]
# calculate the ratio of heineken bought to both types of beer bought by customers who bought both
s1_ratio_prod_1 = sum(both_purchase[prod_id==999682638]$tran_prod_sale_qty)/
  sum(both_purchase[brand_desc=="HEINEKEN"|brand_desc=="SUPER BOCK"]$tran_prod_sale_qty)
# 0.20191
s1_ratio_prod_2 = sum(both_purchase[prod_id==999159921]$tran_prod_sale_qty)/
  sum(both_purchase[brand_desc=="HEINEKEN"|brand_desc=="SUPER BOCK"]$tran_prod_sale_qty)
# 0.00674
s1_ratio_prod_3 = sum(both_purchase[prod_id==999939106]$tran_prod_sale_qty)/
  sum(both_purchase[brand_desc=="HEINEKEN"|brand_desc=="SUPER BOCK"]$tran_prod_sale_qty)
# 0.02668

# segment 2----

# all transactions involving super bock products
SB <- segment2[segment2$brand_desc =='SUPER BOCK']
# total quantity of each customer
segment2_SB_quantity = SB[,sum(tran_prod_sale_qty),by=cust_id]
write.csv(segment2_SB_quantity, "segment2_SB_quantity.csv",row.names = FALSE)

# all the heineken sales in this segment right now
sum(segment2[brand_desc=="HEINEKEN"]$tran_prod_sale_amt) # 4914.15
# all the super bock sales in this segment
sum(segment2[brand_desc=="SUPER BOCK"]$tran_prod_sale_amt) # 225555.6

# get the purchasing profile for each customer (customer vs. brand)
customer_brand_mat2 = dcast(segment2, cust_id ~ brand_desc, fun.aggregate = function(x) 1, fill =0)
# customers who have bought Heineken and Super Bock in the pasts
both = customer_brand_mat2[ HEINEKEN ==1 & `SUPER BOCK`==1,]
# filtered DF of all transactions involving customers who bought both super bock and Heineken
both_purchase <- segment2[segment2$cust_id %in% both$cust_id]
# calculate the ratio of heineken bought to both types of beer bought by customers who bought both
s2_ratio_prod_1 = sum(both_purchase[prod_id==999682638]$tran_prod_sale_qty)/
  sum(both_purchase[brand_desc=="HEINEKEN"|brand_desc=="SUPER BOCK"]$tran_prod_sale_qty)
# 0.13167
s2_ratio_prod_2 = sum(both_purchase[prod_id==999159921]$tran_prod_sale_qty)/
  sum(both_purchase[brand_desc=="HEINEKEN"|brand_desc=="SUPER BOCK"]$tran_prod_sale_qty)
# 0.00571
s2_ratio_prod_3 = sum(both_purchase[prod_id==999939106]$tran_prod_sale_qty)/
  sum(both_purchase[brand_desc=="HEINEKEN"|brand_desc=="SUPER BOCK"]$tran_prod_sale_qty)
# 0.01278

# segment 3----

# all transactions involving super bock products
SB <- segment3[segment3$brand_desc =='SUPER BOCK']
# total quantity of each customer
segment3_SB_quantity = SB[,sum(tran_prod_sale_qty),by=cust_id]
write.csv(segment3_SB_quantity, "segment3_SB_quantity.csv",row.names = FALSE)

# get the purchasing profile for each customer (customer vs. brand)
customer_brand_mat3 = dcast(segment3, cust_id ~ brand_desc, fun.aggregate = function(x) 1, fill =0)
# customers who have bought Heineken and Super Bock in the pasts
both = customer_brand_mat3[ HEINEKEN ==1 & `SUPER BOCK`==1,]
# filtered DF of all transactions involving customers who bought both super bock and Heineken
both_purchase <- segment3[segment3$cust_id %in% both$cust_id]
# calculate the ratio of heineken bought to both types of beer bought by customers who bought both
# calculate the ratio of heineken bought to both types of beer bought by customers who bought both
s3_ratio_prod_1 = sum(both_purchase[prod_id==999682638]$tran_prod_sale_qty)/
  sum(both_purchase[brand_desc=="HEINEKEN"|brand_desc=="SUPER BOCK"]$tran_prod_sale_qty)
# 0.12819
s3_ratio_prod_2 = sum(both_purchase[prod_id==999159921]$tran_prod_sale_qty)/
  sum(both_purchase[brand_desc=="HEINEKEN"|brand_desc=="SUPER BOCK"]$tran_prod_sale_qty)
# 0.00750
s3_ratio_prod_3 = sum(both_purchase[prod_id==999939106]$tran_prod_sale_qty)/
  sum(both_purchase[brand_desc=="HEINEKEN"|brand_desc=="SUPER BOCK"]$tran_prod_sale_qty)
# 0.01369

