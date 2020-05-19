library(dplyr)
library(purrr)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(tidytext)
library(tidyverse)
library(plotly)
library(knitr)
library(data.table)
library(kableExtra)
library(ggpubr)

# The palette for ggplot:
cbPalette <- c("#0073C2FF", "#EFC000FF", "#999999", "#CC79A7", "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")

#Import the data
supstore_df=read.csv('superstore.csv', stringsAsFactors = TRUE, header = T)

#supstore_df=read.csv('E:/Deenu/Personal Workspace/GoogleDrive/CSDA1050-Capstone-Project-Repo/Datasets/superstore-sales/superstore.csv', stringsAsFactors = TRUE, header = T)

#Data format conversion process
supstore_df$Order.Date = parse_date_time(supstore_df$Order.Date, "dmy")
supstore_df$Ship.Date = parse_date_time(supstore_df$Ship.Date, "dmy")
supstore_df$Postal.Code = as.factor(supstore_df$Postal.Code)

#remove the columns
supstore_df = select(supstore_df, -"Customer.Name")

#Adding columns for data and time individually.
supstore_df_new =
    supstore_df %>%
    mutate(Profit_pct = round( (Profit/Sales)*100 ,2),
           Ordered.Year = lubridate::year(Order.Date), 
           Ordered.Quarter = lubridate::quarter(Order.Date), 
           Ordered.Month = lubridate::month(Order.Date, label=TRUE, abbr=TRUE), 
           Ordered.Day = lubridate::day(Order.Date),
           Ordered.Week.day = lubridate::wday(Order.Date, week_start = 1, label=TRUE, abbr=TRUE),
           Delivery_time = difftime(Ship.Date, Order.Date, units = c("days"))
    )

#summary(supstore_df_new)

#Binning or adding categorical values based on ranges in continous data
setDT(supstore_df_new)[Profit_pct > 100, Profit_pct_range := "> 100%"]
supstore_df_new[Profit_pct >= 0 & Profit_pct <10, Profit_pct_range := "0 to 10%"]
supstore_df_new[Profit_pct >= 10 & Profit_pct <20, Profit_pct_range := "10% to 20%"]
supstore_df_new[Profit_pct >= 20 & Profit_pct <30, Profit_pct_range := "20% to 30%"]
supstore_df_new[Profit_pct >= 30 & Profit_pct <40, Profit_pct_range := "30% to 40%"]
supstore_df_new[Profit_pct >= 40 & Profit_pct <50, Profit_pct_range := "40% to 50%"]
supstore_df_new[Profit_pct >= 50 & Profit_pct <60, Profit_pct_range := "50% to 60%"]
supstore_df_new[Profit_pct <= 0 & Profit_pct > -10, Profit_pct_range := "0 to -10%"]
supstore_df_new[Profit_pct <= -10 & Profit_pct > -20, Profit_pct_range := "-10% to -20%"]
supstore_df_new[Profit_pct <= -20 & Profit_pct > -30, Profit_pct_range := "-20% to -30%"]
supstore_df_new[Profit_pct <= -30 & Profit_pct > -40, Profit_pct_range := "-30% to -40%"]
supstore_df_new[Profit_pct <= -40 & Profit_pct > -50, Profit_pct_range := "-40% to -50%"]
supstore_df_new[Profit_pct <= -50 & Profit_pct > -100, Profit_pct_range := "-50% to -100"]
supstore_df_new[Profit_pct <= -100 & Profit_pct > -150, Profit_pct_range := "-100% to -150%"]
supstore_df_new[Profit_pct <= -150 & Profit_pct > -300, Profit_pct_range := "-150% to -300%"]

#Numeric conversion
supstore_df_new$Profit_pct_range = as.factor(supstore_df_new$Profit_pct_range)
supstore_df_new$Profit_pct = as.numeric(supstore_df_new$Profit_pct)

#List for customer segments
customer_seg_list = as.list(unique(supstore_df_new$Segment))

#List for Region segments
region_seg_list = as.list(unique(supstore_df_new$Region))

#List for category segments
category_seg_list = as.list(unique(supstore_df_new$Category))

#Average Basket Price
avg.basket.price =
    supstore_df_new %>%
    group_by(Customer.ID) %>%
    summarise(avg.price = round(mean(Sales), 2))
    
#Total sales revenue
sales.revenue =
    supstore_df_new %>%
    summarise(total_revenue = round(sum(Sales)))

### Analyze the product orders trend by Months
#Overall orders grouped by input
# supstore_df_orders_bySegment = 
#     supstore_df_new %>%
#     filter(Order.Date > "2014-01-01" & Order.Date <"2017-12-30") %>%
#     group_by(Sub.Category) %>%
#     summarise(Orders = n_distinct(Order.ID),
#               Average.profit = round(mean(Profit)),
#               Average.sales = round(mesan(Sales)),
#               total_profit = round(sum(Profit)),
#               total_sales = round(sum(Sales))) %>%
#     mutate(sales_ratio = round( (Average.sales/sum(Average.sales) * 100), 1),
#            profit_ratio = round( Average.profit/Average.sales * 100),
#            profit_ratio_avg = round( total_profit/total_sales * 100)) %>%
#     arrange(Sub.Category)
# 
# 
# # Initiate a ggplot bubble chart
# ggplot(supstore_df_orders_bySegment, aes(x = total_sales, y = profit_ratio)) +
#     geom_point(aes(color = Sub.Category, size = Orders), alpha = 0.6) +
#     theme(legend.position="none") +
#     scale_size(range = c(1, 40)) +
#     scale_y_continuous(labels = scales::label_percent(scale = 1, suffix = "%")) +
#     scale_x_continuous(labels = scales::unit_format(unit = "k", prefix = "$", scale = 1e-3)) +
#     labs(title = "Profit Ratio and Sales by Sub-category",
#          x = "Sales",
#          y = "Profit Ratio")


#label_percent(accuracy = NULL, scale = 100, suffix = "%", trim = TRUE)
#axs + scale_y_continuous(labels = scales::percent_format(accuracy = 5L))
#axs + scale_y_continuous(labels = scales::dollar_format(prefix = "$"))
#leg + scale_x_continuous(labels = scales::unit_format(unit = "k", scale = 1e-3))

if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
    detach(package:dplyr, unload=TRUE)
}
library(plyr)
#install.packages("arules", dependencies=TRUE)
library(arules)

df_itemList_bskt3 = ddply(supstore_df_new, 
                          c("Order.ID"),
                          function(supstore_df_new) paste(supstore_df_new$Product.Name, collapse = "|"))

dim(df_itemList_bskt3)

#For Basket Analysis, we do not need Order.ID and Order.Date
df_itemList_bskt3$Order.ID = NULL
df_itemList_bskt3$Order.Date = NULL
df_itemList_bskt3$Category = NULL


#Rename column headers for ease of use
colnames(df_itemList_bskt3) = c("itemList")

#Write dataframe to a csv file using write.csv()
write.csv(df_itemList_bskt3,"ItemList.csv", quote=FALSE, row.names = TRUE)

#Using the read.transactions() functions, we can read the file ItemList.csv and convert it to a transaction format
txn = read.transactions(file = "ItemList.csv", rm.duplicates = TRUE, format = "basket", sep = "|", cols = 1)
#View(txn@itemInfo$labels)

#Removing the Quotes which are introduced in read.transactions 
txn@itemInfo$labels = gsub("\"", "", txn@itemInfo$labels)
#View(txn)
head(txn@itemInfo$labels, 40)

#Execute the apriori algorithm on the transactions by specifying minimum values for support and confidence. Support is an indication of how frequently the items appear in the data. Confidence indicates the number of times the if-then statements are found true. A third metric, called lift, can be used to compare confidence with expected confidence
basket_rules = apriori(txn, parameter = list(sup = 0.00025, conf = 0.3, target="rules"))

dim(basket_rules@lhs@itemInfo)
#Print the association rules using function inspect(). Note that if package ‘tm’ attached in the session, it creates a conflict with the arules package. Thus, we need to check and detach the package.
if(sessionInfo()['basePkgs']=="tm" | sessionInfo()['otherPkgs']=="tm"){
    detach(package:tm, unload=TRUE)
}

inspect(basket_rules)

#Alternative to inspect() is to convert rules to a dataframe and then use View()
df_basket <- as(basket_rules, "data.frame")
dim(df_basket)

#View(df_basket)
#convert to datframe and view; optional
df_basket <- as(basket_rules,"data.frame")
df_basket$confidence <- df_basket$confidence * 100

# split lhs and rhs into two columns
library(reshape2)
df_basket <- transform(df_basket, rules = colsplit(rules, pattern = "=>", names = c("lhs","rhs")))

# Remove curly brackets around rules
df_basket$rules$lhs <- gsub("[[:punct:]]", "", df_basket$rules$lhs)
df_basket$rules$rhs <- gsub("[[:punct:]]", "", df_basket$rules$rhs)

# convert to chracter
df_basket$rules$lhs <- as.character(df_basket$rules$lhs)
df_basket$rules$rhs <- as.character(df_basket$rules$rhs)

#inspect(df_basket)
library(stringi)
library(dplyr)

#Remove duplicates from LHS
df_basket_filtered = 
    df_basket %>%
    distinct(df_basket$rules$lhs, .keep_all = TRUE)

#Remove column
df_basket_filtered = select(df_basket_filtered, -'df_basket$rules$lhs')

#Input the product name in LHS to show RHS
df_basket_filtered$rules %>%
    filter(stri_detect_fixed(lhs, "Acco Hanging Data Binders")) %>% 
    select(rhs)
