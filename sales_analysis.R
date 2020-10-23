# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# JUMPSTART: First Sales Analysis----

# 1.0 Load libraries ----

# Work horse packages
library(tidyverse)
library(lubridate)

# theme_tq()
library(tidyquant)

# Excel Files
library(readxl)
library(writexl)




# 2.0 Importing Files ----

#read data from excel file into a variable bikes_tbl
    #bikes_tbl <- read_excel(path = "00_data/bike_sales/data_raw/bikes.xlsx")
    #bikeshops_tbl <-read_excel("00_data/bike_sales/data_raw/bikeshops.xlsx")
    #orderlines_tbl <- read_excel("00_data/bike_sales/data_raw/orderlines.xlsx")
bikes_tbl <- read_excel("e:data/bikes/bikes.xlsx")
bikeshops_tbl <-read_excel("e:data/bikes/bikeshops.xlsx")
orderlines_tbl <- read_excel("e:data/bikes/orderlines.xlsx")

# 3.0 Examining Data ----
    #Work with the data to reorganise it to a data model.. part of data wrangling :)
    #We can choose to either output tibble to console or open the data window.
    #We can also inroduce a function glimpse() which works very well with wide data (data with many columns)
    #glimpse() actually transposes the tibble for ease of reading data.
glimpse(bikes_tbl)  #Notice that this can also be coded as bikes_tbl %>% glimpse()



# 4.0 Joining Data ----
    #left_join () function to join two tibbles using common fields. lets connsider orderlines and bikes. Notice
    #the matching columns are bike.id and product.id
left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

    #pipe %>% : useful in joining mu;tiple tibbles. It enables chaining many function operations together
    #The pipe passes the name of the tibble (orderlines_tbl) to the first argument of the function... see below..

bike_orderlines_joined_tbl <- orderlines_tbl %>%
    left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>% # notice that this %>% now extends the left_join() above
    left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))
    
bike_orderlines_joined_tbl %>% glimpse()


# 5.0 Wrangling Data ----

#This is basically data cleaning, manipulation and preaparation for analysis. 
# look at the newly created tibble bike_orderlines_joined_tbl and focus on the description and location columns.. 
# also with unit price we can compute the line total for an order etc. We use alot of dply() library here.

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
    
    #Separate the description field into three category.1, category.2 and frame.material i.e one col into  multiple cols
    separate(col = "description", 
             into = c("category.1", "category.2", "frame.material"),
             sep = " - ",
             remove = TRUE)%>%   


    #Separate location column into city and state. Notice that these two are delimited by ", " 
    separate(col = "location", 
             into = c("city", "state"),
             sep = ", ",
             remove = FALSE)%>%
    
    
    # Add calculated cols using mutate() function.For instance, Total = unit price * quantity
    
    mutate(total.price = price * quantity) %>%
    
   
    # select() : reorganize of the data by removing cols. the negatiuve sign before col name denoted remove. the opposite is true.
    #popular use of select is to use helpers such as select(ends_with())
  
    select(-order.id, -customer.id, -location, -product.id, -...1) %>%


    #combines two dataframes e.g. 
    bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>%
    
    
    #reorder columns using select()
    select(contains("date"), contains("id"), contains("order"),
           quantity, price, total.price, 
           everything()) %>%
    
    
    #renaming columns
    rename(order_date = order.date) %>%  # this is actually reversed to mean all dots to underscores!
    
    #to rename multiple columns programatically use set_names()
    set_names(names(.) %>% tolower()) %>%   #str_replace_all() takes in a pattern argument to fins and a replacement argument to replace with.
    
    set_names(names(.) %>% str_replace_all("\\.", "_"))


#Note that instead of glimpse(), we can also use  %>% View() (note: capital V) to direct the output to source pane as opposed to the cconsole pane..

bike_orderlines_wrangled_tbl %>% glimpse()



# 6.0 Business Insights ----


# 6.1 Sales by Year ----

# Step 1 - Manipulate

sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%
    
    #selecting cols to focus on and addinga year column
    select(order_date, total_price) %>%
    mutate(year = year(order_date)) %>%  #The year function grabs a year from given date.
    
    #summarise the sales data by year. use groupby()
    group_by(year) %>%
    summarise(sales = sum(total_price)) %>%
    ungroup() %>%   #remove any groups which could be a source difficult to detect errors.
    
    #format the currency.
    mutate(sales_text = scales::dollar(sales))
        


# Step 2 - Visualize
sales_by_year_tbl %>%
    #ggplot()setup a canvas with year on x-axis and sales on y axis. plots are built by adding
    #successive layers, including geometries & formatting elements.
    #aes() function maps column names in data to visual properties such as x, y, color, fill, etc.

    ggplot(aes(x = year, y = sales)) +
    
    #Geometries
    geom_col(fill = "#2c3e50") +   #fill specifies the bar fill color
    geom_label(aes(label = sales_text)) +
    geom_smooth(method = "lm", se = FALSE) + #linear regression using sales trend line.

    #Formatting
    theme_tq() +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Revenue by Year",
        subtitle = "Upward Trend",
        x = "",
        y = "Revenue"
    )

# 6.2 Sales by Year and Category 2 ----


# Step 1 - Manipulate

sales_by_year_cat_2_tbl <- bike_orderlines_wrangled_tbl %>%
    
    #Select the col to focus on, add Year col
    select(order_date, total_price, category_2) %>%
    mutate(year = year(order_date)) %>%
    
    #Group by and summarise on the year and category 2 
    group_by(year, category_2)%>%
    summarise(sales = sum(total_price)) %>%
    ungroup() %>%

    #Format currency
    mutate(sales_text = scales::dollar(sales))
sales_by_year_cat_2_tbl


# Step 2 - Visualize
sales_by_year_cat_2_tbl %>%
    
    #setup x, y , fill
    ggplot(aes(x = year, y = sales, fill = category_2)) +
    
    #geometries
    geom_col() +
    geom_smooth(method = "lm", se = FALSE) +

    #facet_wrap() - splits plots into multiple plots by a categorical feature in this case category_2.
    facet_wrap(~ category_2, ncol = 3, scale = "free_y") +
    
    #Formatiing
    theme_tq() + 
    scale_fill_tq()+
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Revenue by Year and Category 2",
        subtitle = "Each product category has an upward trend",
        x = "",
        y = "Revenue",
        fill = "Product Secondary Category"
    )


# 7.0 Writing Files ----
fs::dir_create("00_data/bike_sales/data_wrangled_student")

# 7.1 Excel ----

bike_orderlines_wrangled_tbl %>%
    write_xlsx("00_data/bike_sales/data_wrangled_student/bike_orderlines.xlsx")

  

# 7.2 CSV ----
bike_orderlines_wrangled_tbl %>%
    write_csv("00_data/bike_sales/data_wrangled_student/bike_orderlines.csv")


# 7.3 RDS ----

bike_orderlines_wrangled_tbl %>%
    write_rds("00_data/bike_sales/data_wrangled_student/bike_orderlines.rds")

# Many thanks to Matt Dancho, Business Science University for JumpStart with R course.