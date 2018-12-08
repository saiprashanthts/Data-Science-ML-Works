
/*
Task 1: Understanding the data in hand
A.	Describe the data in hand in your own words. (Word Limit is 500)
There are four-dimension files and one fact file which in turn will result into 4-dimension table and 1 fact table. 
It will be star schema with primary keys from dimension table linking to foreign keys to the fact table. 
The four dimension tables namely are cust_dimen, orders_dimen, prod_dimen and shipping_dimen and the fact table is market_fact

Cust_dimen ( Cust_id (text)(primary key), Customer_Name (text), Province(text), Region(text), Customer_Segment(text)) --> this dimension table provides details regarding the customer like id,name,region,segment,province

orders_dimen ( Order_ID int(11), Order_Date(date), Order_Priority (text), Ord_id (primary key) (text)) --> this dimension provides details regarding the orders like id,date,priority

prod_dimen (Product_Category(text), Product_Sub_Category(text), Prod_id(text)) --> this dimension provides details regarding the products like id,category,sub category

shipping_dimen(Order_ID (int(11)) , Ship_Mode(text), Ship_Date(date), Ship_id(text)(primary  key)) -- this dimension provides details regarding the shipping like id,oreder details,mode,date

market_fact (Ord_id(text)(fk), Prod_id(text) (fk), Ship_id(text) (fk), Cust_id(text) (fk), Sales (double), Discount (double), Order_Quantity(int(11)), Profit(double), Shipping_Cost(double), product_base_margin((text)))
--> this is fact table which takes order id,prod_id,ship_id,Cust_id from the respective dimension tables find out numerical values such as sales,quantity,profit,shipping_cost,margin. Here Margin is consider as text and if required to convert it to numeric we can cast

B.	Identify and list the Primary Keys and Foreign Keys for this dataset (Hint: If a table don’t have Primary Key or Foreign Key, then specifically mention it in your answer.) 
Cust_dimen  Cust_id (Primary Key)
orders_dimen  Ord_id (Primary Key)
prod_dimen  Prod_id (Primary Key)
shipping_dimen  Ship_id (Primary Key)
market_fact  Cust_id(Cust_dimen) (Foreign Key), Ord_id(orders_dimen) (Foreign Key), Prod_id(prod_dimen) (Foreign Key), Ship_id(shipping_dimen) (Foreign Key)
*/



SELECT @@GLOBAL .sql_mode;
SET GLOBAL sql_mode = 'ONLY_FULL_GROUP_BY';
SELECT @@GLOBAL .sql_mode;

use superstoresdb;

SELECT @@GLOBAL .sql_mode;
#Task 2: Basic Analysis

#A. Find the total and the average sales (display total_sales and avg_sales) 
# to find the sum and averae use sum and avg function in sql
SELECT 
    SUM(sales) AS total_sales, AVG(sales) AS avg_sales
FROM
    market_fact;

#B. Display the number of customers in each region in decreasing order of no_of_customers. The result should contain columns Region, no_of_customers
# in order to find the count for each region we need to do group by region and then find the count of cust_id
SELECT 
    region AS Region, COUNT(cust_id) AS no_of_customers
FROM
    cust_dimen
GROUP BY region
ORDER BY no_of_customers DESC;


#C. Find the region having maximum customers (display the region name and max(no_of_customers)
# to find the max make use of sub query in the inner sub query we will find the count of maximum number of customer and then find out the region for which count which matches 
# the innser sub query
SELECT 
    region AS Region, COUNT(cust_id) AS no_of_customers
FROM
    cust_dimen
GROUP BY region
HAVING COUNT(cust_id) = (SELECT 
        MAX(no_of_customers)
    FROM
        (SELECT 
            COUNT(cust_id) AS no_of_customers
        FROM
            cust_dimen
        GROUP BY region) der_table);
        
/*
SELECT 
    region AS Region, COUNT(cust_id) AS no_of_customers
FROM
    cust_dimen
GROUP BY region
HAVING COUNT(cust_id) = (SELECT 
        COUNT(cust_id) AS no_of_customers
    FROM
        cust_dimen
    GROUP BY region
    ORDER BY no_of_customers DESC
    LIMIT 1);
*/


#D. Find the number and id of products sold in decreasing order of products sold (display product id, no_of_products sold)
# In order to find the the number of products sold first do a group by on prod_id column and then sort the resut in descending based on the count result

SELECT 
    prod_id, COUNT(prod_id) AS no_of_productssold
FROM
    market_fact
GROUP BY prod_id
ORDER BY no_of_productssold DESC;


#E. Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and the number of tables purchased (display the customer name, no_of_tables purchased)

#In order to find the customers from Atlantic and purchased TABLES first we need to join market_fact,prod_dimen and cust_dimen and filter out the data and then group by 
#cust_id and cust_name 
SELECT 
    customer_name, COUNT(mf.cust_id) AS no_of_tables_purchased
FROM
    market_fact mf
        JOIN
    prod_dimen pd ON mf.prod_id = pd.prod_id
        JOIN
    cust_dimen cd ON mf.cust_id = cd.cust_id
WHERE
    cd.region = 'ATLANTIC'
        AND product_sub_category = 'TABLES'
GROUP BY mf.cust_id , customer_name;



#Task 3: Advanced Analysis

# A. Display the product categories in descending order of profits (display the product category wise profits i.e. product_category, profits)?

# first join market_fact and prod_dimen and then group by product_category
SELECT 
    product_category, SUM(profit) AS profits
FROM
    market_fact mf
        JOIN
    prod_dimen pd ON mf.prod_id = pd.prod_id
GROUP BY product_category
ORDER BY profits DESC;

# B. Display the product category, product sub-category and the profit within each subcategory in three columns. 
# first join market_fact and prod_dimen and then group by product_category and product_sub_category
SELECT 
    product_category,
    product_sub_category,
    SUM(profit) AS profits
FROM
    market_fact mf
        JOIN
    prod_dimen pd ON mf.prod_id = pd.prod_id
GROUP BY product_category , product_sub_category;





#C. Where is the least profitable product subcategory shipped the most? For the least profitable product sub-category, display the region-wise no_of_shipments and the profit made in each region in decreasing order of profits (i.e. region, no_of_shipments, profit_in_each_region)o Note: You can hardcode the name of the least profitable product subcategory
# the name of the least profitable product subcategory is TABLES
# By using the below code first find the least profitable product sub category 
#select product_sub_category from (select  product_category,product_sub_category,sum(profit) as profits from market_fact mf join prod_dimen pd on mf.prod_id=pd.prod_id group by product_category,product_sub_category order by profits asc limit 1)a;
# And then find sum and count based on group by on region and then displayit in descending order of profit#
#least profitable product subcategory shipped the most is ONTARIO

SELECT 
    region,
    COUNT(*) AS no_of_shipments,
    SUM(profit) AS profit_in_each_region
FROM
    market_fact mf
        JOIN
    cust_dimen cd ON mf.cust_id = cd.cust_id
		join 
    shipping_dimen sd on sd.Ship_id=mf.Ship_id
WHERE
    prod_id = (SELECT 
            prod_id
        FROM
            prod_dimen
        WHERE
            product_sub_category IN (SELECT 
                    product_sub_category
                FROM
                    (SELECT 
                        product_category,
                            product_sub_category,
                            SUM(profit) AS profits
                    FROM
                        market_fact mf
                    JOIN prod_dimen pd ON mf.prod_id = pd.prod_id
                    GROUP BY product_category , product_sub_category
                    ORDER BY profits ASC
                    LIMIT 1) a))
GROUP BY region
ORDER BY profit_in_each_region DESC;




