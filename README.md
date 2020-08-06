# Retail

This data set is related with retail domain and challenge is to predict whether a store should get opened or not based on certain factors such as sales, population,area etc


In dataset,
Each row represnts characteristic of a single planned store [planned does not mean opened].

Id : Store ID 
numeric sale figures for 5 types :
sales0
sales1
sales2
sales3
sales4

country : categorical :: coded values for country 
State : categorical :: coded values for State
CouSub : numeric :: subscription values at county level
countyname : Categorical :: county names
storecode : categorical :: store codes , this should not be used as is but can be source of a feature
Areaname : categorical :: name of the area , many times matches with county name
countytownname : categorical :: county town name
population : numeric :: population of the store area
state_alpha : categorical :: short codes for state
store_Type : categorical :: type of store 
store : categorical 1/0 : target indicator var 1=opened 0=not opened 
