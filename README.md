# Politicide By Alignment

Hello Everybody!

Welcome to this data project to visualize *politicide* by Alignment!

So today, @systats and I stumbled upon a Tweet with a pretty interesting **idea**.

![](images/max_fisher_tweet1.png)

We couldn't agree more. Political Science makes such claims **testable**!

**And so a challenge was born:**

![](images/fabio_tweet.png)

We decided to take on this task.

Right from the beginning we had to tackle a very pressing issue:

*How to measure how aligned a country is with the West or authoritarian countries like (Soviet) Russia and China?*

It seems kind of intuitive at first but hard to measure in reality. Could we use official alliances? Or trade value? We came to the conclusion that both of these measures could be flawed for various reasons. 

We ended up using arms transfers data, which has the clear upside of showing how much a country supporting another, thereby being a proxy measure for their alignment with a their seller country.

So we ended up scraping the military trade register from the **SIPRI Arms Transfers Database**

> The SIPRI Arms Transfers Database can be used to generate detailed written reports (trade registers) and statistical data (trend indicator values, TIV). To access the trade registers and TIV data, see the links provided below.

We used the following code to create our dataset:

```{r}
armstrades <- postForm("http://armstrade.sipri.org/armstrade/html/export_trade_register.php",
                   low_year = "1950",
                   high_year = "2016", 
                   seller_country_code = "AUS AST BEL CAN SWI CYP FRG DEN SPA FIN FRA UK GRE IRE ICE ITA NET NOR NZ POR SWE USA RUS USR CHI",
                   buyer_country_code = "",
                   armament_category_id = "any",
                   buyers_or_sellers = "sellers",
                   filetype = "csv", 
                   include_open_deals = "on", 
                   sum_deliveries = "on", 
                   Submit4 = "Download") %>% read_csv() 

save(armstrades, file = "data/armstrades.Rdata")
```

This includes data from: 

**Western Countries (by vdem variable `e_regionpol`)**

- Australia 
- Austria 
- Belgium 
- Canada 
- Switzerland 
- Cyprus 
- Germany 
- Denmark 
- Spain 
- Finland 
- France 
- United Kingdom of Great Britain and Northern Ireland 
- Greece 
- Ireland 
- Iceland 
- Italy 
- Netherlands 
- Norway 
- New Zealand 
- Portugal 
- Sweden 
- United States of America

**Comparison Countries**

- Soviet Union/Russia
- China

How [trend-indicator-values (TIVs)](https://www.sipri.org/databases/armstransfers/background) are measured