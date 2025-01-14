# Assignment-2
# Load required libraries
library(rvest)
library(dplyr)
library(httr)
library(tidyverse)

# Base URL for the iPhone search results on Amazon India
base_url <- "https://www.amazon.in/s?k=iphone&qid=1730566584&ref=sr_pg_"

# Function to scrape each page
scrape_page <- function(page_number) {
  tryCatch({
    # Construct the URL for the current page
    url <- paste0(base_url, page_number)
    print(paste("Scraping URL:", url))  # Debugging: Print the URL being scraped
    
    # Read the HTML content of the page
    page <- read_html(GET(url, add_headers("User-Agent" = "Chrome/112.0.0.0 Safari/537.36")))
    
    # Extract the data from the page
    Name <- page %>%
      html_nodes(".a-color-base.a-text-normal") %>%
      html_text(trim = TRUE)
    
    Rating <- page %>%
      html_nodes(".a-icon-star-small") %>%
      html_text(trim = TRUE)
    
    People <- page %>%
      html_nodes(".s-link-style .s-underline-text") %>%
      html_text(trim = TRUE)
    
    Price <- page %>%
      html_nodes(".a-price-whole") %>%
      html_text(trim = TRUE)
    
    Bought <- page %>%
      html_nodes(".a-size-small+ .a-size-base .a-color-secondary") %>%
      html_text(trim = TRUE)
    
    # Combine into a data frame
    data <- data.frame(
      Name = Name,
      Rating = Rating,
      People = People,
      Price = Price,
      Bought = Bought,
      stringsAsFactors = FALSE
    )
    
    return(data)
    
  }, error = function(e) {
    message("Error on page: ", page_number)
    message("Error details: ", e$message)  # Print the error message
    return(NULL)
  })
}
# Scrape all pages and combine the data into one data frame
all_data <- lapply(1:num_pages, scrape_page) %>%
  bind_rows()

# Display the first few rows of the scraped data
head(all_data)

### Part I-A Automated Data Collection (30 points)
```{r}
all_data <- read.csv("iPhone_data_amazon.csv")
```


## Step 1: Choosing the Scraping Target and URL Structure
The first step that a researcher has to make in the process of data collection is choosing the website of interest and the site address scheme. Specifically, the scraping object was selected as the search results of iPhone in Amazon India. This aspect is basic for web scraping because the URL structure will define how we will be scrolling through pages of products. The base URL is "https://www.amazon.in/s?k=iphone&qid=1730566584&ref=sr_pg_”; which is then change dynamically to scrap few more pages of the results. We add a page number at the end of the anchor links (e.g., &page=1, &page=2, etc.), which let’s us scrape data from all the pages of the search results. The choice of the base URL with pagination is crucial when working with the large amount of data obtained from multiple pages of products. This helps because the site might have data located in any part of the page structure or URL pattern and we want to make sure we are getting all that we are previewing. The flexibility to have different URL for each page combined with the option for scalability makes it possible to extract data from many pages. This method is effective, for it organizes the collection of product information on individual pages without repetition.

## Step 2: Selecting the Data to Scrape and Identifying CSS Selectors
The second part of the data extraction shows the main information that should to be scraped from each of the Web page. In this case the product data in the analysis entails the name, rating, rating from people, price, and number of purchases made on the item. These variables were chosen because they give some important information about the sales of the product, its quality, and its price, which are all useful for analysis. The choice of these variables is made based on the importance of these factors in consumers buying behavior and product and price trends in the market. The next question is what CSS selectors to evaluate to obtain these data quickly after the key information is defined? For example, .a-color-base.a-text-normal is used for the product names, while .a-icon-star-small for ratings. Actually, the correct selection of CSS selectors is very important since the quality of the extracted data would rely on its selection. When it comes to the choice of selectors, mistakes might occur and lead to the data acquisition with gaps or containing mistakes. Thus, for the sake of making of this step efficient, one has to check the source of the given page to make sure the correct CSS classes and HTML nodes are selected. This process ensures that we get the right data elements as well as making sure that those elements were gathered coherently in the different Google pages.

## Step 3: Data Cleaning and Handling Missing Values
The third process is data cleaning, which is an important process because it make the raw scraped data ready for analysis. Data gathered directly from web pages usually have a relatively high level of noise, variability, and contain gaps. Consequently, data cleaning should done to produce the final data file whose results will be beneficial and accurate. For instance, price and people count may contain commas like ₹1,299 in this instance, all these commas needs to be stripped. Moreover, some product categories may not have information on ratings, number of people, or price tags, and therefore any records containing these attributes will have missing values. For this, the code pads to make the extracted vectors same size i.e Name vector , Rating vector, Price vector etc. In the case where some of the columns are shorter than others, missing data is used (denoted by NA). It makes the data more manageable to analyze as those preparing it categorize it into a common format. The following step is the handling missing values either by imputation or deletion depending on the analysis. For example, there are some ratings missing that are replaced by median value of those ratings. Data cleaning is important especially to remove records that may poll most of the data hence compromising the analysis made on the data.

## Step 4: Automating the Scraping and Combining the Data
The last aspect of the collection of data is to automate that scraping process and then join the data from different pages. The function scrape_page is created to scrape all the pages one by one, extract the meaningful information and present it as data frame. To achieve data from multiple pages the author has used the lapply function that iterates the scrape_page function for the number of pages 1 to 20. This makes sure that data is extracted from a wide coverage of the product listing, to a maximum of twenty results pages. The results from each page are then binded and the bind_rows are used to merge all the data frames into a final data frame. The automation of this process makes the work easier, scalable, to a level where one can collect data from hundreds or thousands of pages without much interference. In addition, the proposed method establishes systematic and regular data collection, which will minimize errors and guarantee a diverse collection of data. So by automating the process it becomes more efficient we can generate more data that can be used further for analysis or to present to the management.

## Part I-B Data Exploration and Contextualisation (10 points)
```{r}
View(all_data)
```

### Rationale Behind the Data Selected
The reason for using the data from Amazon India for this study stems from the fact that it is one of the most popular platforms for selling consumer products including iPhones. Due to coverage and dominance of the virtual marketplace, especially in India, using Amazon as a platform provides rich scope and understanding of consumers’ purchasing behavior, preferences and product evaluations, albeit with several key socio-economic indicators more relevant and useful in the social sciences, particularly on socio-economic factors influencing technology acceptance and consumer decision making processes. Product ratings and prices as well as sales data provides an understanding on how features of products relate to the way they are perceived by consumers and the affordability of such products.

From the social sciences’ perspective, the insights of this dataset can be used to study socio-economic factors: possible digital divide, affordable access to elements of new technologies (like smartphones) by different populations, consumer behavior. It provides insight into how price sensitivity Pale, buyer-seller rating, and social influence reflected in the number of reviews and sales determine the final consumption choice. Secondly, the data points to how various factions use technologies as an adaptation guiding pattern analysis and shifts in consumer trends over time.

Further, the data set used by Amazon is essentially a quasi-experimental design for understanding the effects of the reviews and rating on the products’ performance. This is particularly germane to social sciences research as it aids understanding how the interaction on the social web, such as through reviews and rating affect perceptions, trust, and decision-making. Further, understanding the patterns of ratings, price, and sales volume, the given research unveils how the public opinion, as the reviews represent, directly contributes to the shift of people’s consumer tendencies in the digital economy.

Last, the broad ownership of smartphones in India underscores the importance of this dataset in identifying the prospects of technology in emergent economy. The two demand patterns: the relationship between iPhones and price as well as the feedback that consumers give are all in some way or the other are tied to social concerns including income inequalities and access to technology among others as well as the influence of the consumer culture. Therefore, various phenomena that are vital in sociological studies, including socio-economic, cultural as well as behavioral characters are well captured in this data set.


```{r}
# Load the 'pacman' package (install if needed)
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,  # Tidyverse packages, including dplyr, ggplot2, and purrr
  glue,      
  ggplot2,    # Data visualization
  gridExtra,  
  kableExtra, 
  flextable,  
  skimr       
)

# Avoid scientific notation
options(scipen = 999)
```

Before proceeding with data exploration and contextualization, let us view the structure of the data
```{r}
str(all_data)
```

### Dimension of the Data
```{r}
dim(all_data)
```

### View the data
```{r}
library(kableExtra)
kable(head(all_data))
```

### Remove commas from the Price and People columns and convert to numeric
```{r}
all_data$Price <- as.numeric(gsub(",", "", all_data$Price))
all_data$People <- as.numeric(gsub(",", "", all_data$People))
```

### Extract the first component (numeric rating) and convert to numeric
```{r}
all_data$Rating <- as.numeric(sub(" out of 5 stars", "", all_data$Rating))
```

### Extract the first component for Bought Column
```{r}
all_data$Bought <- as.factor(sub(" bought in past month", "", all_data$Bought))
head(all_data)
```

### Check the structure of the data frame to confirm changes
```{r}
str(all_data)
```

### Alternative, Check the Structure of the Data set
```{r}
glimpse(all_data)
```

The dataset consists of five variables: Name, Rating, People, Price, and Bought. The Name variable is a character vector containing the product names and descriptions, such as "iPhone 16 128 GB: 5G Mobile Phone with Camera Control, A18 Chip and a Big Boost in Battery Life." These names provide essential details about each product, including storage capacity, features, and color. The Rating variable is numeric and represents the customer rating for each iPhone model, ranging from 4.4 to 5.0 stars. The People variable is also numeric, showing the number of customer reviews or ratings each product has received, with values such as 202, 2625, and 32138. The Price variable is numeric and denotes the price of each iPhone model in Indian Rupees (INR), with values like 74900, 67999, and 43999. The Bought variable contains character data describing the number of units sold in the past month, with strings like "400+ bought in past month" and "1K+ bought in past month." This variable requires further processing to extract the numeric values for analysis. The dataset, with 359 observations, includes information relevant to iPhone model popularity, pricing, customer satisfaction, and sales performance.

## Handling Missing Values
### Check for the Missing Values
```{r}
missing_values <- sapply(all_data, function(x) sum(is.na(x)))
print(missing_values)
```

### Eliminate the Missing Values for the Model Name and the Number of Pieces Bought
```{r}
all_data <- na.omit(all_data)
missing_values <- sapply(all_data, function(x) sum(is.na(x)))
print(missing_values)
```

### Descriptive Statistics
```{r}
skim(all_data)
```

```{r}
library(psych)  
library(knitr) 
summary_stats <- describeBy(all_data)
summary_stats_rounded <- summary_stats %>%
  mutate_if(is.numeric, ~round(., 1))
kable(summary_stats_rounded)

```

Rating (M = 4.5, SD = 0.0), which represents the average customer rating, showed very little variation, as evidenced by the zero standard deviation. The median rating was 4.5, and the range was from 4.4 to 4.5, with a skewness of 0.0 and a kurtosis of -1.2, indicating a nearly normal distribution.

People (M = 12,453.9, SD = 14,042.7), representing the number of people who reviewed each product, exhibited high variability, with values ranging from 202 to 32,138. The median number of reviews was 2,625. The distribution was positively skewed (skew = 0.7) and showed platykurtic behavior (kurtosis = -1.5), suggesting that the distribution was not normal and that fewer products had exceptionally high review counts.

Price (M = 58,054.5, SD = 13,340.7) reflected the price of the iPhones, with values ranging from 43,499 to 144,900. The median price was 56,999. The price data were positively skewed (skew = 1.1) and leptokurtic (kurtosis = 3.7), suggesting a concentration of lower-priced products with a few products at higher prices.

## Assessment Part II
### Part II-A Building an Interactive Dashboard with R Shiny (30 points)

### Bar Graph of Units per iPhone Model
```{r, fig.height=9}
library(ggplot2)
# Assuming `all_data` is your cleaned dataset
all_data$Bought <- gsub("K\\+|\\+", "000", all_data$Bought)  # Clean the Bought column

# Create a bar plot of total units sold per iPhone model
ggplot(all_data, aes(x = Name, y = as.numeric(Bought))) +
  geom_bar(stat = "identity", fill = 'steelblue') +
  labs(title = "Total Units Sold per iPhone Model", x = "iPhone Model", y = "Units Sold")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

```{r}
library(highcharter)
# Assuming `all_data` is your cleaned dataset
all_data$Revenue <- as.numeric(all_data$Price) * as.numeric(gsub("K\\+|\\+", "000", all_data$Bought))  # Calculate revenue

# Create a pie chart for revenue distribution
hchart(all_data, "pie", hcaes(x = Name, y = Revenue), name = "Revenue Distribution")

```

```{r}
library(ggplot2)
library(dplyr)

# Comparative Rating Chart for Items Bought
data_filtered <- all_data %>%
  filter(Bought %in% c("100+", "500+", "5K+")) %>%
  filter(Rating %in% c(4.4, 4.5))
    
# Count ratings per Bought category
comparison_data <- data_filtered %>%
  group_by(Bought, Rating) %>%
  summarize(count = n())
    
# Create the bar chart
ggplot(comparison_data, aes(x = Bought, y = count, fill = factor(Rating))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparative Rating for Items Bought (500+, 1K+, 5K+)", 
       x = "Items Bought Category", y = "Count", fill = "Rating") +
  theme_minimal()
```

```{r, fig.height=9}
library(ggplot2)

# Assuming `all_data` is your cleaned dataset
# Create a bar plot for the number of people who rated per iPhone model
ggplot(all_data, aes(x = Name, y = People)) +
  geom_bar(stat = "identity", fill = 'orange') +
  labs(title = "Number of People Who Rated per Model", x = "iPhone Model", y = "Number of Reviews") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

## Justification of the Visuals Selected
### Bar Graph of Units per iPhone Model
Among the visualization used in this report is the “Total Units Sold per iPhone Model” for it is a reliable means of identifying the performance of a product in a specific period as well as the popularity of certain models. It can be said that this bar chart conveys a simple idea of the total units sold of all iPhones, and will help to compare the popularity and the sales of the specific iPhone models. This presentation of information makes it easy to work out which models delivered the most sales, which models did not, and perhaps which areas could benefit from increased attention due to shifting customer preferences.

This visualization is especially useful for analyzing sales trends, for example, the effect that the addition of new features or a new design has on product buying. For example, it may be as follows: Owing to technology development, it is clear that some models which record high levels of sales may be due to changes such as improved cameras or batteries. On the other hand, a decline in sale may well suggest that the product has reached its market or consumers are no longer as interested in the product as before.

Besides, this plot facilitates the generation of strategies on managing product life cycles and product portfolios. For instance, sales data is useful in forming the inventory acquisition, marketing promotions and introduction of new products to the market. The simplicity of the visualization and its narrowed focus further mean that it can be effectively comprehended by a wide range of consumers including business managers and analysts. As presented in the performance by model this chart provides stakeholders with factual information for decisions that reflect market demands and consumers preferences.


### Revenue Distribution by Model
The “Revenue Distribution by Model” method is one of the tools for the analysis of the model’s contribution to revenue. This chart proves helpful in showing figures derived per model, making it easier to determine which product contributes to the company’s overall revenue. Through revenue/ sale share of each model, other stakeholders would easily see which particular product is more profitable to the organization/ business. Of particular note about this visual is the ability to evaluate the degree of coherence at the price level with demand. For instance, a model revealing moderate volume but high dollar sales shows that superiority through superior pricinguracy has been realized. On the other hand, a model that is giving large sales while portraying lesser market value might mean that it is placed among low-price products, requiring critical reevaluation.
The chart also provides the context for analyzing business data and supporting decisions related to new products, marketing expenditure, and resource utilization. For example, the models with high maximum revenue should be optimized and further promoted actively, while those with low maximum revenue might request suitable campaigns or combination strategies to enhance the maximum revenue in the future. This visualization enables decision-makers to better understand business revenue distribution and allocate resources for sustaining future models in a way that is not only customer engaging but that also meets financial goals. Since it is very easy to understand, it makes it easy for even someone less technical to analyze and understand revenue data.

### Comparative Rating for Items Bought
Comparatively, ‘Comparative Rating for Items Bought’ is one of the most important enablers of defining customer satisfaction based on the extent of purchase made. This chart allows analyzing the correlation of the item’s rating (for example, 4.5 against 4.4) and the amount of its purchases (500+, 1k+, 5k+) to reveal subtleties of the connection between product popularity and its quality in the eyes of clients. As with all of the graphs presented here, each of these two is an important tool for analyzing trends in customer preference and satisfaction. For example, high averages for 5k+ in item indicate quality standardisation in highly bought products, low averages in smaller purchase categories could be reflecting fluctuating quality or targeted appeal. This way plans help businesses identify potential locations where improvements in Quality might benefit consumers. Because of the comparative format of the chart it can be a useful tool for determining the need for benchmarking of product performance. It shows which rating is most typical for each purchase volume levels and may demonstrate alterations in client preferences or actions. Besides, it can reveal patterns, for example, whether customers who made large orders are more satisfied with the products, which may be useful for determining the key price promotions or the convenience of creating a program of frequent buyers. In addition, by comparing ratings between purchase levels, this visual helps stakeholders to understand how the product satisfies customers’ needs by identifying the necessary changes to marketing, production, or customer support strategies to meet customers’ demand.

### Number of People Who Rated per Model
In particular, the “Number of People Who Rated per Model” chart is a very effective method to measure customer interest and product models’ appeal. With the help of the presented number of ratings for each model, this chart gives an idea of the quantity of requests and interest on the part of customers in definite goods. This kind of visualization is especially helpful for deciding which models are the most popular since the greater the number of ratings, the more often they are used as well as the higher demand for them on the market. It assists a company in distinguishing which models garner high revenues and analyze the effectiveness of a marketing campaign or a new product line. On the other hand, the models with less rating would mean less amount of engagement or less market reach, which is valuable information for the company to focus on or to target for promotions.

Moreover, this chart may be used as the reflection, albeit indirect, of the tendencies in customer satisfaction. It appears that some models have ratios that are low on the numerator and high on the denominator which could signify that there is low customer interaction or reluctance to provide feedback which could conceivably indicate satisfaction problems. The fact that this particular type of chart is not cluttered and very easy to read helps devise relevant stratagems and make sense of trends quickly. In particular, using the given information, businesses can enhance the range of products and services, improve the ways of receiving and processing customers’ feedback, and make future choices more aligned with customers’ needs and behaviors.

## Part II-C Critical Engagement with AI: ChatGPT (20 points)
### Reflection on ChatGPT's Contributions to the Computational Process
ChatGPT was incredibly useful as a collaboration aid during the creation of the interactive dashboard. The initial competence of Signal to polish code and produce fresh strategies enriched the algorithm computationally to a large extent. For instance, while solving a problem where it was required to plot an interactive comparative bar graph for ratings based on the purchase volume, ChatGPT came up with accurate, technique-oriented and explorable solutions using ggplot2 and plotly. This was particularly helpful in avoiding the time and effort that would otherwise be spent in developing the best methods towards debugging and visualization. Moreover, optimizing time taken solving problems like formatting data or even how to structure the components of Shiny app was a great asset from ChatGPT. Incorporating all these suggestions allowed me to drive more attention toward actualizing the enhancement of the analytical results rather than spending too much time struggling with technical issues. It also provided an iterative feedback mechanism which meant the authors were forced to be more involved with the dataset in order to come up with useable and sensible visualizations.

Besides improving and polishing technical aspects, ChatGPT provided ideas of various solution implementation. For example, the proposed addition of interactivity to the “Number of People Who Rated per Model” improved accessibility of the insights as well as engagement for technical end-users. Another aspect of helping in justifying the visualizations was making the narrative of the dashboard coherent. All in all, ChatGPT did more than enhance the technocrat ACE Project; It promoted higher and imaginative analysis throughout the completion of the Assignment.

### Evaluation of Challenges and Learning Experience
Thus, although using ChatGPT provided significant advantages, some issues arose which suggest critical estimation of the results. For example, although while coming up all the solutions it had correct syntax and code snippets some of them did not understand the data structure for what it was meant for and they had to be modified. This brought out the fact that the tool relies on clear specific instructions most of the time if the instructions given are ambiguous the resultant solutions are generalized or irrelevant. Also, ChatGPT often came up with the implementation methodologies that were more complicated than the problem required, and, hence, required post processing adjustments and simplifications to the output. This was because the work had to achieve the most from its use of this advanced capability, while at the same time, ask business question that could be realistically answered given the environment.

From a learning perspective, ChatGPT greatly influenced this assessment process by being a knowledge guide and a partner. Due to this, it helped to fasten the development of new visualizations and the adjusting of the dashboard, making the process more constructed and not simply aimed at problem solving. But it is with ChatGPT that I had to constantly work to think critically on my own and make sure that I am aware of the reasoning behind the proposed solutions. The principal jobs allowed me to strengthen me problem solving abilities and to broaden me knowledge about Shiny app and data visualization. In the end, ChatGPT enriched the project, enhancing the learning process, and making investigators more aware of the AI tool as an additional useful instrument in computational manipulations and decision-making.
