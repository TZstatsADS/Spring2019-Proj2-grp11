Background & Insights:

•	Though ride-hailing industry is rapidly growing these year, New York City is actually limiting those apps such as Uber and Lyft by putting a freeze in licensing new cars in the city. This plan was made to prevent urban congestion. However, those apps are still very convenient options for people who need short-distance driving services. Under this circumstances, we want to compare the taxi and Uber pickup number. By doing this, we can see which one is more popular at NYC and also the pattern (shrinkage or growth) of number change for both options during 2015 and 2016. 

•	Getting a parking violation ticket might be the last thing you want to see after you drive your friend to the airport. Maybe you should call a taxi. It’s easier for you, but will it be costly? Just to make life easier, we want to compare the taxi price and the potential parking violation fee by time specifically at New York Airports (i.e. JFK, EWR and LGA). This could help people to make a decision when they need to go to the airport. 

Dataset (OPEN NYC):

•	Yellow taxi data

•	Uber pickup data

•	Parking violation data

Plan:

•	Obtain the dataset we need and update the dataset by adding data of 2016

•	Select features we interest that can clearly show the comparison under different conditions (e.g. business day or not)

•	Process the data and explore it more by visualizing it

•	Update the code to add panels we need






# Project 2: Shiny App Development Version 2.0

### [Project Description](doc/project2_desc.md)

![screenshot](doc/screenshot2.png)

In this second project of GR5243 Applied Data Science, we develop a version 2.0 of an *Exploratory Data Analysis and Visualization* shiny app on a topic of your choice using [NYC Open Data](https://opendata.cityofnewyork.us/) or U.S. government open data released on the [data.gov](https://data.gov/) website. See [Project 2 Description](doc/project2_desc.md) for more details.  

The **learning goals** for this project is:

- business intelligence for data science
- study legacy codes and further development
- data cleaning
- data visualization
- systems development/design life cycle
- shiny app/shiny server

*The above general statement about project 2 can be removed once you are finished with your project. It is optional.

## Project Title Lorem ipsum dolor sit amet
Term: Spring 2019

+ Team #
+ **Projec title**: + Team members
	+ Chen, Xinyi
	+ Hu, Xinyi
	+ Jiang, Hongye
	+ Lin, Nelson
	+ Zhang, Liwei 

+ **Project summary**: 
This app three topics, which are traffic map, time flow map and airport traffic map. For the dataset, we covered NYC weather, taxi and uber pickup number, airport traffic between 2015 and 2016.

The first map shows NYC traffic in normal weather(i.e. sunny) and bad weather(i.e. rainy, snowy, hail and foggy). 

The second map focuses on comparing pickup numbers and locations of NYC taxi and uber by times. From the perspective of location, it's not hard to tell that NYC taxi is much more popular than uber in Manhattan Island, while only a small amount of taxi pickups appeared in Brooklyn and Queens. Besides, larger circle radiuses show larger pickup number. From 2015 to 2016, more customers chose to take Taxi rather than Uber. However, the Uber dataset only covers the first half year of 2015. If there is newer dataset for Uber, we might see a clearer trend of Taxi and Uber growth.

The third map shows airport traffic depending on where and when someone is picked up. This is useful for showing the amount of traffic one might encounter on their trip. 


+ **Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement.

Panel 1(traffic map):

Panel 2(time flow map): Liwei Zhang queried, cleaned Uber data. Xinyi Hu queried, clean yellow taxi data. We built the panel 2 together. 

Panel 3(airport traffic map): NL queried, cleaned the data and built the tab for the shiny app. Xinyi Hu integrated it into the rest of the app.

Presentation: Liwei Zhang is the presenter.



Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── app/
├── lib/
├── data/
├── doc/
└── output/
```

Please see each subfolder for a README file.

