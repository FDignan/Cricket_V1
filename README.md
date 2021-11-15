# Cricket_V1

The aim of this project is to create an end to end data pipeline for T20 ball-by-ball (BBB) data.

The source of this data is cricsheet.org, which has recorded BBB data for the vast majority of the all T20 matches ever played.

This data will be unpacked from YAML files into tabular relational database with a stable data model. The data will then updated from the same source daily as new matches are played.

This database will be the source for a dashboard enabling users to explore T20 data 

The need for this project arises because of the lack of suitable alternatives for easily available aggregated T20 statistics based on BBB data. Overall statistics can be found at cricinfo and other similar sites, but there is no breakdown to the ball level, which forms of the basis for most new T20 analytics. 

This data is open, but there is no standard readily available tool for an interested user to explore it without having to compile all of the BBB data themselves. 
