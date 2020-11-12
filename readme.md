# Week 4 Project: Dirty Data

Contained in this project:

|Analysis Folder|Task Title|
|----------|-------------|
|[Task 1](task_1/documentation_and_analysis)|Decathalon Results |
|[Task 2](task_2/documentation_and_analysis)|Cake Ingredients |
|[Task 3](task_3/documentation_and_analysis)|Seabird Sightings |
|[Task 4](task_4/documentation_and_analysis)|Halloween Candy Survey |
|[Task 5](task_5/documentation_and_analysis)|Right Wing Authoritarianism Survey|
|[Task 6](task_6/documentation_and_analysis)|Dog Survey|

How to run the tasks:

Each task has:

- two data directories: one raw (dirty) and one clean
- scripts for producing clean data
- an analysis of the data

To run the analysis:

- pull this repository
- open the desired task directory in RStudio
- either:
- - run all chunks OR
- - knit to html

The analysis for each task uses the clean data. The cleaning scripts should
not need to be run but they are included for reference.

data sources:
| Task No. | Data Info | url |
|----------|-----------|-----|
|Task 1|Decathalon Data|https://husson.github.io/img/decathlon.csv|
|Task 2|Cake List|data(cake.ingredients.1961)*|
|Task 3|Seabirds and Ships|https://www.tepapa.govt.nz/sites/default/files/asms_10min_seabird_counts_final.xls/|
|Task 4|Candy Data Info|https://www.scq.ubc.ca/so-much-candy-data-seriously/|
|Task 5|RWA Survey Data||
|Task 6|Dog Owner Survey||

 
*the data can be accessed in R with this command 