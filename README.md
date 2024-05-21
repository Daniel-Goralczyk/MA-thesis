# MA-Thesis

## About
Title: Navigating Ideological Impact: Exploring Negativity Bias on YouTube 
  
My final work at the University of Konstanz culminated in the production of a master’s thesis regarding the YouTube content posted by U.S. Senators. The thesis is currently under review and will be published at a later date. Below is the source code for all tables and figures as well as the data collection process.

## Instructions

1. Data:  
      
   * The data necessary for each of the projects is found in the [data](/data) folder. Here are the files and their purposes:
     
     * 'new_data.feather': This file contains the raw text inputs needed to run the sentiment analysis with RoBERTa. Found inside a compressed folder. 
     * 'video_sentiment_scores_local_gpu.pkl': This is a file containing all the unaggregated sentiment scores.
     * 'aggregated_elements_list.csv': The final aggregated sentiment score for each video.
     * 'videos_complete.csv': This is the data containing the video information, which is needed to run the web scraping script.
     * 'sen_youtube_data.zip': The data needed to re-create the tables and figures that were used in the final paper.
     
     
        
2. Scripts:  
     
   Currently, there are five files that cover some of the most important steps in creating my final thesis. The first two are Jupyter notebooks that contain the code for collecting the sentiment scores for each of the video captions and then aggregating them. The next three markdown files are R scripts that show how I collected the captions and created the figures and tables used in the final paper.  

    1. Sentiment Analysis with RoBERTa (Python)
       * File: [sentiment_analysis.ipynb](/sentiment_analysis.ipynb)
       * Description: This Jupyter notebook uses the RoBERTa model to perform sentiment analysis on the raw text data. It processes the 'new_data.feather' file to generate sentiment scores for each text.  
         
    2. Aggregating Sentiment (Python)  
       * File: [aggregating_sentiment.ipynb](/aggregating_sentiment.ipynb)
       * Description: This Jupyter notebook takes the sentiment scores generated from the previous step and aggregates them. It uses the 'video_sentiment_scores_local_gpu.pkl' file to provide sentiment metrics, the final output is stored in 'aggregated_elements_list.csv'.
    3. Scraping with RSelenium (R)  
       * File: [Rselenium_collection_git.md](/Rselenium_collection_git.md)
       * Description: This R script demonstrates how to use RSelenium to scrape the captions from Downsub. The file needed is 'video_complete.csv'.  
    4. Replication of Tables (R)
       * File: [replication_tables_git.md](/replication_tables_git.md)
       * Description: This R script replicates the tables that were used on the final paper. It uses the 'sen_youtube_data.csv' file to generate these tables, which is found compressed inside 'sen_youtube_data.zip'.   
    5. Replication of Figures (R)
       * File: [replication_figures_git.md](/replication_figures_git.md)  
       * Description: This R script replicates the figures that were used on the final paper. It uses the 'sen_youtube_data.csv' file to generate these figures, which is found compressed inside 'sen_youtube_data.zip'. 

## Built with
* R version 4.3.3 
* RStudio
* Python version 3.11.5


## Authors
* Daniel Goralczyk
