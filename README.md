# Bulk Forecasting

We have to make **predictions** for web traffic with focus on the **scalability**. 
Infact, we have to manage 25 Stores, 12 Countries and 9 Marketing Channels.. so we have to handle with 2.700 time series!!

We use daily data of Google Analytics from unsampled reports so defined *| Date | Channel | Country | Device | Visits |* to build a predictor that is combination of **Hybrid** and **Prophet** algorithms. In the repository you will find:
- **Run_Scripts** is the master script that loads libraries, reads input files, executes other scripts and writes the final output
- **Mapping** aggregates daily data in months and maps GA Channels and Countries to our internal classification
- **Hybrid Model** builds a predictor combining Arima and Ets models
- **Prophet Model** uses the *prophet* predictor implemented by Facebook
- **Plot** gives back a plot for Country. Each plot is a grid with forecast plots by Channels.
