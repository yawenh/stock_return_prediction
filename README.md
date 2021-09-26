# Forward Return Prediction    
#### Overview:  
In this project, we want to predict the forward return of a financial asset based on the historical price series of the target asset and other two assets. The price data is given as minutely price of the three assets over a year.    

The project mainly composed of three parts:    
* Part 0 - Data Preparation:    
	+ Calculate the backward return of the three assets.    
	+ Calculate the 3-weeks rolling correlation of 3-min backward returns of each pair of the three assets.     
	*Code for this part can be found in `data_prep.R`.*    
    
* Part I - Regression:    
	+ In this part we fitted four models based on backward return. The four models are: Linear regression model, KNN model, Ridge model, and LASSO model.      
	*Code for this part can be found in `data_analysis.R`.*     
     
* Part II - Final Prediction:    
    + In this part, we gather the information from the last part to fit our final prediction model.     
	+ This part mainly contain three steps:     
		* Add more predictors (backward-return).    
	    * Use LASSO to select predictors.    
		* Use linear regression to fit the model based on the selected predictors.    
		*Code for this part can be found in `prediction.R`.*     

