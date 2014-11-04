HospitalQualityProject
======================

Data analysis of disease outcome statistics from hospitals across the U. S. 

This project focuses on three diseases: heart attack, heart failure, and pneumonia. 

best.R takes in a disease and state and returns the hospital with the best outcome in that state. 

rankhospital.R takes in a disease, state, and either "best" or "worst" and returns the hospital with the best or worst outcome in the state. 

rankall.R takes in a disease and a ranking and returns all hospitals with that ranking. 

These scripts manually separate the data into appropriate categories and splits off NA values that confuse "best" or "worst" matches. Probably they could be revised to be more efficient with the aggregate command. 





