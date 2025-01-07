# macro_data_and_forecasts
This project aims at creating a macroeconomic database and produce customizable forecasts for some of the variables (GDP per capita, capital stock, labor participation rate, migration etc.).

For GDP estimations the Solow growth model is used: (capital stock^(1-labor share)*labor stock^(labor share)*TFP (Total Factor Productivity)

Initial data sources: IMF WEO, Penn World Tables, Maddison Project, World Bank, UN single age population projection for the Medium and Zero Migraiton Scenarios

Default model parameters are estimated from the past or are expert judgments
The model tries to be comprehensive, and estimates input data when missing (for example the capital stock, based on the typical capital-output ratio for a given GDP per capita level)
Modeling philosophy: slow, but non-zero convergence to long-term steady state parameters/estimated relationships, if possible empirically based, large default weights on historical outcomes

There are 4 main scenarios produced with any set of parameters:

**Baseline**: accept UN population forecasts  

**Pension reform**: make the upper age limit of the working age population the function of life expectancy at 65 (current implementation: life expectancy minus 15 years)  

**High migration**: estimate migration and population levels based on relative GDP per worker levels  

**Pension reform plus high migration**: implementing both of the above two (pension reform and high migration), most bullish for rich countries/EU  

These scenarios all have separate labor forces, capital stocks, GDP levels etc.




