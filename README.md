# Comparative Analysis of Jakarta Composite Index (JCI) Forecasting

## Short Description
A time series forecasting project comparing traditional statistical methods and artificial neural networks to predict the daily closing price of the Jakarta Composite Index (JCI).

## Overview
This study evaluates six forecasting approaches using JCI historical data from January 1, 2024, to May 13, 2026, covering 558 trading days. The models are compared using the same chronological training and testing split (80:20) to ensure a fair evaluation.

The methods include:
- Basic Naive
- Naive with Drift
- Simple Exponential Smoothing (SES)
- Double Exponential Smoothing (DES)
- Autoregressive Integrated Moving Average (ARIMA)
- Artificial Neural Networks (ANN)

The dataset was cleaned and ordered chronologically, and stationarity was assessed using a power transformation and the Augmented Dickey-Fuller (ADF) test. The ANN experiments used multiple random seeds to reduce the effect of random initialization.

## Methodology
1. **Data preparation:** Cleaned and converted the historical closing-price data to numeric format and sorted observations chronologically.
2. **Train-test split:** Used the first 446 observations for training and the final 112 observations for testing.
3. **Time series analysis:** Examined the series for trend, volatility, and stationarity.
4. **Model development:** Built and compared six forecasting approaches, testing different smoothing parameters, ARIMA orders, and ANN lag/hidden-layer configurations.
5. **Evaluation:** Compared predictions against test data using Mean Absolute Error (MAE), Root Mean Squared Error (RMSE), and Mean Absolute Percentage Error (MAPE).

## Results
The best overall model was an ANN using lags 1 and 3 with two hidden layers containing 10 and 20 neurons, respectively.

| Model | RMSE | MAE | MAPE |
|---|---:|---:|---:|
| Naive with Drift (112-step forecast) | 135.308 | 103.035 | 1.446% |
| SES (alpha = 0.3) | 154.408 | 115.498 | 1.629% |
| DES (alpha = 0.1, beta = 0.05) | 156.138 | 118.946 | 1.675% |
| ARIMA (2, 1, 2) | 165.317 | 140.875 | 1.963% |
| ANN (lags 1, 3; hidden layers 10, 20) | 55.514 | 44.115 | 0.6149% |

The ANN achieved the lowest error values among the models in the full test-period comparison. Naive with Drift remained a useful benchmark and performed particularly well for shorter forecasting horizons.

## Key Findings
- The JCI series showed changing upward and downward movements, volatility, and no clear fixed seasonal pattern.
- The original series was found to be non-stationary; differencing after transformation was used to achieve mean stationarity for ARIMA modeling.
- Traditional methods did not outperform the Naive with Drift benchmark over the full test horizon.
- The ANN captured patterns that improved longer-horizon forecasting performance in this experiment.
- The ANN results were averaged across six random seeds to reduce dependence on a single initialization.

## Contributions
- Prepared and explored historical JCI time series data.
- Conducted stationarity and time series pattern analysis.
- Implemented and compared traditional forecasting models and an artificial neural network.
- Evaluated models using MAE, RMSE, and MAPE.
- Analyzed model performance across forecasting methods and ANN configurations.

## Tools and Technologies
- **Language:** R (as stated in the research design)
- **Time series methods:** Naive, Naive with Drift, SES, DES, ARIMA, ANN
- **Evaluation metrics:** MAE, RMSE, MAPE

## Data Source
The paper identifies the source file as `Data Historis Jakarta Stock Exchange.csv`, obtained from Investing.com. Please check the source's current terms and conditions before redistributing the raw data.

## Authors
- Marcell Prasetyo — BINUS University
- Aaron Jeremy Alexander — BINUS University

## Disclaimer
This project is an academic forecasting comparison, not financial advice. Forecast accuracy on a historical test period does not guarantee future performance.
