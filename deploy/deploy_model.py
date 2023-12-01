import pandas as pd
from statsmodels.tsa.statespace.sarimax import SARIMAX
from sklearn.metrics import mean_squared_error, mean_absolute_error, mean_absolute_percentage_error
import numpy as np
import pickle as pkl

# Load and wrangle data
caiso_data = pd.read_csv("CAISO_data.csv")
caiso_data['date'] = pd.to_datetime(caiso_data['timestamp'].str[:10], format='%Y-%m-%d')
use_data = caiso_data.groupby('date').agg(total_demand=('load_MW', 'sum')).reset_index()

# Train-Test Split
train_size = int(len(use_data) * 0.75)
train, test = use_data[1:train_size], use_data[train_size:]

# Fit SARIMA model
model = SARIMAX(train['total_demand'], order=(1, 1, 1), seasonal_order=(1, 1, 1, 12))
results = model.fit(disp=False)

# Make predictions
forecast = results.get_forecast(steps=len(test))
mean_forecast = forecast.predicted_mean

# Calculate loss metrics
mse = mean_squared_error(test['total_demand'], mean_forecast)
rmse = np.sqrt(mse)
mae = mean_absolute_error(test['total_demand'], mean_forecast)
mape = mean_absolute_percentage_error(test['total_demand'], mean_forecast)

print(f'Mean Squared Error (MSE): {mse}')
print(f'Root Mean Squared Error (RMSE): {rmse}')
print(f'Mean Absolute Error (MAE): {mae}')
print(f'Mean Absolute Percentage Error (MAPE): {mape}')

# Save the model
with open('sarima.pickle', 'wb') as handle:
    pkl.dump(results, handle)
