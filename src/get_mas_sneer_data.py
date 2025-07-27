import os
import pandas as pd
import requests

INPUTS_PATH = os.path.join(os.path.dirname(__file__), 'data', 'inputs')
url = "https://www.mas.gov.sg/api/v1/MAS/chart/sneer"
headers = {
    "Accept": "application/json, text/plain, */*",
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36",
    "Referer": "https://www.mas.gov.sg/statistics/exchange-rates/s$neer",
    "Accept-Language": "en-US,en;q=0.9",
}

# Fetch the data from the MAS API
response = requests.get(url, headers=headers)

if response.status_code == 200:
    data = response.json()
    sneer_df = pd.DataFrame(data['elements'])
    sneer_df = sneer_df[['date', 'value']]
    sneer_df['date'] = pd.to_datetime(sneer_df['date'])
    sneer_df.set_index('date', inplace=True)
    sneer_df.sort_index(inplace=True)
    sneer_df.rename(columns={'value': 'SNEER'}, inplace=True)

    sneer_df.to_csv(os.path.join(INPUTS_PATH, 'mas_sneer_data.csv'))