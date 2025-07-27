import os
import pandas as pd
import yfinance as yf
import asyncio

INPUTS_PATH = os.path.join(os.path.dirname(__file__), 'data', 'inputs')
LOAD_RAW_SPOTS = False

async def query_yahoo_finance_data(ticker, start_date, end_date, info=None):
    """
    Fetch historical stock data from Yahoo Finance.

    Parameters:
    ticker (str): Stock ticker symbol.
    start_date (str): Start date in 'YYYY-MM-DD' format.
    end_date (str): End date in 'YYYY-MM-DD' format.

    Returns:
    pandas.DataFrame: DataFrame containing historical stock data.
    """
    data = yf.download(ticker, start=start_date, end=end_date)

    if info is not None:
        # drop level 1 of multiindex
        data.columns = data.columns.droplevel(1)

        # select field
        data = data[[info['flds']]]

        # rename column
        data.rename(columns={info['flds']: info['name']}, inplace=True)

    # date index to datetime
    data.index = pd.to_datetime(data.index)

    # resample to business days
    data = data.resample('B').ffill()

    return data

async def get_yahoo_finance_data(tickers_dict, start_date, end_date):
    outputs = await asyncio.gather(*[
        query_yahoo_finance_data(ticker, start_date, end_date, info) for ticker, info in tickers_dict.items()
    ])
    outputs_df = pd.concat(outputs, axis=1)

    # drop rows with NaN values
    outputs_df = outputs_df.dropna()

    return outputs_df

currencies_dict = {
    'CNY=X': {'flds': 'Close', 'name': 'USDCNY'},
    'EUR=X': {'flds': 'Close', 'name': 'EURUSD'},
    'SGD=X': {'flds': 'Close', 'name': 'USDSGD'},
    'MYR=X': {'flds': 'Close', 'name': 'USDMYR'},
    'JPY=X': {'flds': 'Close', 'name': 'USDJPY'},
    'IDR=X': {'flds': 'Close', 'name': 'USDIDR'},
    'KRW=X': {'flds': 'Close', 'name': 'USDKRW'},
    'INR=X': {'flds': 'Close', 'name': 'USINR'},
    'THB=X': {'flds': 'Close', 'name': 'USDTHB'},
    'AUD=X': {'flds': 'Close', 'name': 'USDAUD'},
    'CHF=X': {'flds': 'Close', 'name': 'USDCHF'},
    'GBP=X': {'flds': 'Close', 'name': 'USDGBP'},
    'VND=X': {'flds': 'Close', 'name': 'USDVND'},
    'TWD=X': {'flds': 'Close', 'name': 'USDTWD'},
}

if not LOAD_RAW_SPOTS:
    data = asyncio.run(get_yahoo_finance_data(currencies_dict, '1990-01-01', '2024-12-31'))
   
    # save raw data
    data.to_csv(os.path.join(INPUTS_PATH, 'yfinance_fx_spots_raw.csv'))
else:
    # load raw data
    data = pd.read_csv(os.path.join(INPUTS_PATH, 'yfinance_fx_spots_raw.csv'), index_col=0, parse_dates=True)

# transform exchange rates to SGDXXX
new_currencies_dict = {
    'USDCNY': 'SGDCNY',
    'EURUSD': 'SGDEUR',
    'USDSGD': 'SGDUSD',
    'USDMYR': 'SGDMYR',
    'USDJPY': 'SGDJPY',
    'USDIDR': 'SGDIDR',
    'USDKRW': 'SGDKRW',
    'USINR': 'SGDINR',
    'USDTHB': 'SGDTHB',
    'USDAUD': 'SGDAUD',
    'USDCHF': 'SGDCHF',
    'USDGBP': 'SGDGBP',
    'USDVND': 'SGDVND',
    'USDTWD': 'SGDTWD',
}

singapore_data_list = []
for old_name, new_name in new_currencies_dict.items():
    if old_name == 'USDSGD':
        # Special case for USDSGD, we need to keep it as is
        singapore_data_list.append(1/data[['USDSGD']].copy())
        continue

    # Convert the exchange rate to SGDXXX
    usdsgd_df = data['USDSGD'].copy()
    usdxxx_df = data[old_name].copy()
    sgdxxx_df = usdxxx_df / usdsgd_df
    singapore_data_list.append(pd.DataFrame(sgdxxx_df, columns=[new_name], index=sgdxxx_df.index))
singapore_data = pd.concat(singapore_data_list, axis=1).dropna()

singapore_data.to_csv(os.path.join(INPUTS_PATH, 'yfinance_fx_spots_sgd.csv'))
