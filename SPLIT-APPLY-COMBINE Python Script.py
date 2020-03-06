import pandas as pd
import numpy as np

bureau = pd.read_csv('bureau_score.csv')

debt_history = bureau[['CUSTOMER_UNIQUE_ID', 'BAL_30DPD', 'BAL_60DPD', 'BAL_PL_90DPD']]

debt_history = debt_history.groupby(['CUSTOMER_UNIQUE_ID']).sum

debt_history()
# performing the sum function on the BAL_30DPD, BAL_60DPD and the BAL_90DPD 
