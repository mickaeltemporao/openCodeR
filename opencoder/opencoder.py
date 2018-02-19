# -*- coding: utf-8 -*-

"""Main module."""
import pandas as pd
import numpy as np
from pick import pick

raw_data_path = "https://gist.githubusercontent.com/seankross/a412dfbd88b3db70b74b/raw/5f23f993cd87c283ce766e7ac6b329ee7cc2e1d1/mtcars.csv"

data = pd.read_csv(raw_data_path)

def prompt_col(data, title = "Please select the column name to recode:"):
    options = data.columns.tolist()
    option, index = pick(options, title)
    return option


def make_data(data, selected_column):
    output = (
        data[selected_column]
            .str.strip()
            .str.lower()
            .value_counts()
            .reset_index()
    )
    output.columns = ['key', 'count']
    output['value'] = np.nan
    return output.sort_values(by=['count','key']).reset_index(drop=True)


def get_groups(group_file):
    df_tmp = pd.read_csv(group_file)
    option = prompt_col(data=df_tmp, title="Please select the group codings:")
    return df_tmp[option].tolist()


groups = get_groups(group_file)
selected_column = prompt_col(data)
tmp_data = make_data(data, selected_column)

current_pct = 1-tmp_data.value.isna().sum()/tmp_data.shape[0]

while current_pct < 1:
    current = tmp_data[tmp_data.value.isna()].
    title = 'Please group for {}your favorite programming language: '

    options = ['Java', 'JavaScript', 'Python', 'PHP', 'C++', 'Erlang', 'Haskell']
    options = df.cyl.unique().tolist()
    option, index = pick(options, title)
