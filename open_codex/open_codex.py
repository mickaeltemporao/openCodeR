# -*- coding: utf-8 -*-

"""Main module."""
import os
import re
import json
import pandas as pd
import numpy as np

from pick import pick


def prompt_col(input_data, title = "Please select the column name of the variable to recode:"):
    """
    Prompts the user to enter the column name of the variables that
    needs to be recoded.

    """
    options = input_data.columns.tolist()
    option, index = pick(options, title)
    return option


def make_data(input_file, var_to_clean):
    """
    Creates a dictionary of key value pairs to be recoded.

    """

    input_data = pd.read_csv(input_file)
    output = (
        input_data[var_to_clean]
            .str.strip()
            .str.lower()
            .value_counts()
            .reset_index()
    )
    output.columns = ['raw', 'count']
    output['clean'] = np.nan
    return output.sort_values(by=['count','raw']).reset_index(drop=True)


def get_groups(group_file):
    """
    Prompts the user to enter de column name of the group variable to use.

    """
    df_tmp = pd.read_csv(group_file)
    option = prompt_col(input_data=df_tmp, title="Select the coding group id:")
    return option, list(set(df_tmp[option].tolist()))


def dic_lookup(input_file, var_to_clean, group_id):
    """
    Tries to load the coding dictionary or creates one when there is none.

    """
    root = re.sub(".csv", "", input_file)
    file_name = (
        root + "_" + var_to_clean + "_to_" + group_id + "_" + "codes.csv"
    )
    try:
        output = pd.read_csv(file_name)
    except FileNotFoundError:
        output = make_data(input_file=input_file, var_to_clean=var_to_clean)
        output.to_csv(file_name, index=False)
    return file_name, output


def open_codex(input_file="mtcars.csv", group_file="group_labels.csv"):
    """
    Recodes a column of a csv file to other categories defined by the group
    file (see group_labels.csv for an example file).

    Parameters
    ----------
    input_file: string
        The path to the raw csv file with the raw variables to recode.

    group_file: string
        The path to the file containing the different group recoding categories.

    Returns
    -------
        A .csv file is saved every 10 entries. The file will be located next
        to the original raw dataset.
    """

    input_data = pd.read_csv(input_file)
    group_id, group_labels = get_groups(group_file)
    var_to_clean = prompt_col(input_data)
    file_path, dataset = dic_lookup(input_file, var_to_clean, group_id)

    current_pct = 1-dataset.clean.isna().sum()/dataset.shape[0]

    save_count = 0
    while current_pct < 1:
        tmp_id = dataset[dataset.clean.isna()]['count'].idxmax()
        title = (
            """
            {}% Complete
            Please select group for:
              - {}
            """.format(round(current_pct*100, 2), dataset.iloc[tmp_id, 0])
        )
        options = group_labels.copy()
        multi_label = '_  Multi-choice'
        done_label = '_  Done'
        options.append(multi_label)
        option, index = pick(options, title)

        if option == multi_label:
            tmp_choice = []
            tmp_options = options.copy()
            tmp_options.remove(multi_label)
            tmp_options.append(done_label)

            while option == multi_label:
                tmp_option, index = pick(tmp_options, title)
                tmp_choice.append(tmp_option)
                if tmp_option == done_label:
                    tmp_options.remove(done_label)
                    option = tmp_options

        dataset.iloc[tmp_id, 2] = ', '.join(option)
        current_pct = 1-dataset.clean.isna().sum()/dataset.shape[0]

        save_count += 1
        if save_count == 10:
            dataset.to_csv(file_path, index = False)
            save_count = 0
    if current_pct == 1:
        print("\n\n    Bitondo's!!\n\n")
        dataset.to_csv(file_path, index = False)
