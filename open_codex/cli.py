# -*- coding: utf-8 -*-

"""Console script for open_codex."""

import click
import open_codex as oc

@click.group()
def cli():
    pass


@click.command()
@click.option('--i', help="The raw .csv that needs to be recoded.")
@click.option('--g', default="group_labels.csv", help="The .csv file containing new group labels.")
def recode(i, g):
    """
    Recodes the column of a csv file to pre defined categories available
    in the group file (see group_labels.csv for an example file).
    """

    oc.open_codex(i, g)

cli.add_command(recode)
