# -*- coding: utf-8 -*-

"""Console script for open_codex."""

import click
import open_codex as oc

@click.command()
def main(args=None):
    """Console script for open_codex."""
    click.echo("Replace this message by putting your code into "
               "open_codex.cli.main")
    click.echo("See click documentation at http://click.pocoo.org/")
    return 0


if __name__ == "__main__":
    import sys
    sys.exit(main())
