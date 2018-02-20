==========
Open Codex
==========


.. image:: https://img.shields.io/pypi/v/open_codex.svg
        :target: https://pypi.python.org/pypi/open_codex

.. image:: https://img.shields.io/travis/mickaeltemporao/open_codex.svg
        :target: https://travis-ci.org/mickaeltemporao/open_codex

.. image:: https://readthedocs.org/projects/open_codex/badge/?version=latest
        :target: https://open_codex.readthedocs.io/en/latest/?badge=latest
        :alt: Documentation Status




An python tool to recode open-ended survey questions.


* Free software: MIT license
* Documentation: https://open_codex.readthedocs.io.

Getting Started
---------------
These instructions will get you a copy of the project up and running
on your local machine for development and testing purposes.
The package can be built and explored using the cli.

Installation
~~~~~~~~~~~~

OS X & Linux::

    git clone https://github.com/mickaeltemporao/open_codex
    cd open_codex
    make install

Features
~~~~~~~~

To recode a variable run::

    codex recode --i=path/to/input_file.csv --g=path/to/new_groups.csv

Credits
-------

This package was created with Cookiecutter_ and the `audreyr/cookiecutter-pypackage`_ project template.

.. _Cookiecutter: https://github.com/audreyr/cookiecutter
.. _`audreyr/cookiecutter-pypackage`: https://github.com/audreyr/cookiecutter-pypackage
