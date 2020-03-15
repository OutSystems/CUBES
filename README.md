# SQUARES - A SQL Synthesizer Using Query Reverse Engineering

Given a set of input-output examples (tables), SQUARES returns the desired query in R and in SQL. SQUARES is built on top of [Trinity](https://github.com/fredfeng/Trinity).

    
# Usage

## Using Jupyter Notebook:
    - Go to jupyter-notebook folder and launch the jupyter notebook and select demo.ipynb:
        ```
        jupyter notebook
        ```
  
## Using [Google Colab](https://colab.research.google.com/drive/1wPwP1iWBLqmNTk9ffxNPR0mj3GbbUZr2):
    - save the google colab doc to your account and run it.

## Using CLI:
```
usage: squares [-h] [-d] [--symm-on | --symm-off] [--r | --no-r] [--tree | --lines] [--limit LIMIT] [--seed SEED] SPECIFICATION

A SQL Synthesizer Using Query Reverse Engineering

positional arguments:
  SPECIFICATION  specification file

optional arguments:
  -h, --help     show this help message and exit
  -d, --debug    Print debug info.
  --symm-on      compute symmetries online
  --symm-off     compute symmetries offline
  --r            output R program
  --no-r         don't output R program
  --tree         use tree encoding
  --lines        use line encoding
  --limit LIMIT  maximum program size
  --seed SEED
```


#Installation

### Prerequisites
- Python 3.6+
- R

## Using [anaconda](https://www.anaconda.com)

- Install [anaconda](https://www.anaconda.com)
- run 
  ```
  TODO
  ```
 
- every time before using SQUARES run:
    ```
    conda activate squares
    ```

## Using `setup.py` and `setup.r`

- Create a new virtual environment (strongly recommended):
  ```
  python3 -m venv env
  ```

- Install python dependencies:
  ```
  pip install .
  ```
  
- Install R dependencies:
  ```
  Rscript setup.r
  ```

References

 - Pedro Orvalho, Miguel Terra-Neves, Miguel Ventura, Ruben Martins and Vasco Manquinho. Encodings for Enumeration-Based Program Synthesis. CP'19
 - Pedro Orvalho. SQUARES : A SQL Synthesizer Using Query Reverse Engineering. MSc Thesis. Instituto Superior Técnico - Universidade de Lisboa. 2019.
 - Ruben Martins, Jia Chen, Yanju Chen, Yu Feng, Isil Dillig. Trinity: An Extensible Synthesis Framework for Data Science. VLDB'19
- Yu Feng, Ruben Martins, Osbert Bastani, Isil Dillig. Program Synthesis using Conflict-Driven Learning. PLDI'18.
 - Yu Feng, Ruben Martins, Jacob Van Geffen, Isil Dillig, Swarat Chaudhuri. Component-based Synthesis of Table Consolidation and Transformation Tasks from Examples. PLDI'17

