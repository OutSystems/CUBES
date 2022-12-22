# CUBES - A New Dimension in Query Synthesis From Examples

# Setup

## Requirements

- Working installation of conda/miniconda (https://docs.conda.io/en/latest/miniconda.html)

## Instructions

1. Create a new conda environment based on the package list at `conda.txt`:

    ```conda create --name cubes --file conda.txt```

2. Activate the conda environment:

    ```conda activate cubes```

    From now on all commands should be run with the conda environment activated.

3. Install required Python packages through pip:

    ```pip install psutil z3-solver chromalog pylru frozendict sqlalchemy libs/TestSuiteEval-1.0.3-py3-none-any.whl pebble```

# Usage

Instance files are in the `tests-examples` folder.

## Sequential

```python ./sequential.py path-to-yaml-file```

You can use `--help` to see all configuration options available in cubes.

## Parallel

```python ./cubes.py path-to-yaml-file```

You can use `--help` to see all configuration options available in cubes (parallel version).

## Running all benchmarks

In order to select which instances you would like to benchmark, you need to create a folder `tests`. You can then copy the desired instances from `tests-examples` or symlink them, if you desire.

```PYTHONPATH=. ./helper-scripts/benchmark.py execution-descriptor```

Here `execution-descriptor` is any string that is a valid filename and is used to store the results in the folder `./analysis/data/execution-descriptor`.
You can use `--help` to see all configuration options available in the benchmark script.

## Disambiguation

At this point disambiguation is implemented as a post-processing step after cubes has generated all solution under the desired time limit and requires the benchmark script to have been used.

To perform disambiguation you can use the following command:

```PYTHONPATH=. ./helper-scripts/disambiguation-post.py execution-descriptor```

You can use `--help` to see all configuration options available in the disambiguation script.

## Accuracy

Like the disambiguation script, accuracy requires instances to have been run using the benchmark script introduced before.

To perform accuracy analysis you can use the following command:

```PYTHONPATH=. ./helper-scripts/fuzzy-check.py --run=execution-descriptor```

If you want to perform accuracy analysis over the results of a previous disambiguation you can use:

```PYTHONPATH=. ./helper-scripts/fuzzy-check.py --from-dis --run=execution-descriptor```

You can use `--help` to see other configuration options available in the accuracy analysis script.

# Final notes

- The file `groups/cubes_500.instances` contains the list of instances used for evaluation in the parts where only a subset of instances was used. This file can be passed as the `--instances` argument to the benchmark script.

- The folder `analysis` contains all code used to analyse the results obtained and produced all graphs included in the paper.

- The folders `analysis/data` and `analysis/fuzzy` come pre-packaged with all logs used in the evaluation section of the paper.
