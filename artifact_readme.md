# Description of included files

- `analysis`: directory where log files will be stored
- `helper-scripts`: useful script for running several benchmarks or extracting information from log files
- `squares`: source code for the tool
- `tests-examples`: directory that contains all benchmarks
- `tests`: directory that is used to specify which benchmarks should be run when using the helper scripts. Sub-directories from tests-examples can either be symlinked to here, or copied over. The TODO!!!!!!
- `sequential.py`, `portfolio.py`, `cubes.py`: files to run the corresponding configurations.
- `miniconda.sh`, `env.tar`: setup files containing all required dependencies

# Installation
Note: all commands should be run in the CUBES directory


Install and setup conda:

    ./miniconda.sh -b -p $HOME/anaconda
    eval "$(/home/tacas21/anaconda/bin/conda shell.bash hook)"
    conda init

Extract conda environment:

    tar -xvf env.tar

Activate conda environment (**this should be run every time a new shell is opened**):

    conda activate ./env

# Running
Note: all commands should be run in the CUBES directory

## Running a single benchmark

    python sequential.py tests-examples/55-tests/1.yaml
    python portfolio.py tests-examples/55-tests/1.yaml TODO!!!
    python cubes.py tests-examples/55-tests/1.yaml

### Parameters

You can use `-h` or `--help` in any of the 3 files to see what parameters are available.

### Important parameters

By default, `cubes.py` uses the number of available logical processors minus two. This can be changed using the `-j` option. For example, if you wish to use 8 threads, you would use `-j 8`.

It should be taken into account that without disabling some features (TODO say which?), `cubes.py` should not be run with less than 4 threads.

## Running all benchmarks present in the `tests` directory

    python helper-scripts/benchmark.py RUN_IDENTIFIER
    python helper-scripts/benchmark.py --portfolio RUN_IDENTIFIER
    python helper-scripts/benchmark.py --cubes RUN_IDENTIFIER

You should replace `RUN_IDENTIFIER` with a string that will be used as a name for the directory containing the log files for the benchmarks, as well as a CSV file containing the results.

### Important parameters

- `-t` specifies the time limit for each benchmark in seconds (600 seconds by default)
- `-m` specifies the memory limit for each benchmark in MB (57344 MB by default (56GB))

