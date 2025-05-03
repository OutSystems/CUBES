# Description of included files

- `analysis`: directory where log files will be stored
- `helper-scripts`: useful script for running several benchmarks or extracting information from log files
- `squares`: source code for the tool
- `tests-examples`: directory that contains all benchmarks
- `tests`: directory that is used to specify which benchmarks should be run when using the helper scripts. Sub-directories from tests-examples can either be symlinked to here, or copied over.
- `sequential.py`, `cubes.py`: files to run the corresponding configurations.
- `miniconda.sh`, `conda.txt`: setup files containing all required dependencies

# Installation

Install and setup dependencies:

    source setup.sh

The following commands should be executed every time a new shell is opened after the initial setup:
    
    eval "$(/home/tacas23/anaconda/bin/conda shell.bash hook)"
    conda activate cubes

# Running
## Running a single benchmark
You can use the `sequential.py` or `cubes.py` to run either the sequential, or the parallel version of CUBES, respectively:

    python sequential.py tests-examples/55-tests/1.yaml
    python cubes.py tests-examples/55-tests/1.yaml

You can use `-h` or `--help` in any of the 2 files to see what configuration options are available.

### Important parameters

By default, `cubes.py` uses the number of available logical processors minus two. This can be changed using the `-j` option. For example, if you wish to use 8 threads, you would use `-j 8`.

By default, `cubes.py` and `sequential.py` will terminate after finding the first solution. If you wish to use the disambiguator, you should specify how long to search for solutions before terminating using `--under=TIME_IN_SECONDS`. 

It should be taken into account that without disabling some features `cubes.py` should not be run with less than 4 threads.
- Recommended settings for 3 threads: `--no-split-complex-joins`
- Recommended settings for 2 threads: `--no-split-complex-joins --probing-threads=1`
- Recommended settings for 1 thread: `--no-split-complex-joins --probing-threads=0`

## Running all benchmarks present in the `tests` directory
**Remember to symlink/copy the instances you want to test from `tests-examples` to `tests`**  
You can use the benchmark script to evaluate either the sequential, or the parallel version of CUBES, respectively:

    python helper-scripts/benchmark.py RUN_IDENTIFIER
    python helper-scripts/benchmark.py --cubes RUN_IDENTIFIER

You should replace `RUN_IDENTIFIER` with a string that will be used as a name for the directory containing the log files for the benchmarks, as well the name for the CSV file containing the results.

### Important parameters

- `-t` specifies the time limit for each benchmark in seconds (600 seconds by default)
- `-m` specifies the memory limit for each benchmark in MB (57344 MB by default (56GB))
- `-p` specifies how many instances to test in parallel (note that this is unrelated to the `-j` argument of `cubes.py`)
- other arguments are redirected to `sequential.py` / `cubes.py`

## Running the disambiguator

At this point disambiguation is implemented as a post-processing step after cubes has generated all solutions under the desired time limit and requires the `benchmark.py` script to have been used.

To perform disambiguation you can use the following command, where `RUN_IDENTIFIER` should be a previously executed run:

```PYTHONPATH=. python ./helper-scripts/disambiguation_post.py RUN_IDENTIFIER```

You can use `--help` to see all configuration options available in the disambiguation script.

## Running the fuzzy evaluation

Like the disambiguation script, accuracy requires instances to have been run using the `benchmark.py` script introduced before.

To perform fuzzy evaluation you can use the following command, where `RUN_IDENTIFIER` should be a previously executed run:

```PYTHONPATH=. python ./helper-scripts/fuzzy_check.py --run=RUN_IDENTIFIER```

If you want to perform fuzzy evaluation over the results of a previous disambiguation you can use:

```PYTHONPATH=. python ./helper-scripts/fuzzy_check.py --from-dis --run=RUN_IDENTIFIER```

You can use `--help` to see other configuration options available in the fuzzy evaluation script.



