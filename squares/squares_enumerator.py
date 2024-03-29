import contextlib
import logging
import signal
import time
from multiprocessing import Queue

import rpy2
import rpy2.robjects as robjects

from squares.dsl.interpreter import SquaresInterpreter
from . import util, results
from .config import Config
from .decider import LinesDecider
from .disambiguating_synthesizer import DisambiguatingSynthesizer
from .disambiguator import FuzzingDisambiguator
from .tyrell.decider import Example
from .tyrell.enumerator.bitenum import BitEnumerator
from .tyrell.enumerator.input_enumerator import InputEnumerator
from .tyrell.synthesizer import Synthesizer

logger = logging.getLogger('squares')

robjects.r('''
sink("/dev/null")
options(warn=-1)
suppressMessages(library(tidyr))
suppressMessages(library(stringr))
suppressMessages(library(readr))
suppressMessages(library(lubridate))
suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(purrr))''')


def do_not_print(msg):
    pass


rpy2.rinterface_lib.callbacks.consolewrite_print = do_not_print
rpy2.rinterface_lib.callbacks.consolewrite_warnerror = do_not_print


def main(args, specification, id: int, conf: Config, queue: Queue):
    signal.signal(signal.SIGINT, signal.SIG_DFL)
    signal.signal(signal.SIGTERM, signal.SIG_DFL)

    util.seed(conf.seed)
    util.store_config(conf)
    util.set_program_queue(queue)

    if args.verbose >= 1:
        logger.setLevel('INFO')
    if args.verbose >= 2:
        logger.setLevel('DEBUG')
    if args.verbose >= 3:
        logging.getLogger('tyrell').setLevel('DEBUG')

    results.specification = specification

    spec = specification.generate_dsl()

    decider = LinesDecider(interpreter=SquaresInterpreter(specification),
                           examples=[Example(input=specification.input_table_names, output='expected_output')],
                           )

    logger.info('Building synthesizer...')
    loc = max(specification.min_loc, conf.minimum_loc)
    while loc <= util.get_config().maximum_loc:
        start = time.time()
        enumerator = BitEnumerator(spec, specification, loc=loc) if loc > 0 else InputEnumerator(spec, specification)
        results.init_time += time.time() - start

        synthesizer = Synthesizer(enumerator=enumerator, decider=decider)

        if util.get_config().disambiguate:
            disambiguator = FuzzingDisambiguator(16)
            disambiguating = DisambiguatingSynthesizer(synthesizer, disambiguator)
            synth = disambiguating
        else:
            synth = synthesizer

        found = False
        if util.get_config().enum_until is None:
            for prog, _ in synth.synthesize(util.get_config().top_programs):
                if prog:
                    logger.info(f'Solution found: {prog}')
                    queue.put((util.Message.SOLUTION, id, prog, loc, True))
                    found = True
        else:
            for prog, _ in synthesizer.synthesize(enum_all=True):
                if prog:
                    logger.info(f'Solution found: {prog}')
                    queue.put((util.Message.SOLUTION, id, prog, loc, True))
                    found = True

        if found and util.get_config().enum_until is None:
            queue.put((util.Message.DONE, loc, None, None, None))
            return
        else:
            logger.info('Increasing the number of lines of code to %d.', loc + 1)
            loc = loc + 1

    results.exceeded_max_loc = True
    queue.put((util.Message.NO_SOLUTION, loc, None, None, None))
    logger.error('Process %d reached the maximum number of lines (%d). Giving up...', id, util.get_config().maximum_loc)
