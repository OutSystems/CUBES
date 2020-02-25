# File:	squares-enumerator.py
# Description: An SQL Synthesizer Using Query Reverse Engineering
# Author:	Pedro M Orvalho
# Created on:	22-02-2019 15:13:15
# Usage:	python3 squaresEnumerator.py [flags|(-h for help)] specFile.in
# Python version:	3.6.4
import logging
import os
import re
from multiprocessing import Queue

import rpy2.robjects as robjects
import sqlparse as sp

import tyrell.spec as S
from squares import util
from squares.SQLVisitor import SQLVisitor
from squares.Specification import parse_specification
from squares.config import Config
from squares.interpreter import SquaresInterpreter
from tyrell.decider import Example, ExampleConstraintPruningDecider, ExampleConstraintDecider
from tyrell.enumerator import LinesEnumerator, SmtEnumerator
from tyrell.logger import get_logger
from tyrell.synthesizer import Synthesizer

# warnings.filterwarnings("ignore", category=RRuntimeWarning)

logger = get_logger('squares')

robjects.r('''
zz <- file("r_output.log", open = "wt")
sink(zz)
sink(zz, type = "message")
library(dplyr)
library(dbplyr)
library(tidyr)
library(stringr)
options(warn=-1)''')


def eq_r(actual, expect):
    _rscript = f'all_equal(lapply({actual}, as.character), lapply({expect}, as.character))'
    try:
        ret_val = robjects.r(_rscript)
    except:
        return False
    return True == ret_val[0]


def beautifier(sql):
    # parsed = sp.parse(sql)
    # new_sql = beautifier_aux(parsed[0])
    sql = re.sub('`TBL_LEFT`\.`[^,`]*` AS |`LHS`\.`[^,`]*` AS ', "", sql)
    sql = re.sub('`TBL_RIGHT`\.`[^,`]*` AS |`RHS`\.`[^,`]*` AS ', "", sql)
    return sp.format(sql, reindent=True, keyword_case='upper')


def main(args, id: int, conf: Config, queue: Queue, limit: int):
    util.seed(conf.seed)
    util.store_config(conf)

    if args.debug:
        logger.setLevel('DEBUG')
        get_logger('tyrell').setLevel('DEBUG')

    logger.handlers[0].set_identifier(f'prc{id}')

    logger.info('Parsing specification...')

    problem = parse_specification(args.input)

    if logger.isEnabledFor(logging.DEBUG):
        with open(f'dsl{id}.tyrell', 'w') as f:
            f.write(repr(problem.dsl))

    spec = S.parse(repr(problem.dsl))
    logger.info('Parsing succeeded')

    logger.info('Building synthesizer...')
    loc = 1
    while loc <= limit:
        logger.info("Lines of Code: " + str(loc))
        if args.tree:
            enumerator = SmtEnumerator(spec, depth=loc + 1, loc=loc)
        else:
            if args.symm_off:
                enumerator = LinesEnumerator(spec, depth=loc + 1, loc=loc)
            elif args.symm_on:
                enumerator = LinesEnumerator(spec, depth=loc + 1, loc=loc, break_sym_online=True)
            else:
                enumerator = LinesEnumerator(spec, depth=loc + 1, loc=loc, sym_breaker=False)

        # enumerator = ExhaustiveEnumerator(spec, loc)

        synthesizer = Synthesizer(
            # loc: # of function productions
            enumerator=enumerator,
            decider=ExampleConstraintDecider(
            # decider=ExampleConstraintPruningDecider(
                spec=spec,
                interpreter=SquaresInterpreter(problem, False),
                examples=[
                    Example(input=problem.tables, output='expected_output'),
                ],
                equal_output=eq_r
            )
        )
        logger.info('Synthesizing programs...')

        prog = synthesizer.synthesize()
        if prog is not None:
            logger.info(f'Solution found: {prog}')
            interpreter = SquaresInterpreter(problem, True)
            evaluation = interpreter.eval(prog, problem.tables)

            try:
                program = problem.r_init + interpreter.final_program
                robjects.r(program)
                sql_query = robjects.r(f'sink(); sql_render({evaluation})')
            except Exception:
                logger.error('Error while trying to convert R code to SQL.')
                sql_query = None

            queue.put((problem.r_init + '\n' + interpreter.final_program, None if sql_query is None else beautifier(str(sql_query)[6:]), id))
            return

        else:
            logger.info('No more queries to be tested. Solution not found!')
            logger.info('Increasing the number of lines of code.')
            loc = loc + 1

    logger.error('Process %d reached the maximum number of lines (%d). Giving up...', id, limit)


class Squares(object):
    """docstring for Squares."""

    def __init__(self):
        super(Squares, self).__init__()
        self.template = "inputs: {inputs}\noutput: {output}\nconst: {const}\naggrs: {aggrs}\nattrs: {attrs}\nbools:\nloc: {loc}\n"

    def synthesize(self, inputs, output_ex, const="", aggrs="", attrs="", loc=0):
        """docstring for Squares."""
        global argv, dir
        dir = "../"
        ins = list([])
        temp = self.template

        try:
            path, dirs, files = next(os.walk("../users/files"))
        except:
            path, dirs, files = next(os.walk("users/files"))
            dir = "./"
        file_count = str(len(files) + 1)

        i_c = 0
        for i in inputs:
            input = open(dir + "users/tables/" + "i" + str(file_count) + str(i_c), "w+")
            input.write(i)
            input.close()
            ins.append(dir + "users/tables/" + "i" + str(file_count) + str(i_c))
            i_c += 1
        output = open(dir + "users/tables/" + "o" + str(file_count), "w+")
        output.write(output_ex)
        output.close()
        output = dir + "users/tables/o" + str(file_count)

        input_file_name = dir + "users/files/" + "f" + str(file_count)
        input_file = open(input_file_name, "w+")
        inputs = str(ins).replace("\'", "").replace("]", "").replace("[", "")
        input_file.write(
            temp.format(inputs=inputs, output=output, const="\"" + const.replace(",", "\",\"").replace(" ", "") + "\"",
                        aggrs="\"" + aggrs.replace(",", "\",\"").replace(" ", "") + "\"",
                        attrs="\"" + attrs.replace(",", "\",\"").replace(" ", "") + "\"", loc=str(loc)).replace("\"\"",
                                                                                                                ""))
        input_file.close()

        argv = []
        argv.append("lines")
        argv.append(input_file_name)
        return main()

# # not used
# def beautifier_aux(tokens):
# 	# print(tokens)
# 	global index_table_aux
# 	sub_query = ""
# 	left_index = right_index = None
# 	for t in tokens:
# 		if "(SELECT" in str(t):
# 			if "AS `TBL_RIGHT`" == str(t)[-13:]:
# 				right_index = index_table_aux
# 				index_table_aux += 1
# 			elif "AS `TBL_LEFT`" == str(t)[-13:]:
# 				left_index = index_table_aux
# 				index_table_aux += 1
# 		if "`TBL_LEFT`" in str(t):
# 			left_index = index_table_aux
# 			index_table_aux += 1
# 		if "`TBL_RIGHT`" in str(t):
# 			right_index = index_table_aux
# 			index_table_aux += 1
# 	for t in tokens:
# 		if "(SELECT" in str(t):
# 			# print(t)
# 			if "AS `TBL_RIGHT`" == str(t)[-13:]:
# 				aux_str = str(t).split("AS `TBL_RIGHT`")
# 				new_input = sp.parse(aux_str[0])[0]
# 				# print("RIGHT", t, "-->", new_input)
# 				sub_query += beautifier_aux(new_input) + " AS " + "table_"+str(right_index)
# 			elif "AS `TBL_LEFT`" == str(t)[-13:]:
# 				aux_str = str(t).split("AS `TBL_LEFT`")
# 				new_input = sp.parse(aux_str[0])[0]
# 				# print("LEFT", t, "-->", new_input)
# 				sub_query += beautifier_aux(new_input) + " AS " + "table_"+str(left_index)
# 			else:
# 				sub_query += beautifier_aux(t)
# 		else:
# 			sub_query += str(t).replace("`TBL_LEFT`", "table_"+str(left_index)).replace("`TBL_RIGHT`", "table_"+str(right_index))
# 	return sub_query
