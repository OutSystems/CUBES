#!/usr/bin/env python
# File:	squares-enumerator.py
# Description: An SQL Synthesizer Using Query Reverse Engineering
# Author:	Pedro M Orvalho
# Created on:	22-02-2019 15:13:15
# Usage:	python3 squaresEnumerator.py [flags|(-h for help)] specFile.in
# Python version:	3.6.4
import argparse
import logging
import os
import random
import re
from multiprocessing import Process, SimpleQueue
from time import sleep

import rpy2.robjects as robjects
import sqlparse as sp

import tyrell.spec as S
from squares import util
from squares.SQLVisitor import SQLVisitor
from squares.Specification import parse_specification
from squares.interpreter import SquaresInterpreter
from tyrell.decider import Example, ExampleConstraintPruningDecider
from tyrell.enumerator import SmtEnumerator, LinesEnumerator
from tyrell.logger import get_logger
from tyrell.synthesizer import Synthesizer

# warnings.filterwarnings("ignore", category=RRuntimeWarning)

logger = get_logger('tyrell')
distinct = False
output_attrs = ""
robjects.r('''
zz <- file("r_output.log", open = "wt")
sink(zz, type = "message")
library(dplyr)
library(dbplyr)
library(tidyr)
library(stringr)
options(warn=-1)''')


def eq_r(actual, expect):
    global distinct
    _rscript = f'all.equal(lapply({actual}, as.character), lapply({expect}, as.character))'
    try:
        ret_val = robjects.r(_rscript)
    except:
        return False
    return True == ret_val[0]


index_table_aux = 0


def beautifier(sql):
    # parsed = sp.parse(sql)
    # new_sql = beautifier_aux(parsed[0])
    sql = re.sub('`TBL_LEFT`\.`[^,`]*` AS |`LHS`\.`[^,`]*` AS ', "", sql)
    sql = re.sub('`TBL_RIGHT`\.`[^,`]*` AS |`RHS`\.`[^,`]*` AS ', "", sql)
    return sp.format(sql, reindent=True, keyword_case='upper')


# print(sp.format(new_sql, reindent=True, keyword_case='upper'))

def main(args, seed, id, config, queue):
    util.seed(seed)

    logger.info('Parsing specification...')

    problem = parse_specification(args.input, config)

    if logger.isEnabledFor(logging.DEBUG):
        with open(f'dsl{id}.log', 'w') as f:
            f.write(repr(problem.dsl))

    spec = S.parse(repr(problem.dsl))
    logger.info('Parsing succeeded')

    logger.info('Building synthesizer...')
    loc = 1
    while True:
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

        synthesizer = Synthesizer(
            # loc: # of function productions
            enumerator=enumerator,
            # decider=ExampleConstraintDecider(
            decider=ExampleConstraintPruningDecider(
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

            sql_generator = SQLVisitor()
            sql = sql_generator.eval(prog, problem.tables)
            print(sql)

            program = problem.r_init + interpreter.final_program
            robjects.r(program)
            sql_query = robjects.r(f'sql_render({evaluation})')

            queue.put((problem.r_init + '\n' + interpreter.final_program, beautifier(str(sql_query)[6:])))
            return

        else:
            logger.info('No more queries to be tested. Solution not found!')
            logger.info('Increasing the number of lines of code.')
            loc = loc + 1


debug = False
if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='A SQL Synthesizer Using Query Reverse Engineering')

    parser.add_argument('input', metavar='SPECIFICATION', type=str, help='specification file')

    parser.add_argument('-d', '--debug', action='store_true', help="Print debug info.")

    g = parser.add_mutually_exclusive_group()
    g.add_argument('--symm-on', dest='symm_on', action='store_true', help="compute symmetries online")
    g.add_argument('--symm-off', dest='symm_off', action='store_true', help="compute symmetries offline")

    g = parser.add_mutually_exclusive_group()
    g.add_argument('--r', dest='r', action='store_true', help="output R program")
    g.add_argument('--no-r', dest='r', action='store_false', help="don't output R program")
    parser.set_defaults(r=True)

    g = parser.add_mutually_exclusive_group()
    g.add_argument('--tree', dest='tree', action='store_true', help="use tree encoding")
    g.add_argument('--lines', dest='tree', action='store_false', help="use line encoding")
    parser.set_defaults(tree=False)

    parser.add_argument('--seed', default='squares')

    args = parser.parse_args()

    if args.debug:
        debug = True
        logger.setLevel('DEBUG')
    else:
        logger.setLevel('CRITICAL')

    random.seed(args.seed)

    configs = [
        {'disabled': []},
        {'disabled': ['inner_join', 'inner_join3']},
        {'disabled': ['inner_join', 'inner_join4']},
        {'disabled': ['inner_join3', 'inner_join4']}
    ]

    queue = SimpleQueue()

    Ps = []
    for i in range(len(configs)):
        P = Process(target=main, args=(args, random.randrange(2 ** 31), i, configs[i], queue))
        P.start()
        Ps.append(P)

    done = False
    while not done and Ps:
        sleep(.5)
        for p in Ps:
            if not p.is_alive():
                if not queue.empty():
                    done = True
                    break
                Ps.remove(p)

    for p in Ps:
        p.terminate()

    if not queue.empty():
        r, sql = queue.get()

        print()
        if args.r:
            print("------------------------------------- R Solution ---------------------------------------\n")
            print(r + '\n')

        print("+++++++++++++++++++++++++++++++++++++ SQL Solution +++++++++++++++++++++++++++++++++++++\n")
        print(sql)
    else:
        print("No solution found")


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
