#!/usr/bin/env python
# File:	squares-enumerator.py
# Description: An SQL Synthesizer Using Query Reverse Engineering
# Author:	Pedro M Orvalho
# Created on:	22-02-2019 15:13:15
# Usage:	python3 squaresEnumerator.py [flags|(-h for help)] specFile.in
# Python version:	3.6.4
import argparse
import os
import re
import sys

import rpy2.robjects as robjects
import sqlparse as sp

import tyrell.spec as S
from squares.Specification import parse_specification
from squares.interpreter import SquaresInterpreter
from tyrell.decider import Example, ExampleConstraintPruningDecider
from tyrell.enumerator import *
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

def main(args):
    logger.info('Parsing specification...')

    problem = parse_specification(args.input)

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

            print()
            if args.r:
                print("------------------------------------- R Solution ---------------------------------------\n")
                print(problem.r_init)
                print(interpreter.final_program)
                print()

            print("+++++++++++++++++++++++++++++++++++++ SQL Solution +++++++++++++++++++++++++++++++++++++\n")
            robjects.r(f'{problem.r_init + interpreter.final_program}')
            sql_query = robjects.r(f'sql_render({evaluation})')

            print(beautifier(str(sql_query)[6:]))
            return interpreter.final_program, beautifier(str(sql_query)[6:])

        else:
            logger.info('No more queries to be tested. Solution not found!')
            logger.info('Increasing the number of lines of code.')
            loc = loc + 1


debug = False
if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='A SQL Synthesizer Using Query Reverse Engineering')

    parser.add_argument('input', metavar='INPUT', type=str, help='input file')

    parser.add_argument('-d', '--debug', action='store_true', help="Print debug info.")

    parser.add_argument('--symm-on', dest='symm_on', action='store_true', help="compute symmetries online")
    parser.add_argument('--symm-off', dest='symm_off', action='store_true', help="compute symmetries offline")

    parser.add_argument('--r', dest='r', action='store_true', help="output R program")
    parser.add_argument('--no-r', dest='r', action='store_false', help="don't output R program")
    parser.set_defaults(r=True)

    parser.add_argument('--tree', dest='tree', action='store_true', help="use tree encoding")
    parser.add_argument('--lines', dest='tree', action='store_false', help="use line encoding")
    parser.set_defaults(tree=False)

    args = parser.parse_args()

    if args.debug:
        debug = True
        logger.setLevel('DEBUG')
    else:
        logger.setLevel('CRITICAL')

    prog = main(args)


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
