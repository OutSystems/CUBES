#!/usr/bin/env python
# File:	squares-enumerator.py
# Description: An SQL Synthesizer Using Query Reverse Engineering
# Author:	Pedro M Orvalho
# Created on:	22-02-2019 15:13:15
# Usage:	python3 squaresEnumerator.py [flags|(-h for help)] specFile.in
# Python version:	3.6.4

import os
import re
import sys
import warnings
from sys import argv

import rpy2.robjects as robjects
import sqlparse as sp
from rpy2.rinterface import RRuntimeWarning

import tyrell.spec as S
from squares.Problem import parse_problem
from squares.interpreter import SquaresInterpreter
from tyrell.decider import Example, ExampleConstraintPruningDecider
from tyrell.enumerator import *
from tyrell.logger import get_logger
from tyrell.synthesizer import Synthesizer

warnings.filterwarnings("ignore", category=RRuntimeWarning)

logger = get_logger('tyrell')
counter_ = 0
distinct = False
_tables = dict()
output_attrs = ""
attributes = []
robjects.r('''
	library(dplyr)
	library(dbplyr)
	library(tidyr)
	library(stringr)
	options(warn=-1)
   ''')


def eq_r(actual, expect):
    global distinct
    _rscript = 'all.equal(lapply({lhs}, as.character),lapply({rhs}, as.character))'.format(lhs=actual, rhs=expect)
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

def main(seed=None):
    if not debug:
        sys.stderr = open(dir + 'output.err', 'w+')
    # os.close(sys.stderr.fileno())
    warnings.filterwarnings("ignore", category=RRuntimeWarning)
    warnings.filterwarnings('ignore')
    logger.info('Parsing Spec...')

    problem = parse_problem(argv[-1])

    spec = S.parse(problem.dsl)
    logger.info('Parsing succeeded')

    logger.info('Building synthesizer...')
    loc = 1
    while True:
        logger.info("Lines of Code: " + str(loc))
        if argv[1] == "tree":
            enumerator = SmtEnumerator(spec, depth=loc + 1, loc=loc)
        else:
            if "-off" in argv:
                enumerator = LinesEnumerator(spec, depth=loc + 1, loc=loc)
            elif "-on" in argv:
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
            logger.info('Solution found: {}'.format(prog))
            # print(prog_out+"select("+str(prog).replace("@param", "table")+","+output_attrs+")")
            # print(prog_out+str(prog).replace("@param", "table"))
            interpreter = SquaresInterpreter(problem, True)
            evaluation = interpreter.eval(prog, problem.tables)

            print()
            if "-nr" not in argv:
                print("------------------------------------- R Solution ---------------------------------------\n")
                print(problem.r_init)
                print(interpreter.final_program)
                print()
                print()
            print("+++++++++++++++++++++++++++++++++++++ SQL Solution +++++++++++++++++++++++++++++++++++++\n")
            robjects.r('{rscript}'.format(rscript=problem.r_init + interpreter.final_program))
            sql_query = robjects.r('sql_render({result_table})'.format(result_table=evaluation))

            print(beautifier(str(sql_query)[6:]))
            print()
            return interpreter.final_program, beautifier(str(sql_query)[6:])

        else:
            logger.info('No more queries to be tested. Solution not found!')
            logger.info('Increasing the number of lines of code.')
            loc = loc + 1


debug = False
if __name__ == '__main__':
    if "-d" in argv:
        debug = True
        print("Hey")
        logger.setLevel('DEBUG')
    else:
        logger.setLevel('CRITICAL')
    seed = None
    if "-h" in argv:
        exit(
            "Usage: python3 squaresEnumerator.py [tree|lines] [flags -h, ...] input.in\nflags:\n-on : computing symmetries online\n-off : computing symmetries offline\n-d : debug info\n\n-nr : only SQL solution\n\nDefault: lines enumerator and without symmetry breaking")
    if len(argv) > 1:
        try:
            seed = int(argv[1])
        except ValueError:
            pass
    prog = main(seed)


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
