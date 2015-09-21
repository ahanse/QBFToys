#!/usr/bin/python
# Takes a list of csv files and runs a benchmark :P

import argparse
import os
import os.path
import csv
import sys
from subprocess import PIPE, check_output, CalledProcessError, call
import re
import time

import scheduler

def readPreprocessorDescription(fileName):
    preprocessors = []
    # Read preprocessor description
    try:
        with open(fileName, 'rb') as csvFile:
            preprocessorReader = csv.reader(csvFile, delimiter=';')
            preprocessorReader.next() #skip header
            preprocessors=map(lambda x:x[1], preprocessorReader)
    except IndexError:
        print "Not enough columngs in preprocessor decleration."
        sys.exit(1)
    preprocessors.insert(0,"cat")
    return preprocessors 

def readResultLine(line):
    result = [line[0]]
    for i in xrange((len(line)-1)/3):
        n = 3*i+1
        record = (line[n], line[n+1], line[n+2])
        result.append(record)
    return result

def readResults(fileName):
    results = []
    try:
        with open(fileName, 'rb') as csvFile:
            restultReader = csv.reader(csvFile, delimiter=';')
            restultReader.next() #skip header
            results=map(lambda x:readResultLine(x), restultReader)
    except IndexError:
        print "Not enough columns in result decleration."
        sys.exit(1)
    return results 

def readSolvers(fileName):
    solvers = []
    # Read solver description
    try:
        with open(fileName, 'rb') as csvFile:
            solverReader = csv.reader(csvFile, delimiter=';')
            solverReader.next() #skip header
            solvers=map(lambda x:x[1], solverReader)
    except IndexError:
        print "Not enough columns in solver decleration."
        sys.exit(1)
    return solvers 

def runSolver(solver, benchmarkFile, logFileName, timeout):
    solverCmd = solver.replace("%t", str(timeout)).replace("%f", benchmarkFile)
    result = 'u'
    td = 0
    with open(logFileName, 'w') as logFile:
        ts = time.time()
        returnCode = call(solverCmd, shell=True, stdout=logFile) 
        td = time.time() - ts
        if returnCode == 20: # 20 is theorem, 10 count. ex. 
           result = 't' 
        if returnCode == 10:
           result = 'c'
    return result, ts

""" States: b...to big
            e...error
            t...solved
            c...countersat.
            u...unsolved 
            p...preprocessor error """
def doBenchmark(folder, maxClauses, maxVars, timeout, 
              benchmarkFilename, variables, clauses, state, solverNmr, cmd):
    logFileName = "{}_{}.log".format(benchmarkFilename, str(solverNmr))
    ext = ".thf"
    if solverNmr==0:
        ext = ".qdimacs"
    benchmarkFile=benchmarkFilename+ext
    if state == "n":
        if int(clauses) <= maxClauses and int(variables) <= maxVars:
            print "Running {} on {}.".format(cmd, benchmarkFile)
            result,t = runSolver(cmd, benchmarkFile, logFileName, timeout)
            return [state,result,t]
        else: 
            print "Skiping {} on {}: Too Big.".format(cmd, benchmarkFile)
            return [state,'b',0]
    else:
        print "Skiping {} on {}.".format(cmd, benchmarkFile)
        return [state,'e',0]


def newRun(args):
    print "Starting new run."
    preprocessors = readPreprocessorDescription(args.preprocessors)
    solvers = readSolvers(args.solver)
    solvers = [args.qbf]+solvers
    solvers = zip(xrange(len(solvers)),solvers)
    results = readResults(args.results)

    tools = []
    for p in xrange(len(preprocessors)):
        folder = os.path.join(args.input, str(p))
        tool = scheduler.Tool(os.path.join(folder, "result.csv"),
            [folder, args.clauses, args.variables, args.timeout])

        for benchmark in results:
            filename = benchmark[0]
            benchmarkFile = os.path.join(folder, filename)
            benchmarkFilename, benchmarkFileExtension = os.path.splitext(benchmarkFile)
            (variables, clauses, state) = benchmark[p+1]
            
            instance = scheduler.Instance(filename, 
                [benchmarkFilename, variables, clauses, state],solvers)
            tool.addInstance(instance)
        tools.append(tool)
    sche = scheduler.Scheduler(doBenchmark,tools, args.maxtemp,args.continuetemp, args.autosave)
    sche.run()

def resumeRun(args):
    print "Restoring from", args.savedState
    sche = scheduler.restoreScheduler(args.savedState)
    print "Resuming"
    sche.run()

parser = argparse.ArgumentParser(description='Automatically runs a benchmark of HOL and QBF solver.')
subparsers = parser.add_subparsers()

parser_start =  subparsers.add_parser('start')
parser_start.add_argument('input',type=str,
    help='the folder containing the problems.')
parser_start.add_argument("-p", "--preprocessors", type=str, 
    help="name for csv file containing the used preprocessors (for pretty printing).")
parser_start.add_argument("-r", "--results", type=str, 
    help="CSV file containing the result of the preprocessing.")
parser_start.add_argument("-s","--solver", help="CSV file specifying he solvers to use.", type=str)
parser_start.add_argument("-v", "--variables", type=int, 
    help="max. number of variables.")
parser_start.add_argument("-c", "--clauses", type=int, 
    help="max. number of clauses.")
parser_start.add_argument("-t", "--timeout", type=str, 
    help="timeout for each solver in seconds.")
parser_start.add_argument("-q", "--qbf", type=str, 
    help="QBF solver used as reference")
parser_start.add_argument("-m","--maxtemp", help="If the temperature is reached, the script waits until the temperature is below 'continuetemp'", type=int, default=0)
parser_start.add_argument("-o","--continuetemp", help="If the temperature falls bellow this value, the script is continued.", type=int, default=0)
parser_start.add_argument("-a","--autosave", help="Saves the state all N converted files.", type=int, default=0)
parser_start.set_defaults(func=newRun)

parser_resume =  subparsers.add_parser('resume')
parser_resume.add_argument('savedState',type=str,
    help='File containing a previously saved state.')
parser_resume.set_defaults(func=resumeRun)

args = parser.parse_args()
args.func(args)
