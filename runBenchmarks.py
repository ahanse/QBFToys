#!/usr/bin/python
# Takes a list of csv files and runs a benchmark :P

import argparse
import os
import os.path
import csv
import sys
from subprocess import PIPE, check_output, CalledProcessError, call
import re

def readPreprocessorDescription(fileName):
    preprocessors = []
    # Read prprocessor description
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
        print "Not enough columngs in restult decleration."
        sys.exit(1)
    return results 

def readSolvers(fileName):
    solvers = []
    # Read prprocessor description
    try:
        with open(fileName, 'rb') as csvFile:
            solverReader = csv.reader(csvFile, delimiter=';')
            solverReader.next() #skip header
            solvers=map(lambda x:x[1], solverReader)
    except IndexError:
        print "Not enough columngs in solver decleration."
        sys.exit(1)
    return solvers 

def runSolver(solver, benchmarkFile, logFileName, timeout):
    solverCmd = solver.replace("%t", str(timeout)).replace("%f", benchmarkFile)
    result = 'e'
    with open(logFileName, 'w') as logFile:
        returnCode = call(solverCmd, shell=True, stdout=logFile) 
        if returnCode == 20 or returnCode == 10: # 20 is theorem, 10 count. ex. 
           result = 's' 
    return result  

parser = argparse.ArgumentParser(description='Automatically runs a benchmark of HOL and QBF solver.')
parser.add_argument('input',type=str,
    help='the folder containing the problems.')
parser.add_argument("-p", "--preprocessors", type=str, 
    help="name for csv file containing the used preprocessors (for pretty printing).")
parser.add_argument("-r", "--results", type=str, 
    help="CSV file containing the result of the preprocessing.")
parser.add_argument("-s","--solver", help="CSV file specifying he solvers to use.", type=str)
parser.add_argument("-v", "--variables", type=str, 
    help="max. number of variables.")
parser.add_argument("-c", "--clauses", type=str, 
    help="max. number of clauses.")
parser.add_argument("-t", "--timeout", type=str, 
    help="timeout for each solver in seconds.")
args = parser.parse_args()

preprocessors = readPreprocessorDescription(args.preprocessors)
solvers = readSolvers(args.solver)
results = readResults(args.results)

""" States: b...to big
            e...error
            s...solved
            u...unsolved """"

for p in xrange(len(preprocessors)):
    folder = os.path.join(args.input, str(p))
    with open(os.path.join(folder,"results.csv"),"wb") as outputFile:
        output = csv.writer(outputFile, delimiter=";")
        for benchmark in results:
            filename = benchmark[0]
            benchmarkFile = os.path.join(folder, filename)
            benchmarkFilename, benchmarkFileExtension = os.path.splitext(benchmarkFile)
            solverResult = []
            for s in xrange(len(solvers)):
                solver = solvers[s]
                logFileName = "{}_{}.log".format(benchmarkFilename, str(s))
                (variables, clauses, state) = benchmark[p+1]
                if state == "n":
                    if clauses <= args.clauses and variables <= args.variables:
                        print "Running {} on {}.".format(solver, benchmarkFile)
                        result = runSolver(solver, benchmarkFile, logFileName, timeout)
                        solverResult.extend([state,result])
                    else: 
                        print "Skiping {} on {}: To Big.".format(solver, benchmarkFile)
                        solverResult.extend([state,'b'])
                else:
                    print "Skiping {} on {}.".format(solver, benchmarkFile)
                    solverResult.extend([state,'e'])
            output.write(solverResult)
