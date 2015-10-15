#!/usr/bin/python
# Converts a folder containing .qdimacs problems into thf problems.

import argparse
import os
import os.path
import csv
import sys
from subprocess import PIPE, check_output, CalledProcessError, call

import scheduler

class PreprocessorError(Exception):
    pass
class ConverterError(Exception):
    pass

def convert(infileFullName, infileName, outpath, prepNmr, command):
    print "Converting {} with {}.".format(infileName,command)
    with open(infileFullName,"r") as infile: 
        try:
            outfileFullName = os.path.join(outpath,str(prepNmr),infileName)
            prepFile = open(outfileFullName+".qdimacs", "w")
            returnCode = call(command, shell=True, stdin=infile, stdout=prepFile)
            prepFile.close()
            if not (returnCode == 0 or returnCode == 10 or returnCode == 20): 
                raise PreprocessorError() 
           
            # convert
            convertedFile = open(outfileFullName+".qdimacs","r")
            command = "./converter -o {}".format(outfileFullName+".thf")
            h = check_output(command, shell=True, stdin=convertedFile)
            h=h.split()
            statusFlag = "n"
            if len(h)==4:
                statusFlag = "s"
            return [int(h[0]), int(h[1]), statusFlag] 
        except OSError:
            print "[Error exec.: {}]".format(command)
        except CalledProcessError:
            print "[Called Process Error exec.: {}]".format(command)
        except ConverterError:
            print "[Converter Error: {}]".format(command)
        except PreprocessorError:
            print "[Preprocessor Error: {}]".format(command)
        return [0, 0, "e"]

def newRun(args):
    print "Starting new run."
    if not os.path.exists(args.output):
        os.makedirs(args.output)

    preprocessors = []
    # Read prprocessor description
    try:
        with open(args.preprocessors, 'rb') as csvFile:
            preprocessorReader = csv.reader(csvFile, delimiter=';')
            preprocessorReader.next() #skip header
            preprocessors=map(lambda x:x[1], preprocessorReader)
    except IndexError:
        print "Not enough columngs in preprocessor decleration."
        sys.exit(1)

    preprocessors.insert(0,"cat")

    for i in xrange(len(preprocessors)):
        path = os.path.join(args.output,str(i))
        if not os.path.exists(path):
            os.makedirs(path)

    preprocessors = zip(xrange(len(preprocessors)),preprocessors)

    tool = scheduler.Tool(args.results, [])
    for root, dirs, files in os.walk(args.input):
        for file in files:
            fileName, fileExtension = os.path.splitext(file)	
            if fileExtension == ".qdimacs":
                fullName = os.path.join(root,file)
                instance = scheduler.Instance(file,[fullName, fileName, args.output], preprocessors)
                tool.addInstance(instance)
   
    sche = scheduler.Scheduler(convert,[tool]) 
    sche.run()

parser = argparse.ArgumentParser(description='Recursively converts .qdimacs problems into thf problems.')
subparsers = parser.add_subparsers()
parser_start =  subparsers.add_parser('start')
parser_start.add_argument('input',type=str,
    help='the folder containing the problems.')
parser_start.add_argument('output',type=str,
    help='the target folder.')
parser_start.add_argument("-r", "--results", type=str, help="name for csv file containing the results.",
    default="results.csv")
parser_start.add_argument("-p","--preprocessors", help="CSV file containing the preprocessor commands.",
    type=str, default="preprocessors.csv")
parser_start.set_defaults(func=newRun)


args = parser.parse_args()
args.func(args)
