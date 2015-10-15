# resumable schaduler for conversion/benchmarking tasks

"""
    task is expected to return a list, which will be added to the
    csv file.
"""
import pickle
import csv
import signal
import time

class Instance:
    def __init__(self, name, constantArgs, versions):
        self.constantArgs = constantArgs
        self.versions = list(versions)
        self.name = name

    def getNewVersion(self): # throws an index error if no version is left
        top = self.versions.pop(0)
        return list(self.constantArgs) + list(top)

class Tool:
    def __init__(self, outputFilename, constantArgs):
        self.outputFilename = outputFilename
        self.constantArgs = constantArgs
    
        self.instances = []
        self.currentInstance = None
        self._res = []
        self.currentRow = []     
        self.currentVersion = None

        self._shdl = signal.getsignal(signal.SIGINT)
        signal.signal(signal.SIGINT, signal.SIG_IGN)

    def addInstance(self, instance):
        self.instances.append(instance)

    def runNext(self, task):
        if not self.currentVersion:
            try: 
                self.currentVersion = self.currentInstance.getNewVersion()
            except (AttributeError, IndexError):
                self.currentInstance = self.instances.pop(0) # throws an index error if no version is left
                print "====== Remaining instances: {} =====".format(len(self.instances)+1)
                self._res.append(self.currentRow)
                self.currentRow = [self.currentInstance.name]
                self.currentVersion = self.currentInstance.getNewVersion()
        signal.signal(signal.SIGINT, self._shdl)
        res = task(*(self.constantArgs+self.currentVersion))
        signal.signal(signal.SIGINT, signal.SIG_IGN)
        self.currentRow.extend(res)
        self.currentVersion = None
       
    def toCSV(self):
        with open(self.outputFilename, "wb") as csvfile:
            outfile = csv.writer(csvfile, delimiter=";")
            for row in self._res:
                outfile.writerow(row)

class Scheduler:
    def __init__(self, task, tools):
        self.task = task
        self.tools = tools
    
    def run(self):
        for tool in self.tools:
            while True:
                try:
                    tool.runNext(self.task)
                except IndexError:
                    tool.toCSV()
                    break
                except KeyboardInterrupt:
                    # This is not very secure, the interrupt has to happen during the run
                    # of the external tool -.-
                    print "Keyboard interrupt."
                    return
