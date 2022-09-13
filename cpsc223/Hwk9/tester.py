#!/usr/bin/python3
import os
import tempfile
import signal
import subprocess
import resource
import string
import re
import stat
import time
import math
import sys
import functools

class _testerCaughtAlarm(Exception):
    pass

def signalName(sig):
    """Convert numerical signal number to name."""
    # grotesque hack suggested by ssc on stackoverflow.com
    return [v for v, k in signal.__dict__.items() if k == sig][0]

class Tester:
    reportHeader = 'Score: {}/{}\nLateness: {} ({:.2f}%)\nTotal: {}/{}'
    logHeaderFormat = '{:4s}{:3d} {:68s} {}'
    maxCommandLen = 68
    signalReport = '    Caught signal {}'
    statusReport = '  Bad exit status {}'
    outputLimit = 200 
    defaultTimeLimit = 1
    ignoreLateFiles = ['feedback', '.GRADED', 'RCS']
    lateness_interval = 50*3600 # how many seconds until all points are lost

    def __init__(self, dueDate = None, latePenalty = None, gracePeriod = 86400, latenessRate = None, dir = '.'):
        self.points = 0         # points for successful tests
        self.maxpoints = 0      # max possible score
        self._report = []       # summary of tests
        # set up playground
        # generate a unique temp name with suffix .tester
        self.tmpdir = tempfile.mktemp('.tester')
        # create the directory with 0700 permissions
        os.mkdir(self.tmpdir, 0o700)
        # calculate lateness; this rises from 0.0 to 1.0 as lateness_interval proceeds
        if dueDate != None:
            if latenessRate == None:
                # old-style calculation
                self.lateness = min(max(0.0, (self.lastMtime(dir) - dueDate) / self.lateness_interval), 1.0)
            else:
                # fancy new calculation
                mtime = self.lastMtime(dir)
                penalty = 0.0

                # take immediate hit
                if mtime > dueDate and latePenalty != None:
                    penalty += latePenalty

                # take further hit if after grace period
                if mtime > dueDate + gracePeriod and latenessRate != None:
                    penalty += latenessRate * (mtime - (dueDate + gracePeriod)) / 3600.

                self.lateness = min(1.0, penalty / 100.0)
        else:
            self.lateness = 0.0

    def __del__(self):
        # delete tmpdir and its contents
        os.system('''rm -rf "{}"'''.format(self.tmpdir))

    # adds the base name of source file to the temp dir
    # to create an absolute pathname for it
    def _tmpname(self, file):
        return os.path.join(self.tmpdir, os.path.basename(file))

    def chmod(self, file, mode):
        """Change mode of file in temp directory."""
        os.chmod(self._tmpname(file), mode)

    def copy(self, file, destination = None, mode = None):
        """Makes a copy of the source file in the temp dir created 
           in _init__.  Optional destination gives new name.
           Optional mode gives mode for chmod."""
        if destination == None:
            destination = file
        self.write(destination, open(file, "rb").read())
        if mode != None:
            self.chmod(destination, mode)

    def lastMtime(self, dir):
        max = 0
        for file in os.listdir(dir):
            if file in self.ignoreLateFiles:
                continue
            file = os.path.join(dir, file)
            fileStat = os.lstat(file)
            if stat.S_ISREG(fileStat[stat.ST_MODE]):
                if fileStat[stat.ST_MTIME] > max:
                    max = fileStat[stat.ST_MTIME]
        return max

    def copyDir(self, dir):
        """Run self.copy on all plain files in dir."""
        for file in os.listdir(dir):
            file = os.path.join(dir, file)
            if stat.S_ISREG(os.lstat(file)[stat.ST_MODE]):
                self.copy(file)

    def copyDirFollowLinks(self, dir):
        """Run self.copy on all plain files in dir, following links."""
        for file in os.listdir(dir):
            file = os.path.join(dir, file)
            if stat.S_ISREG(os.stat(file)[stat.ST_MODE]):
                self.copy(file)

    def delete(self, file):
        os.unlink(self._tmpname(file))

    def write(self, file, contents):
        open(self._tmpname(file), "wb").write(contents)

    def _runAlarmHandler(self, signum, frame):
        raise _testerCaughtAlarm("caught alarm")

    def run(self, command, timeLimit = None, input=None):
        try:
            olddir = os.getcwd()
        except:
            olddir = None
        oldhandler = signal.signal(signal.SIGALRM, self._runAlarmHandler)
        if timeLimit == None:
            timeLimit = self.defaultTimeLimit
        try:
            os.chdir(self.tmpdir)
            signal.alarm(timeLimit+1)             # kludge; hit XCPU first
            status = None
            output = None
            if input != None:
                stdin = subprocess.PIPE
            else:
                stdin = None
            try:
                # XCPU limit should only apply in child
                child = subprocess.Popen('{ ' + command + '; } 2>&1', shell=True, stdout=subprocess.PIPE, stdin=stdin, preexec_fn = functools.partial(resource.setrlimit, resource.RLIMIT_CPU, (timeLimit, -1)))
                try:
                    # stderr is a dummy
                    (output, stderr) = child.communicate(input)
                except IOError:
                    output = 'Error reading output' 
                status = child.wait()
            except _testerCaughtAlarm:
                # subprocess uses -alarm to indicate caught alarms
                status = -signal.SIGALRM
        finally:
            signal.alarm(0)
            signal.signal(signal.SIGALRM, oldhandler)
            if olddir:
                os.chdir(olddir)
            resource.setrlimit(resource.RLIMIT_CPU, (-1, -1))
            try:
                os.kill(child.pid, signal.SIGKILL)
                child.poll()
            except:
                pass
        return status, output

    def test(self, points, command, outputTest = None, timeLimit = None):
        """Runs test.  Calls testOK if test succeeded,
        or calls testFail if test failed.  Returns true if test succeeded."""
        status, output = self.run(command, timeLimit)
        # did it work?
        if status != 0 or (outputTest and not outputTest(output)):
            # failure!
            self.testFail(points, command, status, output, outputTest)
            return 0
        else:
            self.testOK(points, command, status, output, outputTest)
            return 1

    def testFail(self, points, command, status = 0, output = '', test = None):
        """Log a test failure."""
        self.maxpoints = self.maxpoints + points
        self.logHeader('FAIL', points, command, test)
        if status < 0:
            # report signal
            self.log(self.signalReport.format(signalName(-status)))
        elif status > 0:
            self.log(self.statusReport.format(status))
        if output:
            # report (possibly truncated) output
            # output = output.strip()
            if len(output) > self.outputLimit:
                self.log(output[:self.outputLimit] + b'\n[...]\n' + output[-self.outputLimit:])
            else:
                self.log(output)

    def testOK(self, points, command, status = 0, output = '', test = None):
        self.maxpoints = self.maxpoints + points
        self.points = self.points + points
        self.logHeader('OK', points, command, test)

    def testFailExit(self, *args, **kwargs):
        """Log a failure, print report, and exit."""
        self.testFail(*args, **kwargs)
        print(self.report())
        sys.exit(0)

    def logHeader(self, msg, points, command, test):
        try:
            testName = test.name
        except:
            testName = ''
        self.log(self.logHeaderFormat.format(msg, points, command[:self.maxCommandLen], testName))

    def log(self, s):
        self._report.append(str(s))

    # returns report with points and report text
    def report(self):
        self.latePenalty = math.floor(self.points * self.lateness)
        finalPoints = max(0, self.points-self.latePenalty)
        return '\n'.join([self.reportHeader.format(self.points, self.maxpoints, self.latePenalty, 100.0*self.lateness, finalPoints, self.maxpoints)] + self._report)

    def score(self):
        return self.points, self.maxpoints

# some default output tests
class Match:
    """Test for exact match to expected (which should be bytes)."""
    name = 'm'
    def __init__(self, expected):
        self._expected = expected
    def __call__(self, s2):
        return s2 == self._expected

class MatchNewline:
    """Test for exact match with newline appended."""
    name = 'n'
    def __init__(self, expected):
        self._expected = expected + '\n'
    def __call__(self, s2):
        return s2 == self._expected

def sortLines(s):
    """Sort lines in string or bytestring."""
    lines = s.split('\n')
    return '\n'.join(sorted(lines))

class SortedMatch:
    """Test for match to expected after splitting on newline and sorting."""
    name = 's'
    def __init__(self, expected):
        self._expected = expected
    def __call__(self, s2):
        return sortLines(s2) == sortLines(self._expected)

class ExtractMatch:
    """Test for match to expected after extracting using regexp
(default [0-9]+)."""
    name = 'e'
    def __init__(self, expected, regexp = re.compile('[0-9]+')):
        self._expected = str(expected)
        self._re = regexp
    def __call__(self, s2):
        s2 = s2.decode("ascii")
        q = self._re.search(s2)
        return q and q.group(0) == self._expected

class RemoveMatch:
    """Test for match to expected after removing from both input
and expected all characters listed in second argument remove (default
"\n")."""
    name = 'r'
    def __init__(self, expected, remove = b'\n'):
        self._expected = expected
        self._remove = remove
    def remove(self, s):
        for character in self._remove:
            s = s.replace(character, b'')
    def __call__(self, s2):
        return self.remove(s2) == self.remove(self._expected)

class LessThan:
    """Test for output being an integer value < given threshold."""
    name = '<'
    def __init__(self, threshold):
        self._threshold = threshold
    def __call__(self, s2):
        try:
            got = int(s2)
        except:
            return False
        return got < self._threshold
