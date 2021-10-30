import subprocess
import numpy
from numpy.core.fromnumeric import clip

PROG = "./matmult -n {size} -v {version}"
RUNS_PER_SIZE = 25

def run_program(size, version):
    command = PROG.format(size=size, version=version)
    return subprocess.check_output(command, shell=True, encoding="UTF8")

def get_running_time(result: str):
    last_line = result.split("\n")[2]
    running_time = last_line.split(" ")[2]
    return float(running_time)

def run_n_times(n, size, version) -> list:
    results = []
    for i in range(n):
        result = run_program(size, version)
        running_time = get_running_time(result)
        results.append(running_time)
    return results

def get_average(results : list):
    results.sort()
    n = len(results)
    five_percent_of_n = int(n * 0.05)
    without_extremes = results[five_percent_of_n : -five_percent_of_n]
    return numpy.mean(without_extremes)

def test_version(size,version):
    results = run_n_times(RUNS_PER_SIZE, size, version)
    return get_average(results)

def benchmark_versions():
    sizes = [ x * 16 for x in range(4,45) ] 
    versions = [0,1,2,3]
    version_results = [ [], [], [], [] ]
    for size in sizes:
        for version in versions:
            print("Testing {version} size {size}".format(version=version,size=size))
            r = test_version(size, version)
            version_results[version].append(r)
    with open("versions.dat", "w") as file:
        n_of_sizes = len(sizes)
        for i in range(n_of_sizes):
            file.write(str(sizes[i]) + " ")
            for version in versions:
                file.write( str(version_results[version][i]) + " ")
            file.write("\n")

def benchmark_blocks_offset():
    sizes = [ x * 16 for x in range(4,45) ]
    results = [[], []]
    with open("versions.dat","r") as file:
        content = file.read()
    lines = content.split("\n")
    for line in lines:
        prev_result = line.split(" ")[4]
        results[0].append(prev_result)
    for size in sizes:
        print("Testing {version} size {size}".format(version=3,size=size))
        r = test_version(size, 3)
        results[1].append(r)
    with open("blocks_offset.dat", "w") as file:
        n_of_sizes = len(sizes)
        for i in range(n_of_sizes):
            file.write(str(sizes[i]) + " ")
            file.write(str(results[0][i]) + " ")
            file.write(str(results[1][i]) + " ")
            file.write("\n")

# benchmark_versions()
# benchmark_blocks_offset()