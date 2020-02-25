import sys
import os
import subprocess

if len(sys.argv) != 3:
    print("Expecting 2 arguments: executable binary, directory path/name.")
    exit(1)


def run_tests(executable_path, dir, depth=0):
    contents = os.listdir(dir)
    if len(contents) == 0:
        return

    indent = ''.join(["  " for _ in range(depth)])
    indent_1 = indent + "  "
    indent_2 = indent_1 + "  "

    print(indent + f"Running tests from '{dir}'...")
    for entry in contents:
        entry_path = f"{dir}/{entry}"

        # recursively walk through directories
        if os.path.isdir(entry_path):
            run_tests(executable_path, entry_path, depth + 1)
            continue

        # ignore non-input files
        if not entry.endswith(".in"):
            continue

        name = entry.rsplit('.', 1)[0]
        try:
            with open(f"{dir}/{name}.txt", 'r') as f:
                desc = f.read()
        except FileNotFoundError:
            desc = f"Test '{name}'"

        # get reference output file names
        refs = [
            (ref, ref.split('.')[1:-1], ref.split('.')[-1])
            for ref in contents
            if ref.startswith(name + ".")
                and not ref.endswith(".in")
                and not ref.endswith(".txt")
        ]
        
        print(indent_1 + f"{name}: ", end='')
        if len(refs) == 0:
            print("No reference outputs found!", end='')

        for ref_file, opts, ref_type in refs:
            p2 = subprocess.Popen(
                [executable_path, *[f"-{opt}" for opt in opts], entry_path],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE
            )
            stdout, stderr = p2.communicate()
            if stdout:
                stdout = os.fsdecode(stdout).replace("\r\n", "\n")
            if stderr:
                stderr = os.fsdecode(stderr).replace("\r\n", "\n")

            try:
                with open(f"{dir}/{ref_file}", 'r', encoding="utf-8") as f:
                    ref_output = f.read()
            except FileNotFoundError:
                print("Test initialization failed!", end='')
                continue

            if ref_type == "out":
                if stdout == ref_output:
                    print("OK", end='')
                else:
                    print("Error!")
                    print(indent_2 + "Expected:\n", ref_output)
                    print(indent_2 + "Got:\n", stdout)

            if ref_type == "err":
                if stderr == ref_output:
                    print("OK", end='')
                else:
                    print("Error!")
                    print(indent_2 + "Expected:\n", ref_output)
                    print(indent_2 + "Got:\n", stderr)
        
        print("\t (" + desc + ")")

executable_path = os.path.realpath(sys.argv[1])
testdir = os.path.realpath(sys.argv[2])
run_tests(executable_path, testdir)
