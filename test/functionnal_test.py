import subprocess


def run_glados_test(file_name, expected_output):
    glados_executable = "./glados"
    command = [glados_executable, file_name]
    output = subprocess.check_output(" ".join(command), shell=True, text=True)

    if output != expected_output:
        print(f"Échec du test {file_name}. Sortie inattendue : {output}")
    else:
        print(f"Test {file_name} réussi!\n")


if __name__ == "__main__":
    test_cases = [
        {"file_name": "Lisp/bool.scm", "expected_output": "True\n"},
        {"file_name": "Lisp/builtins1.scm", "expected_output": "11\n"},
        {"file_name": "Lisp/builtins2.scm", "expected_output": "False\n"},
        {"file_name": "Lisp/builtins3.scm", "expected_output": "False\n"},
        {"file_name": "Lisp/call.scm", "expected_output": "5\n"},
        {"file_name": "Lisp/concat.scm", "expected_output": "HelloWorld\n"},
        {"file_name": "Lisp/concat2.scm", "expected_output": "HelloWorld\n"},
        {"file_name": "Lisp/foo.scm", "expected_output": "42\n"},
        {"file_name": "Lisp/function1.scm", "expected_output": "7\n"},
        {"file_name": "Lisp/function3.scm", "expected_output": "6\n"},
        # {"file_name": "Lisp/function5.scm", "expected_output": "6\n"},
        {"file_name": "Lisp/if1.scm", "expected_output": "1\n"},
        {"file_name": "Lisp/if2.scm", "expected_output": "2\n"},
        {"file_name": "Lisp/if3.scm", "expected_output": "21\n"},
        {"file_name": "Lisp/number.scm", "expected_output": "42\n"},
        {"file_name": "Lisp/same.scm", "expected_output": "They are the same\n"},
        {"file_name": "Lisp/same2.scm", "expected_output": "They are not the same\n"},
    ]
    num_tests = test_cases.__len__()
    num_passed = 0
    num_failed = 0

    print(f"\nNombre de tests : {num_tests}\n")
    for test_case in test_cases:
        run_glados_test(test_case["file_name"], test_case["expected_output"])
