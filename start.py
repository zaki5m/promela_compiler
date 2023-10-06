import subprocess

subprocess.run(["python3", "lexer_ply_old.py"], capture_output=True, text=True)
subprocess.run(["python3", "edit_result_analyze.py"], capture_output=True, text=True)