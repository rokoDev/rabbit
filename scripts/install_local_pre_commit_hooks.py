#!/usr/bin/env python3

import os
import shutil
import sys
import git
import subprocess

def make_runnable(path_to_shell_script):
  command = ['chmod', '+x', path_to_shell_script]
  try:
    subprocess.run(command, check=True)
  except subprocess.CalledProcessError as e:
    s = " "
    print(f"Command [{s.join(command)}] exit with code {e.returncode}")
    sys.exit(e.returncode)

def main():

  ABS_PATH_TO_SCRIPT = os.path.dirname(os.path.abspath(__file__))

  PRE_COMMIT_PATH = os.path.join(ABS_PATH_TO_SCRIPT, 'pre-commit')
  PREPARE_COMMIT_MSG_PATH = os.path.join(ABS_PATH_TO_SCRIPT, 'prepare-commit-msg')

  IS_PRE_COMMIT_EXISTS = os.path.exists(PRE_COMMIT_PATH)
  IS_PREPARE_COMMIT_MSG_EXISTS = os.path.exists(PREPARE_COMMIT_MSG_PATH)

  if not (IS_PRE_COMMIT_EXISTS and IS_PREPARE_COMMIT_MSG_EXISTS):
    print(f"pre-commit or prepare-commit-msg files does not reside in expected path:{ABS_PATH_TO_SCRIPT}")
    sys.exit(1)

  repo = git.Repo('.', search_parent_directories=True)
  ROOT_PROJECT_PATH = repo.working_tree_dir

  DST_PRE_COMMIT_PATH = os.path.join(ROOT_PROJECT_PATH, ".git", "hooks", "pre-commit")
  DST_PREPARE_COMMIT_MSG_PATH = os.path.join(ROOT_PROJECT_PATH, ".git", "hooks", "prepare-commit-msg")

  shutil.copyfile(PRE_COMMIT_PATH, DST_PRE_COMMIT_PATH)
  shutil.copyfile(PREPARE_COMMIT_MSG_PATH, DST_PREPARE_COMMIT_MSG_PATH)

  make_runnable(DST_PRE_COMMIT_PATH)
  make_runnable(DST_PREPARE_COMMIT_MSG_PATH)

  print(f"pre-commit hooks successfully installed!")

if __name__ == "__main__":
  main()