#!/bin/bash
# This script runs the main executable of the project.

EXECUTABLE_NAME="main"
echo "--- Launching Flight Management System ---"

if [ -f "./$EXECUTABLE_NAME" ]; then
  ./$EXECUTABLE_NAME
  echo
  echo "--- Application Exited ---"
else
  echo "Error: Executable '$EXECUTABLE_NAME' not found."
  echo "Please run ./compile.sh first."
fi
