#!/bin/bash
# This script compiles the Ada project using the GNAT Project Builder.

echo "--- Compiling Flight Management System ---"
gprbuild -p -P flight_management_hybrid.gpr

if [ $? -eq 0 ]; then
  echo "--- Compilation Successful ---"
  echo "You can now run the application with ./run.sh"
else
  echo "--- Compilation Failed ---"
fi
