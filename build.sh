#!/bin/bash

dotnet tool restore
dotnet paket restore
exit_code=$?
if [ $exit_code -ne 0 ]; then
  exit $exit_code
fi
dotnet fake run build.fsx --target $@
