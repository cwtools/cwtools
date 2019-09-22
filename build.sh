#!/bin/bash

dotnet tool restore
dotnet tool run paket restore
exit_code=$?
if [ $exit_code -ne 0 ]; then
  exit $exit_code
fi
dotnet tool run fake run --fsiargs -d:MONO build.fsx --target $@
