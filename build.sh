#!/bin/bash
if test "$OS" = "Windows_NT"
then
  # use .Net
  dotnet tool install --tool-path ./.paket paket
  dotnet tool install --tool-path ./.fake-cli fake-cli
  .paket/paket.exe restore
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  ./.fake-cli/FAKE.exe $@ --fsiargs build.fsx
else
  # use linux version
  dotnet tool install --tool-path ./.paket paket
  dotnet tool install --tool-path ./.fake-cli fake-cli
  .paket/paket restore
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi
  ./.fake-cli/FAKE $@ --fsiargs -d:MONO build.fsx
fi
