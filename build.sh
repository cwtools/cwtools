#!/bin/bash
if test "$OS" = "Windows_NT"
then
  # use .Net

  .paket/paket.exe restore
  .paket/paket.exe restore -g tools
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  packages/tools/FAKE/tools/FAKE.exe $@ --fsiargs build.fsx
else
  # use mono
  mono .paket/paket.exe restore
  mono .paket/paket.exe restore -g tools
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi
  mono packages/tools/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx
fi
