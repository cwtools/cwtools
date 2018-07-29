@echo off
cls

.paket\paket.exe restore
.paket\paket.exe restore -g tools
if errorlevel 1 (
  exit /b %errorlevel%
)

packages\tools\FAKE\tools\FAKE.exe build.fsx %*
