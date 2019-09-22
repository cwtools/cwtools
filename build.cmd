@echo off
cls
dotnet tool install --tool-path ./.paket paket
dotnet tool install --tool-path ./.fake-cli fake-cli
.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

.\.fake-cli\FAKE.exe run build.fsx %*
