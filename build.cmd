@echo off
cls
dotnet tool restore
dotnet tool run paket restore
if errorlevel 1 (
  exit /b %errorlevel%
)

dotnet tool run fake run build.fsx --target %*
