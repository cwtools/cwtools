dotnet run --no-build --directory "F:\Games\Steam\steamapps\common\Stellaris" --game stl --scope vanilla list --sort path files

dotnet run --no-build --directory "F:\Games\Steam\steamapps\common\Stellaris" --game stl --scope vanilla serialize


robocopy F:\Games\Steam\steamapps\common\Stellaris\ C:\Users\Thomas\git\CWToolsExtensionLSP\src\Main\files *.txt *.yml *.asset *.gfx *.gui /s