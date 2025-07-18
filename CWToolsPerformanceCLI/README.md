# CWTools Performance CLI

A command-line interface for running performance tests on various Paradox Interactive games using the CWTools library. This tool provides benchmarking and validation capabilities for game mods and vanilla installations.

## Purpose

The CWTools Performance CLI is designed to:
- Benchmark parsing and validation performance for Paradox Interactive games
- Test mod compatibility and performance impact
- Validate game files and configurations
- Provide detailed performance metrics and error reporting

## Prerequisites

Before building and running the CLI, ensure you have the required dependencies:

```powershell
# Install dependencies using Paket
paket install

# Or alternatively, restore packages using dotnet
dotnet restore
```

## Building the Project

To build the CLI:

```powershell
dotnet build CWToolsPerformanceCLI.fsproj
```

## Available Commands

### Game-Specific Performance Tests

#### Stellaris
- **`--stellaris-manual` / `-stl-manual`**: Run Stellaris performance test in manual mode
- **`--stellaris-verbose` / `-stl-verbose`**: Run Stellaris performance test in verbose mode  
- **`--stellaris-modcached` / `-stl-modcached`**: Run Stellaris performance test with mod caching

#### Europa Universalis IV (EU4)
- **`--eu4-vanilla` / `-eu4-vanilla`**: Run EU4 vanilla performance test
- **`--eu4-custom` / `-eu4-custom`**: Run EU4 custom performance test

#### Hearts of Iron IV (HOI4)
- **`--hoi4-vanilla` / `-hoi4-vanilla`**: Run HOI4 vanilla performance test
- **`--hoi4-modcached` / `-hoi4-modcached`**: Run HOI4 performance test with mod caching

#### Crusader Kings III (CK3)
- **`--ck3-vanilla` / `-ck3-vanilla`**: Run CK3 vanilla performance test

#### General Testing
- **`--parse-test` / `-parse-test`**: Run parsing test on a specific file

### Optional Path Parameters

The CLI accepts several optional path parameters to customize test locations:

- **`--game-path` / `-g`**: Path to game installation directory
- **`--config-path` / `-c`**: Path to config directory (contains .cwt rule files)
- **`--cache-path` / `-p`**: Path to optional .cwb cache file
- **`--file-path` / `-f`**: Path to file for parsing test

## Usage Examples

### Basic Game Tests

```powershell
# Run Stellaris manual test with default paths
.\CWToolsPerformanceCLI.exe --stellaris-manual

# Run EU4 vanilla test with default paths  
.\CWToolsPerformanceCLI.exe --eu4-vanilla

# Run HOI4 with mod caching
.\CWToolsPerformanceCLI.exe --hoi4-modcached
```

### Tests with Custom Paths

```powershell
# Stellaris test with custom game and config paths
.\CWToolsPerformanceCLI.exe --stellaris-verbose -g "C:\Games\Stellaris" -c "C:\Config\stellaris-config"

# EU4 test with custom paths and cache
.\CWToolsPerformanceCLI.exe --eu4-vanilla -g "D:\Games\EU4" -c "D:\Config\eu4-config" -p "D:\Cache\eu4.cwb"

# HOI4 test with all custom paths
.\CWToolsPerformanceCLI.exe --hoi4-vanilla -g "C:\Games\HOI4" -c "C:\Config\hoi4-config" -p "C:\Cache\hoi4.cwb"
```

### File Parsing Tests

```powershell
# Parse a specific game file
.\CWToolsPerformanceCLI.exe --parse-test -f "C:\Games\Stellaris\common\buildings\00_buildings.txt"

# Parse with default test file
.\CWToolsPerformanceCLI.exe --parse-test
```

### Advanced Examples

```powershell
# Stellaris mod testing with caching
.\CWToolsPerformanceCLI.exe -stl-modcached -g "C:\Users\Username\Documents\Paradox Interactive\Stellaris\mod\my_mod" -c "C:\Config\stellaris-config" -p "C:\Cache\stellaris.cwb"

# CK3 vanilla test with custom installation
.\CWToolsPerformanceCLI.exe -ck3-vanilla -g "D:\Games\Steam\steamapps\common\Crusader Kings III\game" -c "D:\Config\ck3-config"
```

## Output and Results

The CLI provides detailed performance metrics including:
- **Execution time** in milliseconds
- **Error count** from validation
- **Success/failure status** for each test
- **Detailed error messages** when tests fail

Example output:
```
Running Stellaris Manual Test...
✓ Stellaris Manual Test completed successfully
  Elapsed: 2847ms, Errors: 0
```

## Default Paths

When optional path parameters are not provided, the CLI uses these default paths:

### Stellaris
- **Manual mode**: `./CWToolsTests/testfiles/performancetest/`
- **Verbose mode**: `./CWToolsTests/testfiles/performancetest2/`
- **Mod cached**: User's Stellaris mod directory

### EU4
- **Vanilla**: Steam installation path
- **Custom**: `./CWToolsTests/testfiles/custom/files`

### HOI4
- **Vanilla**: Steam installation path
- **Mod cached**: Mod directory path

### CK3
- **Vanilla**: Steam installation path

## Error Handling

The CLI includes comprehensive error handling:
- **Argument parsing errors** are reported with helpful messages
- **Runtime errors** are caught and displayed
- **File not found errors** are handled gracefully
- **Performance test failures** are reported with detailed information

## Notes

- Tests run validation by default and report error counts
- Verbose logging is enabled for certain game types
- Cache files (.cwb) significantly improve performance for repeated tests
- All time measurements are in milliseconds
- The CLI sets culture to Russian (ru-RU) for proper text encoding

## Getting Help

To see all available options:
```powershell
.\CWToolsPerformanceCLI.exe --help
```

## Troubleshooting

1. **Build errors**: Ensure `paket install` or `dotnet restore` has been run
2. **Path errors**: Verify game installation paths are correct
3. **Permission errors**: Run with appropriate permissions for file access
4. **Cache errors**: Clear .cwb cache files if encountering issues

## Performance Tips

- Use cache files (`.cwb`) for repeated testing to improve performance
- Place config files in easily accessible directories
- Use verbose mode for detailed debugging information
- Run tests on SSDs for better I/O performance
