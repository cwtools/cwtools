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
- **`--stellaris` / `-stellaris`**: Run Stellaris performance test
  - Use `--mode manual` for manual mode
  - Use `--mode verbose` for verbose mode (default)
  - Use `--mode cached` for mod caching
  - Automatically uses caching when `--cache-path` is provided

#### Europa Universalis IV (EU4)
- **`--eu4` / `-eu4`**: Run EU4 performance test
  - Use `--mode vanilla` for vanilla test (default)
  - Use `--mode custom` for custom test with test files
  - Automatically uses caching when `--cache-path` is provided

#### Hearts of Iron IV (HOI4)
- **`--hoi4` / `-hoi4`**: Run HOI4 performance test
  - Automatically detects vanilla vs. cached based on cache path parameter

#### Crusader Kings III (CK3)
- **`--ck3` / `-ck3`**: Run CK3 performance test
  - Automatically uses caching when `--cache-path` is provided

#### General Testing
- **`--parse-test` / `-parse-test`**: Run parsing test on a specific file

### Optional Path Parameters

The CLI accepts several optional path parameters to customize test locations:

- **`--game-path` / `-g`**: Path to game installation directory
- **`--config-path` / `-c`**: Path to config directory (contains .cwt rule files)
- **`--cache-path` / `-p`**: Path to optional .cwb cache file
- **`--file-path` / `-f`**: Path to file for parsing test
- **`--mode` / `-m`**: Test mode (varies by game)

## Usage Examples

### Basic Game Tests

```powershell
# Run Stellaris test with default mode (verbose)
.\CWToolsPerformanceCLI.exe --stellaris

# Run Stellaris in manual mode
.\CWToolsPerformanceCLI.exe --stellaris --mode manual

# Run EU4 test with default mode (vanilla)
.\CWToolsPerformanceCLI.exe --eu4

# Run EU4 in custom mode
.\CWToolsPerformanceCLI.exe --eu4 --mode custom

# Run HOI4 vanilla test
.\CWToolsPerformanceCLI.exe --hoi4

# Run HOI4 with caching (when cache path is provided)
.\CWToolsPerformanceCLI.exe --hoi4 -p "C:\Cache\hoi4.cwb"

# Run CK3 test
.\CWToolsPerformanceCLI.exe --ck3
```

### Tests with Custom Paths

```powershell
# Stellaris test with custom game and config paths
.\CWToolsPerformanceCLI.exe --stellaris -g "C:\Games\Stellaris" -c "C:\Config\stellaris-config"

# Stellaris manual mode with custom paths
.\CWToolsPerformanceCLI.exe --stellaris --mode manual -g "C:\Games\Stellaris" -c "C:\Config\stellaris-config"

# EU4 test with custom paths and cache
.\CWToolsPerformanceCLI.exe --eu4 -g "D:\Games\EU4" -c "D:\Config\eu4-config" -p "D:\Cache\eu4.cwb"

# EU4 custom mode with test files
.\CWToolsPerformanceCLI.exe --eu4 --mode custom -c "D:\Config\eu4-config"

# HOI4 test with all custom paths
.\CWToolsPerformanceCLI.exe --hoi4 -g "C:\Games\HOI4" -c "C:\Config\hoi4-config" -p "C:\Cache\hoi4.cwb"

# CK3 test with custom paths
.\CWToolsPerformanceCLI.exe --ck3 -g "D:\Games\CK3" -c "D:\Config\ck3-config" -p "D:\Cache\ck3.cwb"
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
.\CWToolsPerformanceCLI.exe --stellaris --mode cached -g "C:\Users\Username\Documents\Paradox Interactive\Stellaris\mod\my_mod" -c "C:\Config\stellaris-config" -p "C:\Cache\stellaris.cwb"

# CK3 test with custom installation
.\CWToolsPerformanceCLI.exe --ck3 -g "D:\Games\Steam\steamapps\common\Crusader Kings III\game" -c "D:\Config\ck3-config"
```

## Output and Results

The CLI provides detailed performance metrics including:
- **Execution time** in milliseconds
- **Error count** from validation
- **Success/failure status** for each test
- **Detailed error messages** when tests fail

Example output:
```
Running Stellaris Test (verbose)...
✓ Stellaris Test (verbose) completed successfully
  Elapsed: 2847ms, Errors: 0
```

## Default Paths

When optional path parameters are not provided, the CLI uses these default paths:

### Stellaris
- **Manual mode** (`--mode manual`): `./CWToolsTests/testfiles/performancetest/`
- **Verbose mode** (`--mode verbose`, default): `./CWToolsTests/testfiles/performancetest2/`
- **Cached mode** (`--mode cached`): User's Stellaris mod directory

### EU4
- **Vanilla mode** (`--mode vanilla`, default): Steam installation path
- **Custom mode** (`--mode custom`): `./CWToolsTests/testfiles/custom/files`

### HOI4
- **Default**: Steam installation path
- **Caching**: Automatically enabled when cache path parameter is provided

### CK3
- **Default**: Steam installation path
- **Caching**: Automatically enabled when cache path parameter is provided

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
- **Automatic caching**: All games automatically detect whether to use caching based on the presence of the `--cache-path` parameter
- **Mode parameter**: Different games support different modes via the `--mode` parameter
- **Stellaris modes**: manual, verbose (default), cached
- **EU4 modes**: vanilla (default), custom

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
