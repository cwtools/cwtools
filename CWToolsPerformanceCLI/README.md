# CWTools Performance CLI (AI Generated docs...)

A command-line interface for running performance tests on various Paradox Interactive games using the CWTools library. This tool provides benchmarking and validation capabilities for game mods and vanilla installations.

## Available Commands

### Game-Specific Performance Tests

#### Stellaris
- **`--stellaris` / `-stellaris`**: Run Stellaris performance test
  - Default paths are used unless specified
  - Automatically uses caching when `--cache-path` is provided
  - Use `--mod-path` to test vanilla + mod together
  
Default Stellaris root and config paths:
- Root: `./CWToolsTests/testfiles/performancetest2/`
- Config: `./CWToolsTests/testfiles/performancetest2/.cwtools`

#### Europa Universalis IV (EU4)
- **`--eu4` / `-eu4`**: Run EU4 performance test
  - Default paths are used unless specified
  - Automatically uses caching when `--cache-path` is provided
  - Use `--mod-path` to test vanilla + mod together
  
Default EU4 root and config paths:
- Root: `D:\Games\Steam\steamapps\common\Europa Universalis IV`
- Config: `C:\Users\Thomas\git\cwtools-eu4-config\`

#### Hearts of Iron IV (HOI4)
- **`--hoi4` / `-hoi4`**: Run HOI4 performance test
  - Automatically detects vanilla vs. cached based on cache path parameter
  - Use `--mod-path` to test vanilla + mod together

#### Crusader Kings III (CK3)
- **`--ck3` / `-ck3`**: Run CK3 performance test
  - Automatically uses caching when `--cache-path` is provided
  - Use `--mod-path` to test vanilla + mod together

#### General Testing
- **`--parse-test` / `-parse-test`**: Run parsing test on a specific file

### Optional Path Parameters

The CLI accepts several optional path parameters to customize test locations:

- **`--game-path` / `-g`**: Path to game installation directory
- **`--config-path` / `-c`**: Path to config directory (contains .cwt rule files)
- **`--cache-path` / `-p`**: Path to optional .cwb cache file
- **`--mod-path` / `-mod-path`**: Path to mod directory (tests vanilla + mod together)
- **`--steam-root` / `-steam-root`**: Path to Steam installation root (overrides default)
- **`--git-root` / `-git-root`**: Path to Git projects root (overrides default)

## Usage Examples

### Basic Game Tests

```powershell
# Run Stellaris test with default paths
 dotnet run -c Release -- --stellaris

# Run EU4 test with default paths
 dotnet run -c Release -- --eu4

# Run HOI4 test with default paths
 dotnet run -c Release -- --hoi4

# Run HOI4 with caching (when cache path is provided)
 dotnet run -c Release -- --hoi4 -p "C:\Cache\hoi4.cwb"

# Run CK3 test with default paths
 dotnet run -c Release -- --ck3
```

### Tests with Custom Paths

```powershell
# Stellaris test with custom game and config paths
 dotnet run -c Release -- --stellaris -g "C:\Games\Stellaris" -c "C:\Config\stellaris-config"

# EU4 test with custom paths and cache
 dotnet run -c Release -- --eu4 -g "D:\Games\EU4" -c "D:\Config\eu4-config" -p "D:\Cache\eu4.cwb"

# HOI4 test with all custom paths
 dotnet run -c Release -- --hoi4 -g "C:\Games\HOI4" -c "C:\Config\hoi4-config" -p "C:\Cache\hoi4.cwb"

# CK3 test with custom paths
 dotnet run -c Release -- --ck3 -g "D:\Games\CK3" -c "D:\Config\ck3-config" -p "D:\Cache\ck3.cwb"

# Test vanilla + mod together
 dotnet run -c Release -- --stellaris --mod-path "C:\Mods\MyStellarisMod"
 dotnet run -c Release -- --eu4 --mod-path "C:\Mods\MyEU4Mod"
 dotnet run -c Release -- --hoi4 --mod-path "C:\Mods\MyHOI4Mod"
 dotnet run -c Release -- --ck3 --mod-path "C:\Mods\MyCK3Mod"

# Override Steam root for different installation
 dotnet run -c Release -- --eu4 --steam-root "C:\Program Files (x86)\Steam\steamapps\common"

# Override Git root for different project location
 dotnet run -c Release -- --stellaris --git-root "D:\Projects"
```

### Advanced Examples

```powershell
# Stellaris vanilla + mod testing
 dotnet run -c Release -- --stellaris -g "C:\Games\Stellaris" --mod-path "C:\Users\Username\Documents\Paradox Interactive\Stellaris\mod\my_mod" -c "C:\Config\stellaris-config"

# CK3 test with custom installation
 dotnet run -c Release -- --ck3 -g "D:\Games\Steam\steamapps\common\Crusader Kings III\game" -c "D:\Config\ck3-config"

# Testing alternate Stellaris test files
 dotnet run -c Release -- --stellaris -g "./CWToolsTests/testfiles/performancetest/" -c "./CWToolsTests/testfiles/performancetest2/.cwtools"

# Testing EU4 custom test files
 dotnet run -c Release -- --eu4 -g "./CWToolsTests/testfiles/custom/files" -c "./CWToolsTests/testfiles/custom/rules"
```

### File Parsing Tests

```powershell
# Parse a specific game file
 dotnet run -c Release -- --parse-test -f "C:\Games\Stellaris\common\buildings\00_buildings.txt"

# Parse with default test file
 dotnet run -c Release -- --parse-test
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

When optional path parameters are not provided, the CLI uses these default paths. These are derived from a centralized path configuration consisting of `SteamRoot`, `GitRoot`, `UserHome`, and `CacheRoot`. Users can modify these roots to change default paths:

### Central Roots:
- **SteamRoot**: `D:\Games\Steam\steamapps\common` (can be overridden with `--steam-root`)
- **GitRoot**: `C:\Users\Thomas\Git` (can be overridden with `--git-root`)
- **UserHome**: [User's Home Directory]
- **CacheRoot**: `[ExecutableDirectory]\cache`

### Alternative Test Paths

For testing purposes, you can use these alternative paths:

**Stellaris alternative test files:**
- `./CWToolsTests/testfiles/performancetest/` (smaller test set)

**EU4 custom test files:**
- `./CWToolsTests/testfiles/custom/files` (root)
- `./CWToolsTests/testfiles/custom/rules` (config)
