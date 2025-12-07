#!/usr/bin/env nu

def main [
    local_repo: path    # Path to your local git repo
    commit: string      # Commit hash to test (e.g., "HEAD", "a1b2c")
    rules_path: path    # Path to rules
    game_path: path     # Path to game
    --force-rebuild     # Force fresh build
    --force-rerun       # Force rerun even if results exist
] {
    let start = (date now)
    let rules_path = $rules_path | str replace --all '\\' '/' | str replace "D:" ""
    let game_path = $game_path | str replace --all '\\' '/' | str replace "D:" ""

    # --- CONFIGURATION ---
    let BUILDER_IMAGE = "mcr.microsoft.com/dotnet/sdk:10.0-alpine"
    let MARKER_FILE = ".valid-cache"

    let root = ($env.PWD | path expand)
    let repo_path = ($local_repo | path expand)

    # Validate Repo
    if not ($repo_path | path join ".git" | path exists) {
        print $"(ansi red)Error: ($repo_path) is not a git repo(ansi reset)"
        exit 1
    }

    # Resolve Hash (NO CD - use git flags instead)
    let git_dir = ($repo_path | path join ".git")
    let full_hash = (do -i {
        ^git --git-dir $git_dir --work-tree $repo_path rev-parse $commit
    } | str trim)
    if ($full_hash | is-empty) { print "Invalid commit"; exit 1 }

    # Paths
    let cache_parent = ($root | path join "build-cache")
    let cache_dir = ($cache_parent | path join $full_hash)
    let marker_path = ($cache_dir | path join $MARKER_FILE)
    let out_dir_base = ($root | path join "regression-results")

    # Hash inputs early (needed for result path check)
    def safe_hash [p: path] {
        try {
            print ($"($p)**/*" )
            let files = (glob $"($p)**/*" | sort | each {|f| ls $f | first } )
            if ($files | is-empty) {
                return ("" | hash md5)
            }
            $files | each {|f| $"($f.name)($f.modified)"} | str join "" | hash md5
        } catch { |err|
            print $"(ansi yellow)Warning: Could not hash ($p), ($err.msg) - using empty hash(ansi reset)"
            return ("" | hash md5)
        }
    }

    let r_hash = "0"
    let g_hash = "0"
    let result_path = ($out_dir_base | path join $"($full_hash)_($r_hash)_($g_hash)")

    # --- CHECK IF RESULTS ALREADY EXIST ---
    let results_exist = (
        ($result_path | path join "output.json" | path exists) and
        ($result_path | path join "metadata.json" | path exists)
    )

    if $results_exist and (not $force_rerun) {
        print $"⚡ Results already exist: ($result_path)"
        let errorCount = (open ($result_path | path join "output.json") | get "errorCount")
        print $"   ($errorCount) errors found"
        print $"   Use --force-rerun to regenerate"
        exit 0
    }

    # --- 1. BUILD (MARKER FILE ONLY FOR SAFE DELETION) ---
    let should_build = ($force_rebuild or not ($cache_dir | path exists))

    if $should_build {
        print $"🔨 Building ($full_hash | str substring 0..7) in isolated container..."

        # NUSHELL: Only rm if marker file exists (proves it's safe to delete)
        if ($cache_dir | path exists) and ($marker_path | path exists) {
            print "   Cleaning existing valid cache (marker file found)..."
            rm -rf $cache_dir
        } else if ($cache_dir | path exists) {
            print $"(ansi yellow)   Found existing directory without marker file - will rebuild over it(ansi reset)"
        }

        # Ensure parent cache directory exists
        mkdir $cache_parent

        # Container script - creates marker file on success
        let build_cmd = $"
            # Create temp build directory
            mkdir -p /out/temp_($full_hash) &&

            # Clone, build, and validate
            git clone file:///mnt/repo /tmp/build --revision ($full_hash) --depth 1 --quiet &&
            cd /tmp/build &&
            git checkout ($full_hash) --quiet &&
            dotnet tool restore &&
            dotnet restore CWToolsCLI/CWToolsCLI.fsproj -r win-x64 &&
            dotnet publish CWToolsCLI/CWToolsCLI.fsproj --self-contained -r win-x64 -c Release -o /out/temp_($full_hash) --no-restore &&

            # Validate the build succeeded
            if [ -f '/out/temp_($full_hash)/CWToolsCLI.dll' ]; then
                # Atomic move to final cache location
                mv /out/temp_($full_hash) /out/($full_hash) &&

                # Create marker file to prove this is safe to delete in future
                echo '($full_hash)' > /out/($full_hash)/($MARKER_FILE) &&

                echo 'BUILD_SUCCESS' &&
                exit 0
            else
                echo 'BUILD_FAILED: DLL missing after publish' &&
                exit 1
            fi
        " | str replace --all "\r" ""

        # Run Builder (capture output for debugging)
        let build_output = (do -i {
            (^docker run --rm
                -v nuget-cache:/root/.nuget/packages
                -v $"($repo_path):/mnt/repo:ro"
                -v $"($cache_parent):/out"
                $BUILDER_IMAGE
                /bin/sh -c $build_cmd)
        } | complete)

        if $build_output.exit_code != 0 {
            print $"(ansi red)Build failed! Container output:(ansi reset)"
            print $build_output.stdout
            print $build_output.stderr
            exit $build_output.exit_code
        }

        # Final validation: container succeeded, check cache exists
        if not ($cache_dir | path exists) {
            print $"(ansi red)Error: Container reported success but cache directory ($cache_dir) was not created(ansi reset)"
            exit 1
        }

        # Validate key file exists (marker will be created by container)
        let dll_path = ($cache_dir | path join "CWToolsCLI.dll")
        if not ($dll_path | path exists) {
            print $"(ansi red)Error: Cache directory exists but ($dll_path) is missing(ansi reset)"
            # Clean up invalid cache - no marker yet, but it's safe since we just created it
            rm -rf $cache_dir
            exit 1
        }

        print $"   Build Cached: ($cache_dir) ✓"
    } else {
        print $"⚡ Using cached build: ($cache_dir)"
    }

    # --- 2. RUN ---
    print "🚀 Running Validation..."

    mkdir $result_path

    # Native execution - no Docker I/O
    print "   Running natively (no Docker I/O)..."

    # Determine .NET executable name
    let dotnet_exe = if ($env.OS | str contains "windows") { "dotnet.exe" } else { "dotnet" }

    # Change to cache directory temporarily
    let old_pwd = ($env.PWD)
    cd $cache_dir

    # Run natively with absolute paths
    let cli_args = [
        "CWToolsCLI.dll"
        "validate"
        $"--rulespath" $"($rules_path | path expand)"
        $"--directory" $"($game_path | path expand)"
        $"--outputfile" $"($result_path | path join "output.json")"
        $"--reporttype" "json"
        $"--game" "stl"
        $"--scope" "vanilla"
        "all"
    ]

    let run_result = do {
        ^dotnet ...$cli_args
    } | complete

    $run_result.stdout | save -f ($result_path | path join "native_stdout.txt")
    $run_result.stderr | save -f ($result_path | path join "native_stderr.txt")

    cd $old_pwd  # Restore original directory

    if $run_result.exit_code != 0 and $run_result.exit_code != 1 {
        print $"(ansi red)Native run failed with exit code ($run_result.exit_code)(ansi reset)"
        exit $run_result.exit_code
    }

    # --- 3. METADATA ---
    {
        commit: $full_hash
        timestamp: (date now)
        exit_code: $env.LAST_EXIT_CODE
        rules_hash: $r_hash
        game_hash: $g_hash
        build_cache: $cache_dir
    } | to json | save -f ($result_path | path join "metadata.json")
    
    let errorCount = (open ($result_path | path join "output.json") | get "errorCount")

    print $"(ansi green)Done: ($result_path)(ansi reset)"
}
