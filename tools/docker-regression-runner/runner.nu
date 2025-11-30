#!/usr/bin/env nu

def main [
    local_repo: path    # Path to your local git repo
    commit: string      # Commit hash to test (e.g., "HEAD", "a1b2c")
    rules_path: path    # Path to rules
    game_path: path     # Path to game
    --force-rebuild     # Force fresh build
] {
    let start = (date now)

    # --- CONFIGURATION ---
    let BUILDER_IMAGE = "mcr.microsoft.com/dotnet/sdk:10.0"
    let RUNNER_IMAGE = "mcr.microsoft.com/dotnet/runtime:9.0"
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
            git clone /mnt/repo /tmp/build --quiet &&
            cd /tmp/build &&
            git checkout ($full_hash) --quiet &&
            dotnet tool restore &&
            dotnet restore CWToolsCLI/CWToolsCLI.fsproj &&
            dotnet publish CWToolsCLI/CWToolsCLI.fsproj -c Release -o /out/temp_($full_hash) --no-restore &&

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
                -v $"($repo_path):/mnt/repo:ro"
                -v $"($cache_parent):/out"
                $BUILDER_IMAGE
                /bin/bash -c $build_cmd)
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

    # Hash inputs (handle empty dirs gracefully)
    def safe_hash [p: path] {
        try {
            let files = (ls ($p | path join "**/*") | sort-by name)
            if ($files | is-empty) {
                return ("" | hash md5)  # Hash of empty string for empty dirs
            }
            $files | each {|f| $"($f.name)($f.modified)"} | str join | hash md5
        } catch {
            print $"(ansi yellow)Warning: Could not hash ($p) - using empty hash(ansi reset)"
            return ("" | hash md5)
        }
    }

    let r_hash = (safe_hash ($rules_path | path expand))
    let g_hash = (safe_hash ($game_path | path expand))

    let result_path = ($out_dir_base | path join $"($full_hash)_($r_hash)_($g_hash)")
    mkdir $result_path

    let result = do -i {
        (^docker run --rm
            -v $"($cache_dir):/app:ro"
            -v $"($rules_path | path expand):/rules:ro"
            -v $"($game_path | path expand):/game:ro"
            -v $"($result_path):/output"
            $RUNNER_IMAGE
            dotnet /app/CWToolsCLI.dll validate --rulespath /rules --directory /game --outputfile /output/output.json --reporttype json --game stl --scope vanilla  all)

    } | complete
    $result.stdout | save -f ($result_path | path join "stdout.log")
    $result.stderr | save -f ($result_path | path join "stderr.log")

    # --- 3. METADATA ---
    {
        commit: $full_hash
        timestamp: (date now)
        exit_code: $env.LAST_EXIT_CODE
        rules_hash: $r_hash
        game_hash: $g_hash
        build_cache: $cache_dir
    } | to json | save -f ($result_path | path join "metadata.json")

    print $"(ansi green)Done: ($result_path)(ansi reset)"
}
