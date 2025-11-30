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
    # We need an SDK image that includes Git. The standard dotnet SDK usually does.
    let BUILDER_IMAGE = "mcr.microsoft.com/dotnet/sdk:10.0"
    let RUNNER_IMAGE = "mcr.microsoft.com/dotnet/runtime:9.0"

    let root = ($env.PWD | path expand)
    let repo_path = ($local_repo | path expand)

    # Validate Repo
    if not ($repo_path | path join ".git" | path exists) {
        print $"(ansi red)Error: ($repo_path) is not a git repo(ansi reset)"
        exit 1
    }

    # Resolve Hash (On Host) to know where to cache it
    # We do this locally because it's instant
    cd $repo_path
    let full_hash = (do -i { ^git rev-parse $commit } | str trim)
    if ($full_hash | is-empty) { print "Invalid commit"; exit 1 }
    cd $root

    # Paths
    let cache_dir = ($root | path join "build-cache" $full_hash)
    let temp_cache_dir = ($root | path join "build-cache" $"temp_($full_hash)")
    let out_dir_base = ($root | path join "regression-results")

    # --- 1. BUILD (READ-ONLY MOUNT STRATEGY) ---
    if $force_rebuild or not ($cache_dir | path exists) {
        print $"🔨 Building ($full_hash | str substring 0..7) in isolated container..."
        mkdir $cache_dir

        # THE MAGIC:
        # 1. Mount local repo to /mnt/repo (Read-Only)
        # 2. 'git clone' from /mnt/repo to /tmp/build (Inside container)
        #    - This is fast and ignores your local uncommitted changes/garbage
        # 3. Checkout the specific hash
        # 4. Build
        let build_cmd = $"
            git clone /mnt/repo /tmp/build --quiet &&
            cd /tmp/build &&
            git checkout ($full_hash) --quiet &&
            dotnet restore CWTools/CWTools.CLI/CWTools.CLI.fsproj &&
            dotnet publish CWTools/CWTools.CLI/CWTools.CLI.fsproj -c Release -o /out --no-restore
        " | str replace --all "\r" ""

        # Run Builder (capture output for debugging)
        let build_output = (do -i { (^docker run --rm
            -v $"($repo_path):/mnt/repo:ro"
            -v $"($cache_dir):/out"
            $BUILDER_IMAGE
            /bin/bash -c $build_cmd) } | complete)

        if $build_output.exit_code != 0 {
            print $"(ansi red)Build failed! Output:(ansi reset)"
            print $build_output.stdout
            print $build_output.stderr
            exit $build_output.exit_code
        }

        # Validate cache (ensure DLL exists)
        let dll_path = ($cache_dir | path join "CWTools.CLI.dll")
        if not ($dll_path | path exists) {
            print $"(ansi red)Error: Build succeeded but ($dll_path) missing. Cache invalid.(ansi reset)"
            rm -rf $cache_dir
            exit 1
        }

        print $"   Build Cached: ($cache_dir)"
    } else {
        print $"⚡ Using cached build: ($cache_dir)"
    }

    # --- 2. RUN ---
    print "🚀 Running Validation..."

    # Hash inputs for unique output folder
    let r_hash = (ls ($rules_path | path join "**/*") | sort-by name | each {|f| $"($f.name)($f.modified)"} | str join | hash md5)
    let g_hash = (ls ($game_path | path join "**/*") | sort-by name | each {|f| $"($f.name)($f.modified)"} | str join | hash md5)

    let result_path = ($out_dir_base | path join $"($full_hash)_($r_hash)_($g_hash)")
    mkdir $result_path

    # Docker User ID fix (so you own the output files)
    let uid = (do -i { ^id -u } | str trim)
    let gid = (do -i { ^id -g } | str trim)

    let result = do -i {
        (^docker run --rm
            --user $"($uid):($gid)"
            -v $"($cache_dir):/app:ro"          # Mounted Build
            -v $"($rules_path | path expand):/rules:ro"
            -v $"($game_path | path expand):/game:ro"
            -v $"($result_path):/output"
            $RUNNER_IMAGE
            dotnet /app/CWTools.CLI.dll /rules /game /output/output.json)


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
    } | to json | save -f ($result_path | path join "metadata.json")

    print $"(ansi green)Done: ($result_path)(ansi reset)"
}
