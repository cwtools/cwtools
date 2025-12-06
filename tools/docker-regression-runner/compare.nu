# Usage for git bisect:
# 1. Start bisect:  git bisect start <bad_commit> <good_commit>
# 2. Run script:    git bisect run nu tools/docker-regression-runner/bisect.nu <repo_path> <rules_path> <game_path> <expected_error_count>

def main [
    local_repo: path    # Path to the local git repo being bisected
    rules_path: path    # Path to rules
    game_path: path     # Path to game
    --setup             # Enable setup mode (requires --good and --bad)
    --good: string      # The "good" commit hash (setup mode only)
    --bad: string       # The "bad" commit hash (setup mode only)
    --count: int        # The expected "good" error count (run mode only)
] {
    # Locate sibling runner script
    let script_dir = ($env.FILE_PWD )
    let script_path = ($env.CURRENT_FILE)
    let runner_path = ($script_dir | path join "runner.nu")
    
    let good = if $setup { git rev-parse $good } else { null } 
    let bad = if $setup { git rev-parse $bad } else { null}

    # Ensure absolute paths
    let local_repo = ($local_repo | path expand)
    let rules_path = ($rules_path | path expand)
    let game_path = ($game_path | path expand)

    # --- SETUP MODE ---
    if $setup {
        if ($good | is-empty) or ($bad | is-empty) {
            print $"(ansi red)Error: --good and --bad are required for setup mode.(ansi reset)"
            exit 1
        }

        print $"(ansi cyan)=== Bisect Setup ===(ansi reset)"
        print $"Calculating baseline error count for good commit: ($good)..."

        # Run runner on the good commit to get baseline
        nu $runner_path $local_repo $good $rules_path $game_path

        # Locate result
        # runner.nu output logic: $PWD/regression-results/<hash>_0_0/output.json
        let result_dir = ($env.PWD | path join "regression-results" $"($good)_0_0")
        let output_json = ($result_dir | path join "output.json")

        if not ($output_json | path exists) {
            print $"(ansi red)Critical: Could not generate baseline results. File missing: ($output_json)(ansi reset)"
            exit 1
        }

        let errors = (open $output_json)
        let good_count = ($errors.errorCount)

        print $"(ansi green)Baseline established: ($good_count) errors.(ansi reset)"

        # Initialize Git Bisect
        print $"(ansi cyan)Initializing git bisect in ($local_repo)...(ansi reset)"
        
        # We must run git commands inside the target repo
        cd $local_repo

        # Reset any previous bisect state
        try { git bisect reset }
        
        # Start with --no-checkout (since runner.nu handles checkout internally in docker)
        git bisect start --no-checkout
        git bisect good $good
        git bisect bad $bad

        print $"(ansi green)Bisect initialized successfully.(ansi reset)"
        print ""
        print "To start the automated bisect, run the following command:"
        print ""
        print $"(ansi yellow)git bisect run nu ($script_path) ($local_repo) ($rules_path) ($game_path) --count ($good_count)(ansi reset)"
        print ""
        return
    }

    # --- RUN MODE (Executed by git bisect) ---
    if ($count == null) {
        print $"(ansi red)Error: --count is required for run mode.(ansi reset)"
        exit 125 # 125 = git bisect skip
    }

    # Determine commit to test
    # With --no-checkout, git updates the BISECT_HEAD ref to the commit to be tested.
    # We prefer BISECT_HEAD, but fallback to HEAD for manual testing.
    let commit_hash = (do -i {
        cd $local_repo
        if (git rev-parse --verify BISECT_HEAD | complete).exit_code == 0 {
            git rev-parse BISECT_HEAD
        } else {
            git rev-parse HEAD
        }
    } | str trim)

    print $"(ansi cyan)Bisecting: Testing commit ($commit_hash)...(ansi reset)"

    # Execute the Runner
    # Note: runner.nu writes results relative to current PWD. 
    # git bisect run executes from the repo root.
    nu $runner_path $local_repo $commit_hash $rules_path $game_path

    # Locate Output
    let result_dir = ($env.PWD | path join "regression-results" $"($commit_hash)_0_0")
    let output_json = ($result_dir | path join "output.json")

    if not ($output_json | path exists) {
        print $"(ansi red)Critical: Output file not found at ($output_json)(ansi reset)"
        exit 125 # Skip this commit
    }

    # Check Results
    let actual_count = (open $output_json | $in.errorCount)

    print $"Expected errors: ($count)"
    print $"Actual errors:   ($actual_count)"

    if $actual_count == $count {
        print $"(ansi green)Match! This commit is good.(ansi reset)"
        exit 0 # Good
    } else {
        print $"(ansi red)Mismatch! This commit is bad.(ansi reset)"
        exit 1 # Bad
    }
}