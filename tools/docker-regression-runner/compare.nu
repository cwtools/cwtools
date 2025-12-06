#!/usr/bin/env nu

# Usage for git bisect:
# 1. Start bisect:  git bisect start <bad_commit> <good_commit>
# 2. Run script:    git bisect run nu tools/docker-regression-runner/bisect.nu <repo_path> <rules_path> <game_path> <expected_error_count>

def main [
    local_repo: path    # Path to the local git repo being bisected
    rules_path: path    # Path to rules
    game_path: path     # Path to game
    good_count: int     # The number of errors in the "good" state
] {
    let current_commit = (git rev-parse HEAD | str trim)
    print $"(ansi cyan)Bisecting: Testing commit ($current_commit)...(ansi reset)"

    # 1. Run the existing runner script
    # We use 'nu' to call the sibling script. 
    # We point to the current directory of this script to find runner.nu.
    let script_dir = ($env.FILE_PWD | path dirname)
    let runner_path = ($script_dir | path join "runner.nu")
    
    # We invoke the runner. Note: runner.nu writes to regression-results/ based on hash
    # We force-rerun to ensure we aren't picking up a stale result from a different run attempt
    nu $runner_path $local_repo $current_commit $rules_path $game_path

    # 2. Locate the output file
    # runner.nu logic: $root | path join "regression-results" | path join $"($full_hash)_0_0"
    let root = ($env.PWD | path expand)
    let result_dir = ($root | path join "regression-results" $"($current_commit)_0_0")
    let output_json = ($result_dir | path join "output.json")

    if not ($output_json | path exists) {
        print $"(ansi red)Critical: Output file not found at ($output_json)(ansi reset)"
        # 125 tells git bisect to "skip" this commit because it's untestable
        exit 125 
    }

    # 3. Parse the error count
    # Structure of output.json is typically a list of error objects. We count the length.
    let errors = (open $output_json)
    let actual_count = ($errors | length)

    print $"Expected errors: ($good_count)"
    print $"Actual errors:   ($actual_count)"

    # 4. Determine Status
    if $actual_count == $good_count {
        print $"(ansi green)Match! This commit is considered 'good' (count unchanged).(ansi reset)"
        exit 0 # Good
    } else {
        print $"(ansi red)Change detected! This commit is considered 'bad'.(ansi reset)"
        exit 1 # Bad
    }
}