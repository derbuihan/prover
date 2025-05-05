#!/bin/bash

# This script runs in the context of the project root directory.

run_test() {
    local test_file=$1
    echo "Running proof: $test_file"
    output=$(cat "$test_file" | cabal run 2>&1)
    if echo "$output" | grep -q "Proof completed successfully!"; then
        echo "✓ Proof verified successfully!"
    else
        echo "✗ Proof verification failed!"
        echo "------- Output -------"
        echo "$output"
        echo "---------------------"
        exit 1
    fi
}

# Run the tests
proof_files=$(find examples -name "*.txt" | sort)
for proof_file in $proof_files; do
    run_test "$proof_file"
done
