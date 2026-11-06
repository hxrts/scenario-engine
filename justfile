default:
    @just --list

# Compile the project
build:
    spago build

# Run the main module
run:
    spago run

# Run the test suite
test:
    spago test

# Bundle to dist/bundle.js
bundle:
    spago bundle --module Main --outfile dist/bundle.js --platform node

clean:
    rm -rf output .spago dist
