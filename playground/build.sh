#!/bin/bash
set -e

echo "Building quamina-rs Playground..."

# Check if wasm-pack is installed
if ! command -v wasm-pack &> /dev/null; then
    echo "wasm-pack is not installed. Installing..."
    cargo install wasm-pack
fi

# Check if wasm32 target is installed
if ! rustup target list --installed | grep -q "wasm32-unknown-unknown"; then
    echo "Adding wasm32-unknown-unknown target..."
    rustup target add wasm32-unknown-unknown
fi

# Extract version from main Cargo.toml
VERSION=$(grep '^version = ' ../Cargo.toml | head -1 | sed 's/.*"\(.*\)".*/\1/')
echo "Extracted version: $VERSION"

# Update version in HTML file if argument provided
if [ "$1" = "update-version" ]; then
    echo "Updating version in index.html..."
    sed -i '' "s/<span id=\"version\">.*<\/span>/<span id=\"version\">v$VERSION<\/span>/" web/index.html 2>/dev/null || \
    sed -i "s/<span id=\"version\">.*<\/span>/<span id=\"version\">v$VERSION<\/span>/" web/index.html
fi

# Build the WASM module
echo "Building WASM module..."
wasm-pack build --target web --out-dir pkg --release

# Copy WASM files to web directory
echo "Copying WASM files to web directory..."
cp -r pkg web/

echo "Build complete!"
echo ""
echo "To run locally:"
echo "  python3 serve.py"
echo ""
echo "Then open: http://localhost:8000"
