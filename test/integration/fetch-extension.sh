#!/bin/sh
# Download the Competitive Companion browser extension used by the
# integration tests into .cache/extension, unless it is already there.
# The release is pinned and its checksum checked.
set -eu

version=2.65.0
sha256=2c060ff2e49c422ec92aba6f4acd1dcc4aa4a2a9cd0912d054a8ced3fa69192d
url="https://github.com/jmerle/competitive-companion/releases/download/$version/competitive_companion-$version-chrome.zip"

here=$(cd "$(dirname "$0")" && pwd)
cache="$here/.cache"
target="$cache/extension"

if [ -f "$target/manifest.json" ] && grep -q "\"version\": \"$version\"" "$target/manifest.json"; then
    exit 0
fi

mkdir -p "$cache"
zip="$cache/competitive-companion-$version.zip"
curl --silent --show-error --fail --location --output "$zip" "$url"
echo "$sha256  $zip" | sha256sum --check --quiet

rm -rf "$target"
mkdir -p "$target"
unzip -q "$zip" -d "$target"
rm -f "$zip"
echo "Competitive Companion $version unpacked in $target"
