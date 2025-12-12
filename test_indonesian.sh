#!/bin/bash

# Duckling Indonesian Time Parsing Test Script
# This script tests all supported Indonesian time expressions

# Test expressions
expressions=(
  "hari ini"
  "kemarin"
  "besok"
  "lusa"
  "minggu depan"
  "bulan depan"
  "bulan lalu"
  "tahun depan"
  "2 hari lagi"
  "3 minggu lalu"
  "akhir minggu"
  "awal bulan"
  "jam 3 sore"
  "besok pagi"
  "tadi malam"
  "13 desember"
  "14 februari 2025"
  "1 jan 2024"
  "25 desember 2025"
  "2025-11-12"
  "20251112"
  "25/12/2024"
  "25/12"
  "12 Jan 2025"
  "Jan 2025"
)

echo "=================================="
echo "Duckling Indonesian Time Parsing Test"
echo "Test Date: $(date)"
echo "Timezone: WIB (UTC+7)"
echo "=================================="
echo ""

# Check if jq is installed
if ! command -v jq &> /dev/null; then
  echo "Warning: jq is not installed. Install it for better output formatting."
  echo "On macOS: brew install jq"
  echo "On Ubuntu: sudo apt-get install jq"
  echo ""
  USE_JQ=false
else
  USE_JQ=true
fi

# Check if server is running
if ! curl -s http://localhost:8000/parse > /dev/null 2>&1; then
  echo "Error: Duckling server is not running on http://localhost:8000"
  echo "Please start the server first:"
  echo "  docker run -d -p 8000:8000 --name duckling duckling"
  exit 1
fi

passed=0
failed=0

for expr in "${expressions[@]}"; do
  echo "Testing: '$expr'"
  
  result=$(curl -s -XPOST http://localhost:8000/parse \
    --data-urlencode "locale=id_ID" \
    --data-urlencode "text=$expr")
  
  if [ "$USE_JQ" = true ]; then
    if echo "$result" | jq -e 'length > 0' > /dev/null 2>&1; then
      echo "$result" | jq -r '.[0] | "✓ Body: " + .body + "\n  Dimension: " + .dim + "\n  Value: " + (if .value.type == "interval" then "interval: " + .value.from.value + " to " + .value.to.value else .value.value end)'
      ((passed++))
    else
      echo "✗ No match"
      ((failed++))
    fi
  else
    # Fallback without jq
    if echo "$result" | grep -q '"body"'; then
      echo "✓ Match found"
      echo "$result"
      ((passed++))
    else
      echo "✗ No match"
      ((failed++))
    fi
  fi
  echo ""
done

echo "=================================="
echo "Test Summary:"
echo "  Passed: $passed"
echo "  Failed: $failed"
echo "  Total:  $((passed + failed))"
echo "=================================="

if [ $failed -eq 0 ]; then
  echo "✅ All tests passed!"
  exit 0
else
  echo "❌ Some tests failed"
  exit 1
fi

