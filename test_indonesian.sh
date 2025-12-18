#!/bin/bash

# Duckling Indonesian Time Parsing Test Script
# This script tests all supported Indonesian time expressions
# Organized by category for comprehensive coverage

# ============================================
# 1. RELATIVE DATES (Single dates)
# ============================================
relative_dates=(
  "hari ini"
  "kemarin"
  "besok"
  "lusa"
  "kemarin lusa"
  "minggu depan"
  "minggu lalu"
  "bulan depan"
  "bulan lalu"
  "tahun depan"
  "tahun lalu"
)

# ============================================
# 2. RELATIVE DATES WITH NUMBERS
# ============================================
relative_dates_numbers=(
  "2 hari lagi"
  "3 hari lalu"
  "7 hari lalu"
  "2 minggu lalu"
  "3 minggu lalu"
  "1 bulan lalu"
  "2 bulan lalu"
)

# ============================================
# 3. DATE RANGES
# ============================================
date_ranges=(
  "7 hari terakhir"
  "dalam 7 hari terakhir"
  "2 hari terakhir"
  "14 hari terakhir"
  "minggu ini"
  "bulan ini"
  "akhir minggu"
  "awal bulan"
  "dari 1 januari sampai 31 januari"
  "dari 1 jan sampai 15 jan"
  "1 sampai 7 desember"
)

# ============================================
# 4. TIME OF DAY (Hours and Minutes)
# ============================================
time_expressions=(
  "pukul 14:30"
  "pukul 14.30"
  "jam 14:30"
  "jam 14"
  "pukul 14"
  "jam 2 pagi"
  "pukul 2 pagi"
  "jam 2 sore"
  "pukul 2 sore"
  "jam 10 malam"
  "pukul 3 siang"
  "jam 9 pagi"
)

# ============================================
# 5. PART OF DAY
# ============================================
part_of_day=(
  "pagi"
  "siang"
  "sore"
  "malam"
)

# ============================================
# 6. DATE + TIME COMBINATIONS
# ============================================
date_time_combinations=(
  "besok pagi"
  "besok siang"
  "besok sore"
  "besok malam"
  "kemarin pagi"
  "kemarin malam"
  "tadi malam"
  "hari ini jam 3"
  "besok jam 14:30"
  "kemarin pukul 10 pagi"
  "13 desember jam 15:00"
  "besok sore jam 2"
)

# ============================================
# 7. SPECIFIC DATES
# ============================================
specific_dates=(
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
  "15/01/2025"
  "15-01-2025"
  "15.01.2025"
)

# ============================================
# 8. DAYS OF WEEK
# ============================================
days_of_week=(
  "senin"
  "selasa"
  "rabu"
  "kamis"
  "jumat"
  "sabtu"
  "minggu"
  "ahad"
)

# ============================================
# 9. DURATION EXPRESSIONS
# ============================================
duration_expressions=(
  "dalam 2 jam"
  "dalam 30 menit"
  "dalam 1 hari"
  "dalam 3 minggu"
  "2 jam kemudian"
  "30 menit kemudian"
  "1 hari kemudian"
  "2 jam yang lalu"
  "30 menit yang lalu"
  "1 hari yang lalu"
)

# ============================================
# 10. HOLIDAYS
# ============================================
holidays=(
  "hari raya idul fitri"
  "hari raya natal"
  "hari kemerdekaan"
  "hari pahlawan"
  "hari guru nasional"
)

# Combine all test expressions
expressions=(
  "${relative_dates[@]}"
  "${relative_dates_numbers[@]}"
  "${date_ranges[@]}"
  "${time_expressions[@]}"
  "${part_of_day[@]}"
  "${date_time_combinations[@]}"
  "${specific_dates[@]}"
  "${days_of_week[@]}"
  "${duration_expressions[@]}"
  "${holidays[@]}"
)

echo "=================================="
echo "Duckling Indonesian Time Parsing Test"
echo "Test Date: $(date)"
echo "Timezone: WIB (UTC+7)"
echo "=================================="
echo ""
echo "Test Categories:"
echo "  1. Relative Dates: ${#relative_dates[@]} tests"
echo "  2. Relative Dates with Numbers: ${#relative_dates_numbers[@]} tests"
echo "  3. Date Ranges: ${#date_ranges[@]} tests"
echo "  4. Time Expressions: ${#time_expressions[@]} tests"
echo "  5. Part of Day: ${#part_of_day[@]} tests"
echo "  6. Date + Time Combinations: ${#date_time_combinations[@]} tests"
echo "  7. Specific Dates: ${#specific_dates[@]} tests"
echo "  8. Days of Week: ${#days_of_week[@]} tests"
echo "  9. Duration Expressions: ${#duration_expressions[@]} tests"
echo "  10. Holidays: ${#holidays[@]} tests"
echo "  Total: ${#expressions[@]} tests"
echo ""
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

# Function to format output based on value type
format_result() {
  local result=$1
  if [ "$USE_JQ" = true ]; then
    if echo "$result" | jq -e 'length > 0' > /dev/null 2>&1; then
      # Use a simpler jq expression to avoid nested conditional issues
      local body=$(echo "$result" | jq -r '.[0].body')
      local dim=$(echo "$result" | jq -r '.[0].dim')
      local value_type=$(echo "$result" | jq -r '.[0].value.type // "unknown"')
      
      echo "✓ Body: $body"
      echo "  Dimension: $dim"
      
      if [ "$value_type" = "interval" ]; then
        local from=$(echo "$result" | jq -r '.[0].value.from.value')
        local to=$(echo "$result" | jq -r '.[0].value.to.value')
        local grain=$(echo "$result" | jq -r '.[0].value.from.grain // ""')
        echo "  Type: interval"
        echo "  From: $from"
        echo "  To: $to"
        if [ -n "$grain" ] && [ "$grain" != "null" ]; then
          echo "  Grain: $grain"
        fi
      elif [ "$value_type" = "time" ]; then
        local value=$(echo "$result" | jq -r '.[0].value.value')
        local grain=$(echo "$result" | jq -r '.[0].value.grain // ""')
        echo "  Type: time"
        echo "  Value: $value"
        if [ -n "$grain" ] && [ "$grain" != "null" ]; then
          echo "  Grain: $grain"
        fi
      else
        local value=$(echo "$result" | jq -r '.[0].value | tostring')
        echo "  Value: $value"
      fi
      return 0
    else
      return 1
    fi
  else
    # Fallback without jq
    if echo "$result" | grep -q '"body"'; then
      echo "✓ Match found"
      echo "$result" | head -20
      return 0
    else
      return 1
    fi
  fi
}

# Test by category for better organization
test_category() {
  local category_name=$1
  shift
  local category_exprs=("$@")
  
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo "Category: $category_name (${#category_exprs[@]} tests)"
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo ""
  
  local category_passed=0
  local category_failed=0
  
  for expr in "${category_exprs[@]}"; do
    echo "Testing: '$expr'"
    
    result=$(curl -s -XPOST http://localhost:8000/parse \
      --data-urlencode "locale=id_ID" \
      --data-urlencode "text=$expr")
    
    if format_result "$result"; then
      ((category_passed++))
      ((passed++))
    else
      echo "✗ No match"
      ((category_failed++))
      ((failed++))
    fi
    echo ""
  done
  
  echo "  Category Summary: $category_passed passed, $category_failed failed"
  echo ""
}

# Run tests by category
test_category "Relative Dates" "${relative_dates[@]}"
test_category "Relative Dates with Numbers" "${relative_dates_numbers[@]}"
test_category "Date Ranges" "${date_ranges[@]}"
test_category "Time Expressions (Hours/Minutes)" "${time_expressions[@]}"
test_category "Part of Day" "${part_of_day[@]}"
test_category "Date + Time Combinations" "${date_time_combinations[@]}"
test_category "Specific Dates" "${specific_dates[@]}"
test_category "Days of Week" "${days_of_week[@]}"
test_category "Duration Expressions" "${duration_expressions[@]}"
test_category "Holidays" "${holidays[@]}"

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

