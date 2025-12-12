# Docker Guide for Duckling

## Overview

This guide covers building and running Duckling with Indonesian (ID) time parsing support. The Docker image is configured with:
- **Timezone**: Asia/Jakarta (WIB, UTC+7)
- **Languages**: English (EN) and Indonesian (ID) only
- **Default timezone**: Asia/Jakarta for all API requests

---

## Build and Run Locally

If you've made changes to the code (like the Indonesian time parsing improvements), you need to build the Docker image locally:

### Step 1: Build the Docker Image

```bash
# Build the image (this will take 10-30 minutes the first time)
docker build -t duckling-id:latest .

# Or with a specific tag
docker build -t duckling-id:v1.0 .
```

**What happens during build:**
- Downloads Haskell 9.2 base image
- Installs dependencies (libpcre3, build tools)
- Runs `stack setup` (downloads GHC and dependencies)
- Runs `stack install` (compiles the entire project)
- Creates a runtime image with just the binary

**Build time:** 
- First build: 20-40 minutes (downloads everything)
- Subsequent builds: 5-15 minutes (if only code changed)

### Step 2: Run the Container

```bash
# Run the container
docker run -p 8000:8000 duckling-id:latest

# Or run in detached mode (background)
docker run -d -p 8000:8000 --name duckling duckling-id:latest

# View logs
docker logs duckling

# Stop the container
docker stop duckling

# Remove the container
docker rm duckling
```

### Step 3: Test the Server

The server will be available at `http://localhost:8000`

**Quick test:**
```bash
curl -XPOST http://localhost:8000/parse \
  -d 'locale=id_ID&text=hari ini'
```

See the [Comprehensive Testing](#comprehensive-testing) section below for full test suite.

---

## Docker Commands Cheat Sheet

```bash
# Build
docker build -t duckling-id:latest .

# Run (foreground)
docker run -p 8000:8000 duckling-id:latest

# Run (background)
docker run -d -p 8000:8000 --name duckling duckling-id:latest

# View logs
docker logs -f duckling

# Stop
docker stop duckling

# Start (if stopped)
docker start duckling

# Remove container
docker rm duckling

# Remove image
docker rmi duckling-id:latest

# List running containers
docker ps

# List all containers (including stopped)
docker ps -a

# List images
docker images

# Execute command in running container
docker exec -it duckling /bin/bash

# View container resource usage
docker stats duckling
```

---

## Troubleshooting

### Build Fails with Out of Memory (OOM)

The Dockerfile mentions that parallel builds can cause OOM. If this happens:

1. **Option 1:** Build with single core (slower but safer)
   ```bash
   # Edit Dockerfile line 30, change to:
   RUN stack install --install-ghc -j1
   ```

2. **Option 2:** Increase Docker memory limit
   - Docker Desktop: Settings → Resources → Memory (increase to 4GB+)

### Build Takes Too Long

**Optimizations already in place:**
- BuildKit cache mounts for Stack index and GHC (persists across builds)
- Only EN and ID languages are compiled (much faster)
- Layer caching is optimized for dependency changes

**IMPORTANT: Always use BuildKit for faster builds:**

The Dockerfile uses BuildKit cache mounts which require BuildKit to be enabled:

```bash
# Build with BuildKit (REQUIRED for cache mounts to work)
DOCKER_BUILDKIT=1 docker build -t duckling-id:latest .

# Or enable BuildKit permanently:
export DOCKER_BUILDKIT=1
# Then just use: docker build -t duckling-id:latest .
```

**Build time expectations (with BuildKit):**
- First build: 20-40 minutes (downloads Hackage index ~200MB and GHC ~260MB)
- Subsequent builds (code changes only): 2-5 minutes (uses cached index and GHC)
- Subsequent builds (dependency changes): 5-10 minutes (index cached, only new deps download)

**Without BuildKit:**
- First build: 20-40 minutes
- Subsequent builds: 10-20 minutes (index re-downloads if cache invalidated)

### Port Already in Use

If port 8000 is already in use:

```bash
# Use a different port (e.g., 8001)
docker run -p 8001:8000 duckling-id:latest

# Then access at http://localhost:8001
```

### Test Indonesian Locale

Make sure to use `id_ID` or `id` as the locale:

```bash
curl -XPOST http://localhost:8000/parse \
  -d 'locale=id_ID&text=hari ini&dims=["time"]'
```

---

## Development Workflow

1. **Make code changes** (e.g., in `Duckling/Time/ID/Rules.hs`)
2. **Rebuild Docker image:**
   ```bash
   docker build -t duckling-id:latest .
   ```
3. **Stop old container:**
   ```bash
   docker stop duckling
   docker rm duckling
   ```
4. **Run new container:**
   ```bash
   docker run -d -p 8000:8000 --name duckling duckling-id:latest
   ```
5. **Test your changes:**
   ```bash
   curl -XPOST http://localhost:8000/parse \
     -d 'locale=id_ID&text=YOUR_TEST_TEXT&dims=["time"]'
   ```

---

## Alternative: Run Without Docker

If you have Haskell/Stack installed locally:

```bash
# Install dependencies
stack setup

# Build
stack build

# Run
stack exec duckling-example-exe
```

This is faster for development but requires Haskell environment setup.

---

## Comprehensive Testing

After starting the container, you can test all supported Indonesian time expressions. The following test suite covers all implemented features:

### Test Script

Save this as `test_indonesian.sh`:

```bash
#!/bin/bash

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
echo "=================================="
echo ""

for expr in "${expressions[@]}"; do
  echo "Testing: '$expr'"
  result=$(curl -s -XPOST http://localhost:8000/parse \
    --data-urlencode "locale=id_ID" \
    --data-urlencode "text=$expr")
  
  if echo "$result" | jq -e 'length > 0' > /dev/null 2>&1; then
    echo "$result" | jq -r '.[0] | "✓ Body: " + .body + "\n  Dimension: " + .dim + "\n  Value: " + (if .value.type == "interval" then "interval: " + .value.from.value + " to " + .value.to.value else .value.value end)'
  else
    echo "✗ No match"
  fi
  echo ""
done
```

Make it executable and run:
```bash
chmod +x test_indonesian.sh
./test_indonesian.sh
```

### Individual Test Examples

#### 1. Relative Time Expressions

```bash
# Today
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=hari ini'

# Yesterday
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=kemarin'

# Tomorrow
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=besok'

# Day after tomorrow
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=lusa'

# Now
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=sekarang'
```

#### 2. Week Expressions

```bash
# Next week
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=minggu depan'

# Last week (informal)
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=minggu kemaren'

# Last week (formal)
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=minggu kemarin'

# N weeks ago
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=3 minggu lalu'

# This week (returns interval)
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=minggu ini'

# Weekend
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=akhir minggu'
```

#### 3. Month Expressions

```bash
# Next month
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=bulan depan'

# Last month
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=bulan lalu'

# This month (returns interval)
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=bulan ini'

# Beginning of month (returns interval)
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=awal bulan'

# Month to date
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=bulan ini sampai sekarang'
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=dari awal bulan sampai sekarang'
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=sejak awal bulan'
```

#### 4. Year Expressions

```bash
# Next year
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=tahun depan'
```

#### 5. Duration-Based Expressions

```bash
# In X duration
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=dalam 2 hari'

# X duration later
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=2 hari lagi'

# X duration ago
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=2 hari yang lalu'
```

#### 6. Date Formats

```bash
# DD/MM/YYYY
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=25/12/2024'

# DD/MM (current year)
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=25/12'

# DD-MM-YYYY
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=25-12-2024'

# ISO format
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=2025-11-12'

# YYYYMMDD (no separators)
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=20251112'

# With month name
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=13 desember'
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=14 februari 2025'
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=1 jan 2024'
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=25 desember 2025'

# Month and year only
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=Jan 2025'
```

#### 7. Time Expressions

```bash
# 24-hour format
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=pukul 14:30'
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=jam 14'

# 12-hour format with part of day
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=jam 3 sore'
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=pukul 2 pagi'
```

#### 8. Part of Day Combinations

```bash
# Tomorrow morning
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=besok pagi'

# Last night
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=tadi malam'
```

#### 9. Interval Expressions

```bash
# From X to Y
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=dari 2 minggu lalu sampai sekarang'

# X until Y
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=minggu lalu sampai sekarang'
```

#### 10. Days of Week

```bash
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=senin'
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=selasa'
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=rabu'
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=kamis'
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=jumat'
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=sabtu'
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=minggu'
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=ahad'
```

#### 11. Indonesian Holidays

```bash
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=hari kemerdekaan'
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=17 agustusan'
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=tahun baru'
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=hari pahlawan'
curl -XPOST http://localhost:8000/parse -d 'locale=id_ID&text=hari raya natal'
```

### Expected Results

All expressions should return valid JSON with:
- `body`: The matched text
- `dim`: Dimension type (usually "time")
- `value`: The parsed time value (ISO 8601 format with WIB timezone, UTC+7)

**Example response:**
```json
[{
  "body": "hari ini",
  "dim": "time",
  "end": 8,
  "latent": false,
  "start": 0,
  "value": {
    "grain": "day",
    "type": "value",
    "value": "2025-12-12T00:00:00.000+07:00",
    "values": [{
      "grain": "day",
      "type": "value",
      "value": "2025-12-12T00:00:00.000+07:00"
    }]
  }
}]
```

**Note**: All times are returned in WIB (UTC+7) timezone by default.

---

## Notes

- **Timezone**: All times are returned in WIB (Asia/Jakarta, UTC+7) by default
- **Languages**: Only English (EN) and Indonesian (ID) are compiled for faster builds
- **Image size**: ~1-2GB due to Haskell dependencies
- **Build time**: Depends on your machine (CPU, RAM, disk speed)
- **Port**: Server runs on port 8000 by default
- **Dimensions**: All dimensions are enabled by default, but you can specify with `dims` parameter
- **BuildKit**: Always use `DOCKER_BUILDKIT=1` for faster builds with cache persistence

