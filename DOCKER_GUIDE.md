# Duckling Docker Guide

Simple guide to build, run, and test Duckling with Docker.

## Prerequisites

- Docker installed and running
- `jq` installed (optional, for better test output): `brew install jq` or `sudo apt-get install jq`

## Quick Start

### 1. Build the Docker Image

```bash
DOCKER_BUILDKIT=1 docker build . -t duckling
```

### 2. Run the Docker Container

```bash
docker run -p 8000:8000 duckling
```

The server will be available at `http://localhost:8000`

### 3. Test Indonesian Time Parsing

```bash
./test_indonesian.sh
```

This will run comprehensive tests for Indonesian time expressions including:
- Relative dates (hari ini, kemarin, besok, etc.)
- Date ranges (7 hari terakhir, dalam 7 hari terakhir, etc.)
- Time expressions (pukul 14:30, jam 2 pagi, etc.)
- Date + time combinations
- Duration expressions
- And more...

## API Usage

### Parse a single expression

```bash
curl -XPOST http://localhost:8000/parse \
  --data-urlencode "locale=id_ID" \
  --data-urlencode "text=7 hari terakhir"
```

### Parse with reference time

```bash
curl -XPOST http://localhost:8000/parse \
  --data-urlencode "locale=id_ID" \
  --data-urlencode "text=besok jam 3 sore" \
  --data-urlencode "reftime=2025-12-18T10:00:00"
```

## Troubleshooting

### Port already in use

If port 8000 is already in use, stop the existing container:

```bash
docker ps | grep duckling
docker stop <container_id>
```

Or use a different port:

```bash
docker run -p 8001:8000 duckling
```

### Container keeps running

To run in detached mode (background):

```bash
docker run -d -p 8000:8000 --name duckling duckling
```

To stop it later:

```bash
docker stop duckling
docker rm duckling
```

## Notes

- The build process may take several minutes the first time
- The server starts on port 8000 by default
- Test script requires the server to be running on `http://localhost:8000`
