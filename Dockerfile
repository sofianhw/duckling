FROM haskell:9.2 AS builder

# Update sources to use archive.debian.org for buster
RUN sed -i 's/deb.debian.org/archive.debian.org/g' /etc/apt/sources.list && \
  sed -i 's|security.debian.org|archive.debian.org|g' /etc/apt/sources.list && \
  sed -i '/stretch-updates/d' /etc/apt/sources.list && \
  sed -i '/buster-updates/d' /etc/apt/sources.list

RUN apt-get update -qq && \
  apt-get install -qq -y libpcre3 libpcre3-dev build-essential pkg-config --fix-missing --no-install-recommends && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN mkdir /log

WORKDIR /duckling

ENV LANG=C.UTF-8

# Copy only dependency files first for better caching
# This allows stack setup to be cached if dependencies don't change
COPY stack.yaml duckling.cabal ./

# Use BuildKit cache mounts to persist stack directory across builds
# This caches the Hackage index (~200MB) and GHC downloads (~260MB)
# The cache persists even when Docker layers are invalidated
# Requires: DOCKER_BUILDKIT=1 docker build
RUN --mount=type=cache,target=/root/.stack \
    stack update || true

# Run stack setup with --install-ghc to ensure GHC is installed
# Uses the cached stack directory from the previous step
# GHC and dependencies will be cached and reused across builds
RUN --mount=type=cache,target=/root/.stack \
    stack setup --install-ghc

# Now copy all source code
# This layer will be rebuilt when source code changes
COPY . .

# Build and install (GHC is already available from stack setup or system)
# Uses cached stack directory for faster dependency resolution
# NOTE:`stack build` will use as many cores as are available to build
# in parallel. However, this can cause OOM issues as the linking step
# in GHC can be expensive. If the build fails, try specifying the
# '-j1' flag to force the build to run sequentially.
RUN --mount=type=cache,target=/root/.stack \
    stack install

FROM debian:bullseye

ENV LANG=C.UTF-8
ENV TZ=Asia/Jakarta

RUN apt-get update -qq && \
  apt-get install -qq -y libpcre3 libgmp10 tzdata --no-install-recommends && \
  ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && \
  echo $TZ > /etc/timezone && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Create working directory and log directory with proper permissions
WORKDIR /app
RUN mkdir -p /app/log && \
  chmod 755 /app && \
  chmod 755 /app/log

COPY --from=builder /root/.local/bin/duckling-example-exe /usr/local/bin/

EXPOSE 8000

CMD ["duckling-example-exe", "-p", "8000"]
