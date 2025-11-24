# Maven Quick Reference for Lab05

## 🚀 Common Commands

### Build & Compile
```bash
mvn clean                   # Clean build directory
mvn compile                 # Compile source code
mvn package                 # Build JAR file
mvn clean package           # Clean and build
mvn clean compile           # Clean and compile only
```

### Run Application
```bash
# Run with custom arguments
mvn exec:java -Dexec.args="--compare --size 5000 --threads 8"

# Run predefined profiles
mvn exec:java@compare           # Compare all 4 variants
mvn exec:java@naive-seq         # Naive sequential
mvn exec:java@naive-par         # Naive parallel
mvn exec:java@karatsuba-seq     # Karatsuba sequential
mvn exec:java@karatsuba-par     # Karatsuba parallel

# Quiet mode (less output)
mvn exec:java@compare -q

# Run from JAR
mvn package
java -jar target/lab05.jar --compare --size 4000
```

### Example Runs
```bash
# Small benchmark
mvn exec:java -Dexec.args="--compare --size 2000 --threads 4 --repeats 3"

# Large benchmark
mvn exec:java -Dexec.args="--compare --size 50000 --threads 16 --repeats 5"

# Single variant with tuning
mvn exec:java -Dexec.args="--algorithm karatsuba --mode par --size 100000 --threads 8 --karatsuba-max-depth 4"

# Show help
mvn exec:java -Dexec.args="--help"
```

## 📦 Project Structure

```
lab05/
├── pom.xml                     Maven project file
├── src/
│   └── main/
│       └── java/
│           └── ppd/
│               └── lab05/
│                   ├── Main.java
│                   ├── Polynomial.java
│                   └── PolynomialMultiply.java
└── target/                     Build output (auto-generated)
    ├── classes/                Compiled .class files
    └── lab05.jar               Executable JAR
```

## 🔧 Configuration

### Java Version
Edit `pom.xml` to change Java version:
```xml
<properties>
    <maven.compiler.source>25</maven.compiler.source>
    <maven.compiler.target>25</maven.compiler.target>
</properties>
```

### JVM Options
```bash
# Increase heap size
mvn exec:java -Dexec.args="--compare --size 200000" \
  -Dexec.jvmArgs="-Xmx4g"

# Or set globally
export MAVEN_OPTS="-Xmx4g -XX:+UseG1GC"
mvn exec:java -Dexec.args="--compare --size 200000"
```

## 🎯 Benchmarking Workflow

### 1. Quick Test
```bash
mvn compile
mvn exec:java -Dexec.args="--compare --size 1000 --repeats 2"
```

### 2. Performance Analysis
```bash
# Test different sizes
for SIZE in 2000 5000 10000 20000 50000; do
  echo "=== Size: $SIZE ==="
  mvn exec:java -q -Dexec.args="--compare --size $SIZE --threads 8 --repeats 5"
done
```

### 3. Thread Scaling
```bash
# Test different thread counts
for THREADS in 1 2 4 8 16; do
  echo "=== Threads: $THREADS ==="
  mvn exec:java -q -Dexec.args="--algorithm naive --mode par --size 20000 --threads $THREADS --repeats 5"
done
```

## 📊 IDE Integration

### IntelliJ IDEA
1. File → Open → Select `pom.xml`
2. Let Maven import the project
3. Run configurations will be auto-detected

### VS Code
1. Install Java Extension Pack
2. Open `lab05` folder
3. Maven tasks appear in sidebar

### Eclipse
1. File → Import → Existing Maven Projects
2. Select `lab05` folder
3. Click Finish

### Zed Editor
1. Open `lab05` folder
2. JDTLS will auto-detect Maven project
3. No "package mismatch" errors! ✅

## 🐛 Troubleshooting

### Maven not found
```bash
# macOS
brew install maven

# Check version
mvn --version
```

### Java version mismatch
```bash
# Check current Java
java -version

# With jenv
jenv local 25
jenv version
```

### Clean rebuild
```bash
mvn clean
rm -rf target/
mvn compile
```

### Dependencies issue
```bash
# Force update
mvn clean install -U

# Clear local cache
rm -rf ~/.m2/repository/ppd/lab05
```

### Out of memory
```bash
# Increase heap
export MAVEN_OPTS="-Xmx4g"

# Or per-command
mvn exec:java -Dexec.args="..." -Dexec.jvmArgs="-Xmx8g"
```
