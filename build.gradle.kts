plugins {
    id("org.jetbrains.dokka") version "0.9.17"
    kotlin("jvm") version "1.3.10"
    id("org.jmailen.kotlinter") version "1.20.1"
    application
    id("me.champeau.gradle.jmh") version "0.4.7"
}

repositories {
    jcenter()
}

application {
    mainClassName = "io.github.ephemient.aoc2018.MainKt"
}
 
defaultTasks = listOf("test", "run")

dependencies {
    implementation(kotlin("reflect"))
    implementation(kotlin("stdlib-jdk8"))
    testImplementation(kotlin("test-junit5"))
    testImplementation("org.junit.jupiter:junit-jupiter-engine:5.3.2")
    jmhImplementation(kotlin("reflect"))
    jmhImplementation(kotlin("stdlib-jdk8"))
}

tasks.test {
    useJUnitPlatform()
    testLogging.showStandardStreams = true
}

jmh {
    benchmarkMode = listOf("sample")
    fork = 1
    threads = 1
    timeOnIteration = "1s"
    timeUnit = "ms"
    warmupIterations = 1
}

task<JavaExec>("ktlintIdea") {
    group = "IDE"
    description = "Apply ktlint style to IntelliJ IDEA project."
    main = "com.github.shyiko.ktlint.Main"
    classpath = buildscript.configurations["classpath"]
    args("--apply-to-idea", "-y")
}

tasks.getByName<Wrapper>("wrapper") {
    gradleVersion = "5.0"
}
