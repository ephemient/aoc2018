plugins {
    id("org.jetbrains.dokka") version "0.9.17"
    kotlin("jvm") version "1.3.10"
    `scala`
    id("org.jmailen.kotlinter") version "1.20.1"
    application
    id("me.champeau.gradle.jmh") version "0.4.7"
    `maven-publish`
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
    implementation("org.scala-lang:scala-library:2.12.8")
    testImplementation(kotlin("test-junit5"))
    testImplementation("org.junit.jupiter:junit-jupiter-engine:5.3.2")
    jmhImplementation(kotlin("reflect"))
    jmhImplementation(kotlin("stdlib-jdk8"))
}

tasks.withType<JavaCompile> {
    sourceCompatibility = "1.8"
    targetCompatibility = "1.8"
    options.encoding = "UTF-8"
}

tasks.withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile> {
    kotlinOptions.jvmTarget = "1.8"
    val compileScala = tasks.getByName<ScalaCompile>("${name.removeSuffix("Kotlin")}Scala")
    dependsOn(compileScala)
    classpath = classpath + project.files(compileScala)
}

tasks.withType<ScalaCompile> {
    sourceCompatibility = "1.8"
    targetCompatibility = "1.8"
    setDependsOn(dependsOn.filter {
        (it as? Task)?.name ?: (it as? String) != "${name.removeSuffix("Scala")}Java"
    })
}

tasks.test {
    useJUnitPlatform()
    testLogging.showStandardStreams = true
}

val jmhExclude: String? by project
val jmhInclude: String? by project

jmh {
    benchmarkMode = listOf("sample")
    if (!jmhExclude.isNullOrEmpty()) exclude = listOf(jmhExclude)
    if (!jmhInclude.isNullOrEmpty()) include = listOf(jmhInclude)
    fork = 1
    threads = 1
    timeOnIteration = "1s"
    timeUnit = "ms"
    warmupIterations = 1
}

val dokka by tasks.getting(org.jetbrains.dokka.gradle.DokkaTask::class) {
    includes = listOf("README.md")
    jdkVersion = 8
    linkMappings.add(org.jetbrains.dokka.gradle.LinkMapping().apply {
        dir = "src"
        url = "https://github.com/ephemient/aoc2018/blob/kotlin/src"
        suffix = "#L"
    })
}

publishing {
    publications {
        create<MavenPublication>(project.name) {
            val dokkaJar by tasks.creating(Jar::class) {
                from(dokka)
                classifier = "javadoc"
            }
            artifact(dokkaJar)
        }
    }
}

val ktlintIdea by tasks.creating(JavaExec::class) {
    group = "IDE"
    description = "Apply ktlint style to IntelliJ IDEA project."
    main = "com.github.shyiko.ktlint.Main"
    classpath = buildscript.configurations["classpath"]
    args("--apply-to-idea", "-y")
}

tasks.wrapper {
    gradleVersion = "5.0"
}
