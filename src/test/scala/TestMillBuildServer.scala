import java.io.File
import java.nio.file.Path
import java.util.concurrent.ExecutionException

import build_client.TestBuildClient
import ch.epfl.scala.bsp4j.{BuildClientCapabilities, BuildServer, BuildServerCapabilities, BuildTarget, BuildTargetCapabilities, BuildTargetIdentifier, CleanCacheParams, CompileParams, DependencySourcesParams, DiagnosticSeverity, InitializeBuildParams, InitializeBuildResult, InverseSourcesParams, ResourcesParams, RunParams, ScalaBuildServer, ScalaBuildTarget, ScalaPlatform, SourceItem, SourceItemKind, SourcesParams, StatusCode, TestParams, TextDocumentIdentifier}
import com.google.gson.JsonObject
import org.scalatest.{BeforeAndAfterEach, BeforeAndAfterEachTestData, FunSuite, TestData}

import collection.JavaConverters._

class TestMillBuildServer extends FunSuite with BeforeAndAfterEach {

    /*
     *  Testing strategy and partitions:
     * - JavaModule, ScalaModule, TestModule, Tests, JUnitTests, CrossScalaModule
     *
     * - no request accepted before initialize
     *
     * - initialize/initialized/shutdown/exit/buildTargets
     * /buildTargetSources/buildTargetInverseSource/buildTargetDependencySources/
     * /buildTargetCleanCache/buildTargetCompile/buildTargetTest/buildTargetRun/
     * scalaMainClasses/scalacOptions/scalaTestClasses
     *
     * - nested modules, module depending on other module, 1 root module, >1 root module
     *
     * - has/hasn't ivyDeps/compileIvyDeps
     * - has/hasn't scalacOptions/scalacPluginIvyDeps
     * - has/hasn't forkArgs/forkEnv ( for run and test)
     * - has repositories
     * - has unmanagedClasspath
     * - defines a mainClass
     *
     * - has compilation error, warning, information, and no diagnostics
     * - has runtime error
     * - has test failures
     */
    val client = new TestBuildClient()
    val path = "/home/alexandra/mill/scratch"
    val serverCommand = Array("/home/alexandra/mill/out/dev/launcher/dest/run",
                              "-i", "mill.contrib.MainMillBuildServer/startServer")
    val mill_exercise = new BuildTargetIdentifier(getUri("mill_exercise/"))
    val test = new BuildTargetIdentifier(getUri("mill_exercise/test/"))
    val random = new BuildTargetIdentifier(getUri("random/"))
    var server: BuildServer with ScalaBuildServer = _

    override def afterEach: Unit = {
      server.buildShutdown()
      server.onBuildExit()
    }

    override def beforeEach: Unit = {
      server = establishServerConnection(serverCommand).asInstanceOf[BuildServer with ScalaBuildServer]
      Thread.sleep(500)
      initializeServer(server)
      server.onBuildInitialized()
      server.buildTargetCleanCache(new CleanCacheParams(List(mill_exercise, test, random).asJava))
    }

    private[this] def establishServerConnection(startServerCommand: Array[String]): BuildServer = {
      val process = Runtime.getRuntime.exec(startServerCommand, null, new File(path))
      val inStream = process.getInputStream
      val outStream = process.getOutputStream
      MainBuildClient.generateBuildServer(client, inStream, outStream)
    }

    private[this] def getUri(relativePath: String): String = {
      new File(path).toPath.resolve(relativePath).toUri.toString
    }

    private[this] def assertMillExercise(target: BuildTarget): Unit = {
      assert(target.getBaseDirectory == getUri("mill_exercise/"),
        "incorrect base directory")
      assert(target.getDisplayName == "mill_exercise", "incorrect display name")
      assert(target.getCapabilities == new BuildTargetCapabilities(true, false, true),
         "incorrect capabilities for this target")
      assert(target.getLanguageIds.contains("scala") && target.getLanguageIds.contains("java"),
                "does not support both languages scala and java")
      assert(target.getDependencies.isEmpty, "mill_exercise does not have any module dependencies")
      assert(target.getDataKind == "scala", "data kind for this target should be scala")
  }

    private[this] def assertTestTarget(target: BuildTarget): Unit = {
      assert(target.getBaseDirectory == getUri("mill_exercise/test/"),
        "incorrect base directory")
      assert(target.getDisplayName == "test", "incorrect display name")
      assert(target.getCapabilities == new BuildTargetCapabilities(true, true, true),
        "incorrect capabilities for this target")
      assert(target.getLanguageIds.contains("scala") && target.getLanguageIds.contains("java"),
        "does not support both languages scala and java")
      assert(target.getDependencies.asScala == List(new BuildTargetIdentifier(getUri("mill_exercise/"))),
        "mill_exercise.test depends on mill_exercise")
      assert(target.getDataKind == "scala", "data kind for this target should be scala")
    }

    private[this] def assertScalaBuildTarget(target: JsonObject): Unit = {
      val scalaVersion = target.get("scalaVersion").getAsString
      assert(target.get("scalaOrganization").getAsString == "org.scala-lang", "incorrect scala organization")
      assert(scalaVersion == "2.12.8", "incorrect scala version")
      assert(target.get("scalaBinaryVersion").getAsString == "2.12", "incorrect scala binary version")
      assert(target.get("platform").getAsInt == 1, "wrong platform type")
      assert(target.get("jars").getAsJsonArray.size() == 3, "wrong number fo scala jars")
      for (jar <- target.get("jars").getAsJsonArray.iterator().asScala) {
        assert(jar.getAsString.contains("scala-library-" + scalaVersion + ".jar") ||
                jar.getAsString.contains("scala-reflect-" + scalaVersion + ".jar") ||
          jar.getAsString.contains("scala-compiler-" + scalaVersion + ".jar"))
      }
    }

    private[this] def initializeServer(server: BuildServer with ScalaBuildServer): InitializeBuildResult = {
      server.buildInitialize(new InitializeBuildParams(
        "test-client",
        "1.0.0",
        "2.0.0-M4",
        getUri("./"),
        new BuildClientCapabilities(List("java", "scala").asJava)
      )).get
    }

    //TODO: see if main classes can be retrieved even if compilation fails
    test("verifying that build targets are constructed properly in mill_exercise") {
      val buildTargets = server.workspaceBuildTargets().get.getTargets.asScala
      assert(buildTargets.length == 3, "Incorrect number of targets")
      val mill_exercise = buildTargets.filter(t => t.getDisplayName == "mill_exercise").head
      val test = buildTargets.filter(t => t.getDisplayName == "test").head
      assertMillExercise(mill_exercise)
      assertScalaBuildTarget(mill_exercise.getData.asInstanceOf[JsonObject])
      assertTestTarget(test)
      assertScalaBuildTarget(test.getData.asInstanceOf[JsonObject])
    }

    private[this] def assertInitialize(server: BuildServer with ScalaBuildServer): Unit = {
      val initializeResult = initializeServer(server)
      assert(initializeResult.getBspVersion == "2.0.0-M4", "wrong bsp version")
      assert(initializeResult.getDisplayName == "mill-bsp")
      val compileLang = initializeResult.getCapabilities.getCompileProvider.getLanguageIds
      val runLang = initializeResult.getCapabilities.getRunProvider.getLanguageIds
      val testLang = initializeResult.getCapabilities.getTestProvider.getLanguageIds
      assert(compileLang.contains("scala") || compileLang.contains("java"), "wrong server compile provider")
      assert(runLang.contains("scala") || compileLang.contains("java"), "wrong server run provider")
      assert(testLang.contains("scala") || compileLang.contains("java"), "wrong server test provider")
      assert(initializeResult.getCapabilities.getDependencySourcesProvider,
        "server does have depencency sources capabilities")
      assert(initializeResult.getCapabilities.getInverseSourcesProvider,
              "server does have inverse sources capabilties")
    }

    test("verifying that no request/notification sent before initialize is successful" +
      "and that initialization occurs") {
      val server = establishServerConnection(serverCommand).asInstanceOf[BuildServer with ScalaBuildServer]
      Thread.sleep(500)
      assertThrows[ExecutionException](server.workspaceBuildTargets().get)
      assertThrows[ExecutionException](server.buildTargetCleanCache(new CleanCacheParams(List().asJava)).get)
      assertThrows[ExecutionException](server.buildTargetCompile(new CompileParams(List().asJava)).get)
      assertThrows[ExecutionException](server.buildTargetSources(new SourcesParams(List().asJava)).get)
      assertThrows[ExecutionException](server.buildTargetResources(new ResourcesParams(List().asJava)).get)
      assertThrows[ExecutionException](server.buildTargetDependencySources(new DependencySourcesParams(List().asJava)).get)
      assertThrows[ExecutionException](server.buildTargetInverseSources(new InverseSourcesParams(new TextDocumentIdentifier(""))).get)
      assertThrows[ExecutionException](server.buildTargetRun(new RunParams(new BuildTargetIdentifier(""))).get)
      assertThrows[ExecutionException](server.buildTargetTest(new TestParams(List().asJava)).get)
      assertInitialize(server)
      server.onBuildInitialized()
      server.buildShutdown()
      server.onBuildExit()
    }

    private[this] def getTargetIds(server: BuildServer with ScalaBuildServer): List[BuildTargetIdentifier] = {
      server.workspaceBuildTargets().get.getTargets.asScala.map(target => target.getId).toList
    }

    test("testing sources are retrieved correctly") {
      val sourcesResult = server.buildTargetSources(
              new SourcesParams(getTargetIds(server).asJava)).get
      val sources = sourcesResult.getItems.asScala.flatMap(item => item.getSources.asScala)
      assert(sources.contains(new SourceItem(getUri("mill_exercise/src/Bill.scala"), SourceItemKind.FILE,
        false)))
      assert(sources.contains(new SourceItem(getUri("mill_exercise/src/Compiler.scala"), SourceItemKind.FILE,
        false)))
      assert(sources.contains(new SourceItem(getUri("mill_exercise/src/AddFile.scala"), SourceItemKind.FILE,
        false)))
      assert(sources.contains(new SourceItem(getUri("mill_exercise/test/src/CompilerTest.scala"), SourceItemKind.FILE,
        false)))
      assert(sources.contains(new SourceItem(getUri("mill_exercise/test/src/TestmainClass.scala"), SourceItemKind.FILE,
        false)))
    }

    test("testing that inverse sources are retrieved correctly") {
      val result1 = server.buildTargetInverseSources(new InverseSourcesParams(
              new TextDocumentIdentifier(getUri("mill_exercise/test/src/CompilerTest.scala")))).get
      assert(result1.getTargets.asScala.length == 1, "incorrect number of targets for this text doc")
      assert(result1.getTargets.asScala.head ==
        new BuildTargetIdentifier(getUri("mill_exercise/test/")), "incorrect target for this source")

      val result2 = server.buildTargetInverseSources(new InverseSourcesParams(
        new TextDocumentIdentifier(getUri("mill_exercise/src/Bill.scala")))).get
      assert(result2.getTargets.asScala.length == 1, "incorrect number of targets for this text doc")
      assert(result2.getTargets.asScala.head ==
        new BuildTargetIdentifier(getUri("mill_exercise/")), "incorrect target for this source")
    }

    test("testing resources are retrieved correctly") {
      val result1 = server.buildTargetResources(new ResourcesParams(List(mill_exercise).asJava)).get
      val resources = result1.getItems.asScala.head.getResources.asScala
      assert( result1.getItems.asScala.head.getTarget == mill_exercise, "incorrect target id in resources result")
      assert( resources.length == 2, "incorrect number of resources for mill_exercise")
      assert( resources.contains(getUri("mill_exercise/resources/res.txt")),
            "some resources were not retrieved.")
      assert( resources.contains(getUri("mill_exercise/resources/res2.txt")),
        "some resources were not retrieved.")

      val result2 = server.buildTargetResources(new ResourcesParams(List(test).asJava)).get
      assert( result2.getItems.asScala.head.getTarget == test, "incorrect target id in resources result")
      assert( result2.getItems.asScala.head.getResources.asScala.isEmpty, "mill_exercise.test doesn't have any resources")
    }

    //TODO: Think of a good way to test if all correct dependencies are computed
    test("testing that dependencies are retrieved correctly") {
      val result1 = server.buildTargetDependencySources(new DependencySourcesParams(List(mill_exercise).asJava))

      val result2 = server.buildTargetDependencySources(new DependencySourcesParams(List(test).asJava))
    }

    //TODO: see exactly how characters in a file are counted
    private[this] def assertPublishDiagnostics(targetId: BuildTargetIdentifier,
                                                expSeverity: DiagnosticSeverity,
                                               expSourceFile: String, expStartLine: Int,
                                               expEndLine: Int, expStartChar: Int,
                                               expEndChar: Int, expCode: String,
                                               expDiagnosticNum: Int,
                                               originId: String): Unit = {
      assert(client.diagnostics.filter(p => p.getBuildTarget == targetId && p.getOriginId == originId).
                        exists(p => p.getTextDocument.getUri == expSourceFile &&
                                    p.getDiagnostics.asScala.
                                      exists(d => d.getCode == expCode &&
                                                  d.getSeverity == expSeverity &&
                                                  d.getRange.getStart.getLine == expStartLine &&
                                                  d.getRange.getStart.getCharacter == expStartChar &&
                                                  d.getRange.getEnd.getLine == expEndLine &&
                                                  d.getRange.getEnd.getCharacter == expEndChar)),
       "diagnostic was not recorded by the client")
      assert(client.diagnostics.count(d => d.getBuildTarget == targetId) == expDiagnosticNum, "incorrect number of diagnostics")
    }

    test("testing compilation - compiling error") {
      val compileParams = new CompileParams(List(mill_exercise).asJava)
      compileParams.setOriginId("1000")
      val result = server.buildTargetCompile(compileParams).get
      assertPublishDiagnostics(mill_exercise, DiagnosticSeverity.ERROR,
                              getUri("mill_exercise/src/AddFile.scala"),
                              6, 6, 85, 85, "    true", 2, "1000")
      assert(result.getOriginId == "1000", "originId was not set correctly")
      assert(result.getStatusCode == StatusCode.ERROR, "incorrect status code")
    }

    test("testing compilation with no `unused` compilation argument") {
      val compileParams = new CompileParams(List(random).asJava)
      compileParams.setOriginId("10")
      val result = server.buildTargetCompile(compileParams).get
      assertPublishDiagnostics(random, DiagnosticSeverity.INFORMATION,
        getUri("random/"),
        0, 0, 0, 0, "", 1, "10"
      )
      assert(result.getOriginId == "10", "originId was not set correctly")
      assert(result.getStatusCode == StatusCode.OK, "incorrect status code")
    }

    test("testing compilation - compiling warning and info - with given `unused` argument") {
      val compileParams = new CompileParams(List(random).asJava)
      compileParams.setOriginId("10")
      compileParams.setArguments(List("-Ywarn-unused").asJava)
      val result = server.buildTargetCompile(compileParams).get
      println(client.diagnostics)
      assertPublishDiagnostics(random, DiagnosticSeverity.WARNING,
        getUri("random/src/main/scala/RandomClass.scala"),
        27, 27, 297, 297, "    case value: B => None", 3, "10")
      assertPublishDiagnostics(random, DiagnosticSeverity.WARNING,
        getUri("random/src/main/scala/RandomClass.scala"),
        1, 1, 0, 7, "import ch.epfl.scala.bsp4j._", 3, "10")
      assertPublishDiagnostics(random, DiagnosticSeverity.INFORMATION,
        getUri("random/"),
        0, 0, 0, 0, "", 3, "10"
        )
      assert(result.getOriginId == "10", "originId was not set correctly")
      assert(result.getStatusCode == StatusCode.OK, "incorrect status code")
    }

    test("testing compilation - no diagnostics") {

    }



}
