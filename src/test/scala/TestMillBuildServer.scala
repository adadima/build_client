import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.Path
import java.util.concurrent.ExecutionException

import MainBuildClient.config
import build_client.TestBuildClient
import ch.epfl.scala.bsp4j.{BuildClientCapabilities, BuildServer, BuildServerCapabilities, BuildTarget, BuildTargetCapabilities, BuildTargetIdentifier, CleanCacheParams, CompileParams, DependencySourcesParams, DiagnosticSeverity, InitializeBuildParams, InitializeBuildResult, InverseSourcesParams, ResourcesParams, RunParams, ScalaBuildServer, ScalaBuildTarget, ScalaMainClassesParams, ScalaPlatform, ScalaTestClassesItem, ScalaTestClassesParams, ScalaTestParams, SourceItem, SourceItemKind, SourcesParams, StatusCode, TestParams, TextDocumentIdentifier}
import com.google.gson.JsonObject
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.scalatest.{BeforeAndAfterEach, BeforeAndAfterEachTestData, FunSuite, TestData}

import collection.JavaConverters._
import scala.io.Source

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
    var client: TestBuildClient = _
    val path = "/home/alexandra/mill_exercise/"
    //val serverStartCommand = Array("../out/dev/launcher/dest/run", "-i", "mill.contrib.BSP/start")
    val serverStartCommand = Array(
      "java",
      "-DMILL_CLASSPATH=/home/alexandra/mill-release",
      "-DMILL_VERSION=0.5.0-55-91a48a-DIRTY99ca76d6",
      "-Djna.nosys=true",
      "-cp",
      "/home/alexandra/mill-release",
      "mill.MillMain",
      "mill.contrib.BSP/start")
    val mill_exercise = new BuildTargetIdentifier(getUri("mill_exercise/"))
    val test = new BuildTargetIdentifier(getUri("mill_exercise/mill_exercise.test"))
    val random = new BuildTargetIdentifier(getUri("random/"))
    val foo = new BuildTargetIdentifier(getUri("foo/"))
    val invalidTarget = new BuildTargetIdentifier(getUri("build"))
    val random_test = new BuildTargetIdentifier(getUri("random/random.test"))
    val foo_test = new BuildTargetIdentifier(getUri("foo/foo.test"))
    var server: BuildServer with ScalaBuildServer = _

    override def afterEach: Unit = {
      server.buildShutdown()
      server.onBuildExit()
    }

    override def beforeEach: Unit = {
      client = new TestBuildClient
      server = establishServerConnection(serverStartCommand).asInstanceOf[BuildServer with ScalaBuildServer]
      initializeServer(server)
      server.onBuildInitialized()
      server.buildTargetCleanCache(new CleanCacheParams(List(mill_exercise, test, random, foo, random_test).asJava)).get
    }

    private[this] def establishServerConnection(startServerCommand: Array[String]): BuildServer = {
      val process2 = Runtime.getRuntime.exec(startServerCommand, null, new File(path))
      val inStream = process2.getInputStream
      val outStream = process2.getOutputStream

      val launcher = new Launcher.Builder[BspServer]()
        .setRemoteInterface(classOf[BspServer])
        .setInput(inStream)
        .setOutput(outStream)
        .setLocalService(client)
        .traceMessages(new PrintWriter((os.pwd / "bsp.log").toIO))
        .create()
      launcher.startListening()
      val bspServer = launcher.getRemoteProxy
      client.onConnectWithServer(bspServer)
      bspServer
    }

    private[this] def getUri(relativePath: String): String = {
      (os.Path(path) / os.RelPath(relativePath)).toIO.toURI.toString
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
      assert(target.getDisplayName == "mill_exercise.test", "incorrect display name")
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

    test("verifying that build targets are constructed properly in mill_exercise") {
      val buildTargets = server.workspaceBuildTargets().get.getTargets.asScala
      assert(buildTargets.length == 7, "Incorrect number of targets")
      val mill_exercise = buildTargets.filter(t => t.getDisplayName == "mill_exercise").head
      val test = buildTargets.filter(t => t.getDisplayName == "mill_exercise.test").head
      assertMillExercise(mill_exercise)
      assertScalaBuildTarget(mill_exercise.getData.asInstanceOf[JsonObject])
      assertTestTarget(test)
      assertScalaBuildTarget(test.getData.asInstanceOf[JsonObject])
    }

    private[this] def assertInitialize(server: BuildServer with ScalaBuildServer): Unit = {
      val initializeResult = initializeServer(server)
      assert(initializeResult.getBspVersion == "2.0.0", "wrong bsp version")
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
      val server = establishServerConnection(serverStartCommand).asInstanceOf[BuildServer with ScalaBuildServer]
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
      assert(sources.contains(new SourceItem(getUri("mill_exercise/src/"), SourceItemKind.DIRECTORY,
        false)))
      assert(sources.contains(new SourceItem(getUri("mill_exercise/test/src/"), SourceItemKind.DIRECTORY,
        false)))
      assert(sources.contains(new SourceItem(getUri("random/src/main/scala"), SourceItemKind.DIRECTORY,
        false)))
      assert(sources.contains(new SourceItem(getUri("foo/src/"), SourceItemKind.DIRECTORY,
        false)))
    }

    test("testing that inverse sources are retrieved correctly") {
      val result1 = server.buildTargetInverseSources(new InverseSourcesParams(
              new TextDocumentIdentifier(getUri("mill_exercise/test/src/CompilerTest.scala")))).get
      assert(result1.getTargets.asScala.length == 1, "incorrect number of targets for this text doc")
      assert(result1.getTargets.asScala.head == test, "incorrect target for this source")

      val result2 = server.buildTargetInverseSources(new InverseSourcesParams(
        new TextDocumentIdentifier(getUri("mill_exercise/src/Bill.scala")))).get
      assert(result2.getTargets.asScala.length == 1, "incorrect number of targets for this text doc")
      assert(result2.getTargets.asScala.head == mill_exercise, "incorrect target for this source")
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
                                               expSourceFile: String,
                                               expDiagnosticNum: Int,
                                               originId: String): Unit = {
      assert(client.diagnostics.filter(p => p.getBuildTarget == targetId && p.getOriginId == originId).
                        exists(p => p.getTextDocument.getUri == expSourceFile &&
                                    p.getDiagnostics.asScala.
                                      count(d => d.getSeverity == expSeverity) == expDiagnosticNum),
       "diagnostic was not recorded by the client")
    }

    private[this] def assertProgressNotifications(taskName: String): Unit = {
      assert(client.taskProgresses.nonEmpty, "no progress notifications were sent.")
      assert(client.taskProgresses.exists(
        notification =>notification.getUnit == taskName
      ), "no notification for the compile task was sent.")
    }

    test("testing compilation - compiling error") {
      val compileParams = new CompileParams(List(mill_exercise).asJava)
      compileParams.setOriginId("1000")
      val result = server.buildTargetCompile(compileParams).get
      assertProgressNotifications("mill_exercise.compile")

      assertPublishDiagnostics(mill_exercise, DiagnosticSeverity.ERROR,
                              getUri("mill_exercise/src/AddFile.scala"),
                              1, "1000")
      assertPublishDiagnostics(mill_exercise, DiagnosticSeverity.WARNING,
                              getUri("mill_exercise/src/Bill.scala"),
                              1, "1000")
      assert(result.getOriginId == "1000", "originId was not set correctly")
      assert(result.getStatusCode == StatusCode.ERROR, "incorrect status code")
    }

    test("testing compilation with no `unused` compilation argument") {
      val compileParams = new CompileParams(List(random).asJava)
      compileParams.setOriginId("10")
      val result = server.buildTargetCompile(compileParams).get
      assertProgressNotifications("random.compile")
      assertPublishDiagnostics(random, DiagnosticSeverity.INFORMATION,
        getUri("random/"), 1, "10")
      assert(result.getOriginId == "10", "originId was not set correctly")
      assert(result.getStatusCode == StatusCode.OK, "incorrect status code")
    }

    test("testing compilation - compiling warning and info - with given `unused` argument") {
      val compileParams = new CompileParams(List(random).asJava)
      compileParams.setOriginId("10")
      compileParams.setArguments(List("-Ywarn-unused").asJava)
      val result = server.buildTargetCompile(compileParams).get

      assertPublishDiagnostics(random, DiagnosticSeverity.WARNING,
          getUri("random/src/main/scala/RandomClass.scala"), 2, "10")
      assertPublishDiagnostics(random, DiagnosticSeverity.INFORMATION,
          getUri("random/"), 1, "10"
      )
      assertProgressNotifications("random.compile")
      assert(result.getOriginId == "10", "originId was not set correctly")
      assert(result.getStatusCode == StatusCode.OK, "incorrect status code")
    }

    test("testing compilation - no diagnostics") {
      val compileParams = new CompileParams(List(foo).asJava)
      compileParams.setOriginId("foo-id")
      compileParams.setArguments(List("-Ywarn-unused").asJava)
      val result = server.buildTargetCompile(compileParams).get
      assertProgressNotifications("foo.compile")
      assert(client.diagnostics.isEmpty, "diagnostics were sent when there was no reason to.")
      assert(result.getOriginId == "foo-id", "originId was not set correctly")
      assert(result.getStatusCode == StatusCode.OK, "incorrect status code")
    }

    test("testing that the scala main classes are computed correctly") {
      val params = new ScalaMainClassesParams(List(mill_exercise, test, random, foo).asJava)
      params.setOriginId("classes")
      val result = server.buildTargetScalaMainClasses(params).get
      assert(result.getItems.asScala.length == 4, "wrong number of scala main classes items.")
      val expectedClassesArgs = Map(
        random -> ("RandomClass", Seq("-DMILL_VERSION=0.5.0"))
      )
      val actualClassesArgs = ( for (item <- result.getItems.asScala.filter(i => i.getTarget != foo)
                                     if item.getClasses.asScala.nonEmpty)
        yield
        (
                    item.getTarget,
                    (item.getClasses.asScala.head.getClassName, item.getClasses.asScala.head.getJvmOptions.asScala)
        )).toMap
      assert(expectedClassesArgs == actualClassesArgs, "Main classes and fork arguments were not computed" +
        "correctly.")
      assert(result.getItems.asScala.filter(item => item.getTarget == foo).head.getClasses.isEmpty,
      "module foo has no main classes")
    }

    test("Testing that the cleanCache response is correct if cache was/was not cleaned") {
      val result1 = server.buildTargetCleanCache(new CleanCacheParams(List(invalidTarget).asJava)).get
      assert(!result1.getCleaned, "the invalid target appears to have been cleaned - this is not" +
        "k since the target corresponds to a module that doesn't exist")
      assert(result1.getMessage.contains("clean Cannot resolve build. Try `mill resolve _` to see what's available."),
      "the error message was not set correctly")

      val result2 = server.buildTargetCleanCache(new CleanCacheParams(List(foo).asJava)).get
      assert(result2.getCleaned, "the server reported that foo was not cleaned.")
      assert(result2.getMessage.contains("[1/1] clean"), "the success massage was not displayed correctly")
    }

    test("Testing the run request without parameters - status code OK") {
      val params = new RunParams(random)
      params.setOriginId("run")
      val result = server.buildTargetRun(params).get
      assert(result.getStatusCode == StatusCode.OK, "the server reported the execution was not successful")
      assert(os.exists(os.Path(path) / "random.txt"), "the main class was not ran.")
       assert(result.getOriginId == "run", "the server did not set the response origin id")
    }

    test("Testing the run request with parameters - status code OK") {
      val params = new RunParams(random)
      params.setOriginId("run2")
      params.setArguments(List("MILL", "BSP").asJava)
      val result = server.buildTargetRun(params).get
      assert(result.getStatusCode == StatusCode.OK, "the server reported the execution was not successful.")
      assert(os.exists(os.Path(path) / "random.txt"), "the main class was not ran.")
       assert(result.getOriginId == "run2", "the server did not set the response origin id")

      val file = Source.fromFile(path + "random.txt")
      val lines = file.getLines().toSeq
      assert(lines.length == 2, "the main class did not execute correctly")
      assert(lines.head == "MILL", "wrong execution")
      assert(lines(1) == "BSP", "wrong execution")
      file.close()
    }

    test("Testing the run request with parameters - status code ERROR") {
      val params = new RunParams(random)
      params.setOriginId("run3")
      params.setArguments(List("argument").asJava)
      val result = server.buildTargetRun(params).get
      assert(result.getStatusCode == StatusCode.ERROR, "the server reported the execution went ok.")
       assert(result.getOriginId == "run3", "the server did not set the response origin id")
    }

    test("Testing run request on a module with compile errors - status code ERROR.") {
      val params = new RunParams(test)
      params.setOriginId("run4")
      params.setArguments(List("argument").asJava)
      val result = server.buildTargetRun(params).get
      assert(result.getStatusCode == StatusCode.ERROR, "the server reported the execution was not error.")
      assert(result.getOriginId == "run4", "the server did not set the response origin id")
    }

    test("Testing test request on a module with compile errors - status code ERROR") {
      val params = new TestParams(List(test).asJava)
      params.setOriginId("test")
      val result = server.buildTargetTest(params).get
      assert(result.getStatusCode == StatusCode.ERROR, "the server reported the execution was not error.")
      assert(result.getOriginId == "test", "the server did not set the response origin id")
    }

    private[this] def assertTestReport(testReport: JsonObject,
                                       expTarget: BuildTargetIdentifier,
                                       expFailed: Int, expPassed: Int,
                                       expIgnored: Int, expCancelled: Int,
                                       expSkipped: Int): Unit = {
      assert(testReport.get("failed").getAsInt == expFailed, "incorrect number of failed tests")
      assert(testReport.get("passed").getAsInt == expPassed, "incorrect number of passed tests")
      assert(testReport.get("ignored").getAsInt == expIgnored, "incorrect number of ignored tests")
      assert(testReport.get("cancelled").getAsInt == expCancelled, "incorrect number of cancelled tests")
      assert(testReport.get("skipped").getAsInt == expSkipped, "incorrect number of skipped tests")
      assert(testReport.get("target").getAsJsonObject.get("uri").getAsString == expTarget.getUri,
        "incorrect target Id for this test report")
    }

    test("testing the test request with no arguments - passed, ignored and failed test cases.") {
      val params = new TestParams(List(random_test).asJava)
      params.setOriginId("random-test")
      val result = server.buildTargetTest(params).get
      assert(client.taskStarts.count(not => not.getDataKind == "test-started") == 11,
        "wrong number of task start notifications")
      assert(client.taskFinishes.count(not => not.getDataKind == "test-finished") == 11,
        "wrong number of task finish notifications")
      val testReport = client.taskFinishes.filter(finish => finish.getDataKind == "test-report").head.getData.
        asInstanceOf[JsonObject]
      assertTestReport(testReport, random_test, 2, 6, 3, 0, 0)
      assert(result.getStatusCode == StatusCode.ERROR, "the server reported the execution was not error.")
      assert(result.getOriginId == "random-test", "the server did not set the response origin id")
      assertProgressNotifications("random.test.testLocal")
    }

    test("Testing test request with arguments - only passed test cases.") {
      val params = new TestParams(List(random_test).asJava)
      params.setOriginId("random-test2")
      val scalaParams = new ScalaTestParams()
      scalaParams.setTestClasses(List(
        new ScalaTestClassesItem(
                                random_test,
                                List("-t", "test3").asJava
        )).asJava)
      params.setData(scalaParams)
      params.setDataKind("scala-test")
      val result = server.buildTargetTest(params).get
      assert(client.taskStarts.count(not => not.getDataKind == "test-started") == 2,
        "wrong number of task start notifications")
      assert(client.taskFinishes.count(not => not.getDataKind == "test-finished") == 2,
        "wrong number of task finish notifications")
      val testReport = client.taskFinishes.filter(finish => finish.getDataKind == "test-report").head.getData.
        asInstanceOf[JsonObject]
      assertTestReport(testReport, random_test, 0, 2, 0, 0, 0)
      assert(result.getStatusCode == StatusCode.OK, "the server reported the execution was not error.")
      assert(result.getOriginId == "random-test2", "the server did not set the response origin id")
      assertProgressNotifications("random.test.testLocal")
    }

    test("Testing test request with arguments - passed and ignored test cases.") {
      val params = new TestParams(List(random_test).asJava)
      params.setOriginId("random-test3")
      val scalaParams = new ScalaTestParams()
      scalaParams.setTestClasses(List(
        new ScalaTestClassesItem(
          random_test,
          List("-t", "test3",
              "-t", "test1").asJava
        )).asJava)
      params.setData(scalaParams)
      params.setDataKind("scala-test")
      val result = server.buildTargetTest(params).get
      assert(client.taskStarts.count(not => not.getDataKind == "test-started") == 4,
        "wrong number of task start notifications")
      assert(client.taskFinishes.count(not => not.getDataKind == "test-finished") == 4,
        "wrong number of task finish notifications")
      val testReport = client.taskFinishes.filter(finish => finish.getDataKind == "test-report").head.getData.
        asInstanceOf[JsonObject]
      assertTestReport(testReport, random_test, 0, 3, 1, 0, 0)
      assert(result.getStatusCode == StatusCode.OK, "the server reported the execution was not error.")
      assert(result.getOriginId == "random-test3", "the server did not set the response origin id")
      assertProgressNotifications("random.test.testLocal")
    }

    test("Testing that scala test classes are retrieved correctly - even with two test classes / file.") {
      val params = new ScalaTestClassesParams(List(random_test, foo_test).asJava)
      params.setOriginId("scala test classes")
      val result = server.buildTargetScalaTestClasses(params).get
      assert(result.getItems.asScala.length == 2, "did not find enough test classes.")
      val testClassMap = (for (item <- result.getItems.asScala) yield
        (item.getTarget, item.getClasses.asScala)).toMap
      assert(testClassMap(random_test).toSet == Seq("RandomTestClass", "RandomTestClass2", "RandomTest2Class").toSet,
            "test classes not computed correctly")
      assert(testClassMap(foo_test).toSet == Set("EmptyTestClass"),
            "test classes not computed correctly")
    }

    test("Testing that scala test classes can not be computed when there is compilation error.") {
      val params = new ScalaTestClassesParams(List(test).asJava)
      params.setOriginId("scala test classes")
      val result = server.buildTargetScalaTestClasses(params).get
      assert(result.getItems.asScala.length == 1, "wrong number of targets in the response.")
      assert(result.getItems.asScala.head.getClasses.asScala.isEmpty,
        "target does not have any identifiable test classes.")
    }
}
