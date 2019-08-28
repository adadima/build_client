package tests

import ch.epfl.scala.bsp4j._
import com.google.gson.JsonObject
import scala.collection.JavaConverters._
import scala.io.Source

class TestMillExercise extends MillBuildServerTest {

    override def path = "/Users/adadima/mill_exercise/"
    val mill_exercise = new BuildTargetIdentifier(getUri("mill_exercise/"))
    val test = new BuildTargetIdentifier(getUri("mill_exercise/mill_exercise.test"))
    val random = new BuildTargetIdentifier(getUri("random/"))
    val foo = new BuildTargetIdentifier(getUri("foo/"))
    val invalidTarget = new BuildTargetIdentifier(getUri("build"))
    val random_test = new BuildTargetIdentifier(getUri("random/random.test"))
    val foo_test = new BuildTargetIdentifier(getUri("foo/foo.test"))

    test("verifying that build targets are constructed properly") {
      val buildTargets = server.workspaceBuildTargets().get.getTargets.asScala
      assert(buildTargets.length == 7, "Incorrect number of targets")
      val mill_exercise = buildTargets.filter(t => t.getDisplayName == "mill_exercise").head
      val test = buildTargets.filter(t => t.getDisplayName == "mill_exercise.test").head
      assertDefaultTarget(
        getTargetWithName("mill_exercise").get,
        "mill_exercise/",
        "mill_exercise",
        new BuildTargetCapabilities(true, false, true),
        Seq.empty[BuildTargetIdentifier], 3)
      assertDefaultTarget(
        getTargetWithName("mill_exercise.test").get,
        "mill_exercise/test",
        "mill_exercise.test",
        new BuildTargetCapabilities(true, true, true),
        Seq(mill_exercise.getId), 3
      )
      assertDefaultTarget(
        getTargetWithName("random").get,
        "random/",
        "random",
        new BuildTargetCapabilities(true, false, true),
        Seq.empty[BuildTargetIdentifier], 1
      )
      assertDefaultTarget(
        getTargetWithName("random.test").get,
        "random/src/test/",
        "random.test",
        new BuildTargetCapabilities(true, true, true),
        Seq(random), 2
      )
      assertDefaultTarget(
        getTargetWithName("foo").get,
        "foo/",
        "foo",
        new BuildTargetCapabilities(true, false, true),
        Seq.empty[BuildTargetIdentifier], 1
      )
      assertDefaultTarget(
        getTargetWithName("foo.test").get,
        "foo/test/",
        "foo.test",
        new BuildTargetCapabilities(true, true, true),
        Seq(foo), 2
      )
    }

    test("testing sources are retrieved correctly") {
      val sources = Seq("mill_exercise/src/", "mill_exercise/test/src/", "random/src/main/scala", "foo/src/")
      assertSources(Seq(mill_exercise, test, random, foo), sources)
    }

    test("testing that inverse sources are retrieved correctly") {
      assertInverseSources("mill_exercise/test/src/CompilerTest.scala", Set(test))
      assertInverseSources("mill_exercise/src/Bill.scala", Set(mill_exercise))
      assertInverseSources("random/src/main/scala/RandomClass.scala", Set(random))
      assertInverseSources("random/src/test/scala/RandomTestClass.scala", Set(random_test))
      assertInverseSources("foo/src/EmptySourceClass.scala", Set(foo))
      assertInverseSources("foo/test/src/EmptyTestClass.scala", Set(foo_test))
    }

    test("testing resources are retrieved correctly") {
      assertResources(mill_exercise, Set("mill_exercise/resources/res.txt", "mill_exercise/resources/res2.txt"))
      assertResources(foo, Set())
    }

    //TODO: Think of a good way to test if all correct dependencies are computed
    test("testing that dependencies are retrieved correctly") {
      val result1 = server.buildTargetDependencySources(new DependencySourcesParams(List(mill_exercise).asJava))

      val result2 = server.buildTargetDependencySources(new DependencySourcesParams(List(test).asJava))
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
          getUri("random/src/main/scala/RandomClass.scala"), 1, "10")
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
      assert(expectedClassesArgs == actualClassesArgs, "Main classes and fork arguments were not computed " +
        "correctly.")
      assert(result.getItems.asScala.filter(item => item.getTarget == foo).head.getClasses.isEmpty,
      "module foo has no main classes")
    }

    test("Testing that the cleanCache response is correct if cache was/was not cleaned") {
      val result1 = server.buildTargetCleanCache(new CleanCacheParams(List(invalidTarget).asJava)).get
      assert(!result1.getCleaned, "the invalid target appears to have been cleaned - this is not" +
        "k since the target corresponds to a module that doesn't exist")
      assert(result1.getMessage.contains("Target build could not be cleaned"),
      "the error message was not set correctly")

      val result2 = server.buildTargetCleanCache(new CleanCacheParams(List(foo).asJava)).get
      assert(result2.getCleaned, "the server reported that foo was not cleaned.")
      assert(result2.getMessage.contains("foo cleaned"), "the success massage was not displayed correctly")
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
      assert(client.taskStarts.count(not => not.getDataKind == TaskDataKind.TEST_START) == 11,
        "wrong number of task start notifications")
      assert(client.taskFinishes.count(not => not.getDataKind == TaskDataKind.TEST_FINISH) == 11,
        "wrong number of task finish notifications")
      val testReport = client.taskFinishes.filter(finish => finish.getDataKind == "test-report").head.getData.
        asInstanceOf[JsonObject]
      assertTestReport(testReport, random_test, 2, 6, 3, 0, 0)
      assert(result.getStatusCode == StatusCode.ERROR, "the server reported the execution was error.")
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
      assert(client.taskStarts.count(not => not.getDataKind == TaskDataKind.TEST_START) == 2,
        "wrong number of task start notifications")
      assert(client.taskFinishes.count(not => not.getDataKind == TaskDataKind.TEST_FINISH) == 2,
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
      assert(client.taskStarts.count(not => not.getDataKind == TaskDataKind.TEST_START) == 4,
        "wrong number of task start notifications")
      assert(client.taskFinishes.count(not => not.getDataKind == TaskDataKind.TEST_FINISH) == 4,
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

    test("Testing that scalac options are computed correctly - no scalac options given.") {
      val result = server.buildTargetScalacOptions(new ScalacOptionsParams(List(foo_test, foo).asJava)).get
      assert(result.getItems.asScala.length == 2, "wrong number of items in the response")
      assert(result.getItems.asScala.forall(item => item.getOptions.asScala.isEmpty),
            "scalac options were reported but none were given in the build file")
      assert(result.getItems.asScala.map(item => item.getClassDirectory).toSet ==
        Set(getUri("out/foo/test/compile/dest/classes/"),
            getUri("out/foo/compile/dest/classes/")))
    }

    test("Testing that scalac options are computed correctly - scalac options given.") {
      val result = server.buildTargetScalacOptions(new ScalacOptionsParams(List(random).asJava)).get
      assert(result.getItems.asScala.length == 1, "wrong number of items in the response")
      assert(result.getItems.asScala.head.getOptions.asScala.toSet == Set("-Ylog-classpath"),
            "scalac options were not computed correctly")
      assert(result.getItems.asScala.head.getClassDirectory == getUri(
              "out/random/compile/dest/classes/"),
        "wrong class directory"
      )
      val runCommand = Array("java", "-classpath", result.getItems.asScala.head.getClasspath.asScala.mkString(":"),
                              "RandomClass", "MILL", "BSP")
      val process = Runtime.getRuntime.exec(runCommand)
      process.waitFor
      assert(process.exitValue() == 0, "wrong exit value for running the module with the scala classpath")
    }
}
