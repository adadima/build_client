package tests

import java.io.{File, PrintWriter}
import java.util.concurrent.ExecutionException
import build_client.TestBuildClient
import ch.epfl.scala.bsp4j._
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.scalatest.{BeforeAndAfterEach, FunSuite}
import build_client.BspServer
import com.google.gson.JsonObject

import scala.collection.JavaConverters._

trait MillBuildServerTest extends FunSuite with BeforeAndAfterEach {
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
  def path : String
  def serverStartCommand : Array[String]
  var server: BuildServer with ScalaBuildServer = _

  override def afterEach: Unit = {
    server.buildShutdown()
    server.onBuildExit()
  }

  override def beforeEach: Unit = {
    client = new TestBuildClient
    server = establishServerConnection(serverStartCommand).asInstanceOf[BuildServer with ScalaBuildServer]
    initializeServer(server)
    println("After initialize")
    server.onBuildInitialized()
    server.buildTargetCleanCache(new CleanCacheParams(
      server.workspaceBuildTargets.get.getTargets.asScala.map(t => t.getId).asJava)
    ).get
  }

  def establishServerConnection(startServerCommand: Array[String]): BuildServer = {
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

  def getUri(relativePath: String): String = {
    (os.Path(path) / os.RelPath(relativePath)).toIO.toURI.toString
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

  def assertTarget(target: BuildTarget, relPath: String,
                    name: String, capabilties: BuildTargetCapabilities,
                    langIds: Seq[String], deps: Seq[BuildTargetIdentifier],
                    dataKind: String
                  ): Unit = {
    assert(target.getBaseDirectory == getUri(relPath), "incorrect base directory")
    assert(target.getDisplayName == name, "incorrect display name")
    assert(target.getCapabilities == capabilties, "incorrect capabilities for this target")
    assert(target.getLanguageIds.asScala == langIds, "does not support both languages scala and java")
    assert(target.getDependencies.asScala == deps, s"$name does not have any module dependencies")
    assert(target.getDataKind == dataKind, "data kind for this target should be scala")
  }

  def assertDataField(target: JsonObject, version: String,
                      org: String, binaryVersion: String,
                      platform: Int, numJars: Int
                     ): Unit = {
    val scalaVersion = target.get("scalaVersion").getAsString
    assert(target.get("scalaOrganization").getAsString == org, "incorrect scala organization")
    assert(scalaVersion == version, "incorrect scala version")
    assert(target.get("scalaBinaryVersion").getAsString == binaryVersion, "incorrect scala binary version")
    assert(target.get("platform").getAsInt == platform, "wrong platform type")
    assert(target.get("jars").getAsJsonArray.size() == numJars, "wrong number fo scala jars")
    for (jar <- target.get("jars").getAsJsonArray.iterator().asScala) {
      assert(jar.getAsString.contains("scala-library-" + scalaVersion + ".jar") ||
        jar.getAsString.contains("scala-reflect-" + scalaVersion + ".jar") ||
        jar.getAsString.contains("scala-compiler-" + scalaVersion + ".jar"))
    }
  }

  def assertDefaultDataField(target: JsonObject, numJars: Int): Unit = {
    assertDataField(target, "2.12.8", "org.scala-lang", "2.12", 1, numJars)
  }

  def assertDefaultTarget(target: BuildTarget, relPath: String,
                          name: String, capabilities: BuildTargetCapabilities,
                          deps: Seq[BuildTargetIdentifier], numJars: Int): Unit = {
    assertTarget(target, relPath, name, capabilities, Seq("scala", "java"), deps, "scala")
    assertDefaultDataField(target.getData.asInstanceOf[JsonObject], numJars)
  }

  def assertInitialize(server: BuildServer with ScalaBuildServer): Unit = {
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

  def getServerTargetIds(server: BuildServer with ScalaBuildServer): List[BuildTargetIdentifier] = {
    server.workspaceBuildTargets().get.getTargets.asScala.map(target => target.getId).toList
  }

  def getTargetWithName(name: String): Option[BuildTarget] = {
    server.workspaceBuildTargets.get.getTargets.asScala.find(target => target.getDisplayName == name)
  }

  def assertSources(targets: Seq[BuildTargetIdentifier], sources: Seq[String]): Unit = {
    val sourcesResult = server.buildTargetSources(
      new SourcesParams(getServerTargetIds(server).asJava)).get
    val sourceItems = sourcesResult.getItems.asScala.flatMap(item => item.getSources.asScala)
    for (source <- sources) {
      assert(sourceItems.contains(new SourceItem(getUri(source), SourceItemKind.DIRECTORY, false)),
      "not all sources have been retrieved")
    }
  }

  def assertInverseSources(relPath: String, targetIds: Set[BuildTargetIdentifier]): Unit = {
    val result1 = server.buildTargetInverseSources(new InverseSourcesParams(
      new TextDocumentIdentifier(getUri(relPath)))).get
    assert(result1.getTargets.asScala.toSet == targetIds, "incorrect targets for this source")
  }

  def assertResources(targetId: BuildTargetIdentifier, resources: Set[String]): Unit = {
    val result1 = server.buildTargetResources(new ResourcesParams(List(targetId).asJava)).get
    val resourcesResult = result1.getItems.asScala.head.getResources.asScala.toSet
    assert(resourcesResult == resources.map(res => getUri(res)), "resources were not computed correctly")
  }

  //TODO: see exactly how characters in a file are counted
  def assertPublishDiagnostics(targetId: BuildTargetIdentifier,
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

  def assertProgressNotifications(taskName: String): Unit = {
    assert(client.taskProgresses.nonEmpty, "no progress notifications were sent.")
    assert(client.taskProgresses.exists(
      notification =>notification.getUnit == taskName
    ), "no notification for the compile task was sent.")
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
    assertThrows[ExecutionException](server.buildTargetInverseSources(new InverseSourcesParams(
      new TextDocumentIdentifier(""))).get
    )
    assertThrows[ExecutionException](server.buildTargetRun(new RunParams(new BuildTargetIdentifier(""))).get)
    assertThrows[ExecutionException](server.buildTargetTest(new TestParams(List().asJava)).get)
    assertInitialize(server)
    server.onBuildInitialized()
    server.buildShutdown()
    server.onBuildExit()
  }

}
