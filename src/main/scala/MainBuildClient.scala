import java.io.{File, FileWriter, InputStream, OutputStream, PrintWriter}
import java.util
import java.util.Collections
import java.util.concurrent.CompletableFuture

import build_client.TestBuildClient
import ch.epfl.scala.bsp4j._
import org.eclipse.lsp4j.jsonrpc.Launcher
import play.api.libs.json._
import scopt.OParser

import scala.collection.JavaConverters._
import scala.io.Source

trait BspServer extends BuildServer with ScalaBuildServer

case class ServerInformation(serverName: String,
                             serverVersion: String,
                             serverBspVersion: String,
                             serverLanguages: Seq[String],
                             serverStartCommand: List[String])


case class CLIConfig(projectDir: File,
                     logFile: Option[File])

/**
  * Main class for running a build client. Command line arguments
  * are used to interact with the TestBuildClient. Example commands:
  *
  * - initialize
  * - initialized
  * - shutdown
  * - exit
  * - compile
  * - test
  * - run
  * - clan-cache
  * - target-source
  *
  */
object MainBuildClient extends App {

  val config = parseArgs()
  var uniqueTargetid = 0
  val build_client = new TestBuildClient

  val file = new File(config.projectDir, ".bsp").listFiles().find(f => f.getName.endsWith(".json"))
    .map(f => {
      println(s"Picked config file ${f.getAbsolutePath}")
      Source.fromFile(f)
    }).getOrElse(throw new RuntimeException("Could not find any *.json file"))
  var canCompile = false
  var canRun = false
  var canTest = false

  val jsonResult = file.mkString("")
  file.close()
  val serverInfo = getServerInformation(jsonResult)
  println("Command: " + serverInfo.serverStartCommand)
  val process = new ProcessBuilder()
    .command(serverInfo.serverStartCommand.asJava)
    .directory(config.projectDir)
    .start()
  println("Started process")
  val inStream = process.getInputStream
  val outStream = process.getOutputStream
  val errStream = process.getErrorStream
  println("Got streams")
  //println(Source.fromInputStream(errStream).getLines().mkString("\n"))
  //println("Read from streams")
  val buildServer = generateBuildServer(build_client, inStream, outStream)
  val serverName = serverInfo.serverName
  val serverVersion = serverInfo.serverVersion
  val serverBspVersion = serverInfo.serverBspVersion
  val serverLanguages = serverInfo.serverLanguages

  println(process.isAlive)
  println(buildServer.getClass)
  var allTargets: List[BuildTarget] = _
  var canCompileTargets = Seq.empty[BuildTarget]
  var canRunTargets = Seq.empty[BuildTarget]
  var canTestTargets = Seq.empty[BuildTarget]


  println(Console.WHITE + "Started process and got streams")
  println(allTargets)
  while ( true ) {

    val request = scala.io.StdIn.readLine()

    try {
      execCommand(request)
    } catch {
      case exp: Error => println(Console.RED + "An exception/error occurred: " + exp.getMessage + " " + exp.getStackTrace.toString)
    }
  }


  def parseArgs(): CLIConfig = {
    val cliBuilder = OParser.builder[CLIConfig]
    val cliArgParser = {
      import cliBuilder._
      OParser.sequence(
        programName("BSP Test Build Client"),
        opt[File]('p', "project-path")
          .required()
          .action((x, c) => c.copy(projectDir = x))
          .text("Project working directory"),
        opt[File]('l', "logfile")
          .optional()
          .action((x, c) => c.copy(logFile = Some(x)))
          .text("Bsp trace log file")
      )
    }

    OParser.parse(cliArgParser, args, CLIConfig(new File("."), None)) match {
      case Some(x) => x
      case None =>
        System.exit(1)
        null
    }
  }

  def execCommand(request: String): Unit = request match {
    case "initialize" =>
      val result = initialize().get()
      setServerCapabilities(result)
      println(allTargets)
      allTargets = buildServer.workspaceBuildTargets().get().getTargets.asScala.toList
      println(allTargets)
      filterTargets(allTargets)
      assertInitializeResult(result)
      println(Console.WHITE + "Server initialization OK")

    case "initialized" =>
      buildServer.onBuildInitialized()
      println(Console.WHITE + "Server initialized OK")

    case "build-targets" =>
      for (target <- allTargets) {
        println(Console.WHITE + "Target: " + target + " \n")
      }

    case "compile" =>

      if (canCompile) {
        for (compileTarget <- canCompileTargets) {
          assertCompileResult(compile(compileTarget).get())
        }
        println(Console.WHITE + "Compilation OK")
      } else println("Your server does not have compile capabilities")

    case "run" =>
      if (canRun) {
        println(canRunTargets)
        for (runTarget <- canRunTargets) {
          println("RunResult: " + run(runTarget).get())
          assertRunResult(run(runTarget).get())
        }
        println(Console.WHITE + "Running OK")
      } else println(Console.WHITE + "Your server does not have run capabilities")

    case "test" =>
      if (canTest) {
        for (testTarget <- canTestTargets) {
          println("TestResult: " + test(testTarget).get())
          assertTestResult(test(testTarget).get())
        }
        println(Console.WHITE + "Testing OK")
      } else println(Console.WHITE + "Your server does not have test capabilities")

    case "scala-main" =>
      val params = new ScalaMainClassesParams(allTargets.map(target => target.getId).asJava)
      println(buildServer.asInstanceOf[BuildServer with ScalaBuildServer].buildTargetScalaMainClasses(params).get)
    case "scala-test" =>
      val params = new ScalaTestClassesParams(allTargets.map(target => target.getId).asJava)
      println(buildServer.asInstanceOf[BuildServer with ScalaBuildServer].buildTargetScalaTestClasses(params).get)
    case "clean-cache" =>
      for (cleanTarget <- allTargets) {
        assertCleanCacheResult(cleanCache(cleanTarget).get)
      }
      println(Console.WHITE + "Clean cache OK")

    case "dependencies" =>
      for (target <- allTargets) {
        val depend = assertDependencies(dependencies(target).get(), target.getId)
        println("Fetching dependencies: " + depend.mkString("\n") + " OK")
      }

    case "sources" =>
      println(buildServer.buildTargetSources(new SourcesParams(allTargets.map(target => target.getId).asJava)).get)

    case "inverse-sources" =>
      println(buildServer.buildTargetInverseSources(new InverseSourcesParams(new TextDocumentIdentifier(
        "file:///home/alexandra/mill/scratch/foo/src/FooMain.scala"
      ))).get)

    case "scalac-options" =>
      println(buildServer.asInstanceOf[BuildServer with ScalaBuildServer].buildTargetScalacOptions(
        new ScalacOptionsParams(allTargets.map(target => target.getId).asJava)
      ).get)

    case "target-source" =>
      assertTargetSourceRelation()
      println(Console.WHITE + "Targets - Source correlation OK")

    case "shutdown" =>
      buildServer.buildShutdown()
      assert(process.isAlive, message = "The server should not die before sending the response to the shutdown request")
      println(Console.WHITE + "Server shutdown OK - the server did not stop yet, needs to wait for exit notification")

    case "exit" =>
      buildServer.onBuildExit()
      Thread.sleep(1000)
      assert(!process.isAlive, message = "The server should stop after receiving the exit notification from the client")
      println(Console.WHITE + "Server exit OK - the server stopped")

    case _ => println(Console.WHITE + "Not a valid command, try again")

  }

  // return a tuple of the server display name, server version, bsp version and language capabilities
  def getServerInformation(jsonString: String): ServerInformation = {

    val jsValConnection = Json.parse(jsonString)
    val displayname = (jsValConnection \\ "name").head.as[String]
    val version = (jsValConnection \\ "version").head.as[String]
    val bspVersion = (jsValConnection \\ "bspVersion").head.as[String]
    val languages = (jsValConnection \\ "languages").map(jsVal => jsVal.as[List[String]]).head
    val command = (jsValConnection \\ "argv").map(jsVal => jsVal.as[List[String]]).head
    ServerInformation(displayname, version, bspVersion, languages, command)

  }

  def generateBuildServer(client: BuildClient, inS: InputStream, outS: OutputStream): BuildServer = {

    val launcher = new Launcher.Builder[BspServer]()
      .setRemoteInterface(classOf[BspServer])
      .setInput(inS)
      .setOutput(outS)
      .setLocalService(client)
      .traceMessages(config.logFile.map(f => new PrintWriter(new FileWriter(f, true))).orNull)
      .create()
    launcher.startListening()
    val bspServer = launcher.getRemoteProxy
    client.onConnectWithServer(bspServer)

    bspServer
  }

  def initialize(): CompletableFuture[InitializeBuildResult] = {
    val supportedLanguages = new util.ArrayList[String] ()
    supportedLanguages.add("scala")
    supportedLanguages.add("java")

    val params = new InitializeBuildParams("test_client",
      "0.0.1",
      "2.0",
      "file:/home/alexandra/build_client/",
      new BuildClientCapabilities(supportedLanguages))

    buildServer.buildInitialize(params)
  }

  def setServerCapabilities(result: InitializeBuildResult): Unit = {
    try {
      result.getCapabilities.getCompileProvider.getLanguageIds
      canCompile = true
    } catch {
      case _: Exception => println(Console.WHITE + "Your server does not have compile capabilities")
    }

    try {
      result.getCapabilities.getTestProvider.getLanguageIds
      canTest = true
    } catch {
      case _: Exception => println(Console.WHITE + "Your server does not have test capabilities")
    }

    try {
      result.getCapabilities.getRunProvider.getLanguageIds
      canRun = true
    } catch {
      case _: Exception => println(Console.WHITE + "Your server does not have run capabilities")
    }
  }

  def filterTargets(targets: List[BuildTarget]): Unit = {
    canCompileTargets = allTargets.filter( target => target.getCapabilities.getCanCompile)
    canRunTargets = allTargets.filter( target => target.getCapabilities.getCanRun)
    canTestTargets = allTargets.filter( target => target.getCapabilities.getCanTest)
  }

  def assertInitializeResult(result: InitializeBuildResult): Unit = {
    assert (result.getDisplayName == serverName,
      message = "The display name was not transmitted correctly: ")
    assert (result.getBspVersion.substring(0, 3) == serverBspVersion.substring(0, 3),
      message = "The bsp version was not transmitted correctly: " + result.getBspVersion + s" but should be $serverBspVersion")
    assert (result.getVersion == serverVersion,
      message = "The server version was not transmitted correctly: " + result.getVersion + s" but should be $serverVersion" )

    if ( canCompile ) {
      assert(result.getCapabilities.getCompileProvider.getLanguageIds.asScala.toSet == serverLanguages.toSet,
        message = "The supported languages for compilation  are not as in the connection file")
    }

    if ( canRun ) {
      assert (result.getCapabilities.getRunProvider.getLanguageIds.asScala.toSet == serverLanguages.toSet,
        message = "The supported languages for testing are not as in the connection file")
    }

    if ( canTest ) {
      assert (result.getCapabilities.getTestProvider.getLanguageIds.asScala.toSet == serverLanguages.toSet,
        message = "The supported languages for running are not as in the connection file")
    }
  }

  def compile(compileTarget: BuildTarget): CompletableFuture[CompileResult] = {

    val compileParams = new CompileParams(Collections.singletonList(compileTarget.getId))
    compileParams.setOriginId(uniqueTargetid.toString)
    buildServer.buildTargetCompile(compileParams)
  }

  def assertCompileResult(result: CompileResult): Unit = {
    //    assert(result.getOriginId == uniqueTargetid.toString,
    //      message = "The origin id assigned by the client was not transmitted back correctly, got: " + result.getOriginId +
    //                "but expected : " + uniqueTargetid.toString)
    println(result)
    println("Task starts: ", build_client.taskStarts)
    println("Task finishes: ", build_client.taskFinishes)
    println("Compile report: ", build_client.compileReports)
    println("Compile diagnostics: ", build_client.diagnostics)
    uniqueTargetid = uniqueTargetid + 1
  }

  def run(runTarget: BuildTarget): CompletableFuture[RunResult] = {
    val runParams = new RunParams(runTarget.getId)
    runParams.setOriginId(uniqueTargetid.toString)
    runParams.setArguments(List("help").asJava)
    buildServer.buildTargetRun(runParams)
  }

  def assertRunResult(result: RunResult): Unit = {
    assert(result.getOriginId == uniqueTargetid.toString,
      message = "The origin id assigned by the client was not transmitted back correctly, got: " + result.getOriginId +
        "but expected : " + uniqueTargetid.toString)
    uniqueTargetid = uniqueTargetid + 1
  }

  def test(testTarget: BuildTarget): CompletableFuture[TestResult] = {
    val testParams = new TestParams(Collections.singletonList(testTarget.getId))
    testParams.setOriginId(uniqueTargetid.toString)
    buildServer.buildTargetTest(testParams)
  }

  def assertTestResult(result: TestResult): Unit = {
    assert(result.getOriginId == uniqueTargetid.toString,
      message = "The origin id assigned by the client was not transmitted back correctly, got: " + result.getOriginId +
        "but expected : " + uniqueTargetid.toString)
    uniqueTargetid = uniqueTargetid + 1
    println("Task Starts: " + build_client.taskStarts)
    println("Task Finished: " + build_client.taskFinishes)
  }

  def cleanCache(cleanTarget: BuildTarget): CompletableFuture[CleanCacheResult] = {
    val cleanParams = new CleanCacheParams(Collections.singletonList(cleanTarget.getId))
    buildServer.buildTargetCleanCache(cleanParams)
  }

  def assertCleanCacheResult(result: CleanCacheResult): Unit = {
    println("message: " + result.getMessage)
    assert( result.getCleaned, message = "The server reported that the cache was not cleaned")
  }

  def dependencies(target: BuildTarget): CompletableFuture[DependencySourcesResult] = {
    val dependencyParams = new DependencySourcesParams(Collections.singletonList(target.getId))
    buildServer.buildTargetDependencySources(dependencyParams)
  }

  def assertDependencies(result: DependencySourcesResult, id: BuildTargetIdentifier): List[String] = {
    assert(result.getItems.get(0).getTarget == id, message = "The target id from the response is incorrect")
    result.getItems.get(0).getSources.asScala.toList
  }

  def getTargetSourcesList(target: BuildTarget): List[String] = {
    val result = buildServer.buildTargetSources(
      new SourcesParams(Collections.singletonList(target.getId))).get
    result.getItems.get(0).getSources.asScala.map(sourceItem => sourceItem.getUri).toList
  }

  def getTargetToSourceMap: Map[BuildTarget, List[String]] = {
    (for (target <- allTargets) yield (target, getTargetSourcesList(target))).toMap
  }

  def getAllSources: List[String] = {
    getTargetToSourceMap.values.reduceLeft((B, sourceList) => B ++ sourceList)
  }

  def getSourceTargetsList(source: String): List[BuildTargetIdentifier] = {
    val inverseSourcesParams = new InverseSourcesParams(new TextDocumentIdentifier(source))
    buildServer.buildTargetInverseSources(inverseSourcesParams).get().getTargets.asScala.toList
  }

  def getSourceToTargetMap(allSources: List[String]): Map[String, List[BuildTargetIdentifier]] = {
    (for (source <- allSources) yield (source, getSourceTargetsList(source))).toMap
  }

  def assertTargetSourceRelation(): Unit = {
    val targetToSource = getTargetToSourceMap
    val sourceToTarget = getSourceToTargetMap(getAllSources)

    for ( (target, sources) <- targetToSource ) {
      for ( source <- sources ) {
        assert( sourceToTarget(source).contains(target.getId),
          message = "Target is associated with a source, but the source is not associated with the target ( or the other way )")
      }
    }
  }
}
