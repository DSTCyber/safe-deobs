/**
 * *****************************************************************************
 * Copyright (c) 2016-2017, KAIST.
 * Copyright (c) 2017, DST Group.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.phase

import scala.util.{ Try, Success }
import kr.ac.kaist.safe.errors.ExcLog
import kr.ac.kaist.safe.SafeConfig
import kr.ac.kaist.safe.deobfuscator._
import kr.ac.kaist.safe.nodes.ast.Program
import kr.ac.kaist.safe.util.{ NodeUtil => NU }
import kr.ac.kaist.safe.util._

// Deobfuscate phase
case object Deobfuscate extends PhaseObj[Program, DeobfuscateConfig, Program] {
  val name: String = "deobfuscator"
  val help: String =
    "Deobfuscates JavaScript malware (string decoder, constant folder and propagator)"

  private def deobfuscate(program: Program, excLog: ExcLog): (Program, ExcLog) = {
    var newProgram = program
    var newExcLog = excLog

    // Decode strings
    println("run string decoder")
    val stringDecoder = new StringDecoder(newProgram)
    newProgram = stringDecoder.result
    newExcLog += stringDecoder.excLog

    // Fold constants
    println("run constant folder")
    val constantFolder = new ConstantFolder(newProgram)
    newProgram = constantFolder.result
    newExcLog += constantFolder.excLog

    // Propagate constants
    println("run constant propagator")
    val constantPropagator = new ConstantPropagator(newProgram)
    newProgram = constantPropagator.result
    newExcLog += constantPropagator.excLog

    // Inline functions
    println("run function inliner")
    val functionInliner = new FunctionInliner(newProgram)
    newProgram = functionInliner.result
    newExcLog += functionInliner.excLog

    // Remove dead branches
    println("run dead branch remover")
    val deadBranchRemover = new DeadBranchRemover(newProgram)
    newProgram = deadBranchRemover.result
    newExcLog += deadBranchRemover.excLog

    // Simplify
    println("run simplifier")
    newProgram = NU.SimplifyWalker.walk(newProgram)

    if (newExcLog.hasError) {
      (newProgram, newExcLog)
    } else if (newProgram =~ program) {
      (newProgram, newExcLog)
    } else {
      // XXX not yet working
      //deobfuscate(newProgram, newExcLog)
      (newProgram, newExcLog)
    }
  }

  def apply(
    pgm: Program,
    safeConfig: SafeConfig,
    config: DeobfuscateConfig
  ): Try[Program] = {
    // Run all of the deobfuscators until we reach a steady-state
    val (program, excLog) = deobfuscate(pgm, new ExcLog)

    // Report errors.
    if (excLog.hasError && !safeConfig.testMode && !safeConfig.silent) {
      println(program.relFileName + ":")
      println(excLog)
    }

    // Pretty print to file.
    config.outFile match {
      case Some(out) => {
        val ((fw, writer)) = Useful.fileNameToWriters(out)
        writer.write(program.toString(0))
        writer.close; fw.close
        println("Dumped deobfuscated AST to " + out)
      }
      case None => Try(program)
    }

    Success(program)
  }

  def defaultConfig: DeobfuscateConfig = DeobfuscateConfig()
  val options: List[PhaseOption[DeobfuscateConfig]] = List(
    ("silent", BoolOption(c => c.silent = true),
      "messages during deobfuscting AST are muted."),
    ("out", StrOption((c, s) => c.outFile = Some(s)),
      "the deobfuscated AST will be written to the outfile.")
  )
}

// Deobfuscate phase config
case class DeobfuscateConfig(
  var silent: Boolean = false,
  var outFile: Option[String] = None
) extends Config
