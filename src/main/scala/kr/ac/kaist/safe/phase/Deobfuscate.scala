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
    val stringDecoder = new StringDecoder(newProgram)
    newProgram = stringDecoder.result
    newExcLog += stringDecoder.excLog
    if (newExcLog.hasError) return (newProgram, newExcLog)

    // Fold constants
    val constantFolder = new ConstantFolder(newProgram)
    newProgram = constantFolder.result
    newExcLog += constantFolder.excLog
    if (newExcLog.hasError) return (newProgram, newExcLog)

    // Propagate constants
    val constantPropagator = new ConstantPropagator(newProgram)
    newProgram = constantPropagator.result
    newExcLog += constantPropagator.excLog
    if (newExcLog.hasError) return (newProgram, newExcLog)

    // Inline functions
    val functionInliner = new FunctionInliner(newProgram)
    newProgram = functionInliner.result
    newExcLog += functionInliner.excLog
    if (newExcLog.hasError) return (newProgram, newExcLog)

    // Simplify known functions
    val knownFunctionSimplifier = new KnownFunctionSimplifier(newProgram)
    newProgram = knownFunctionSimplifier.result
    newExcLog += knownFunctionSimplifier.excLog
    if (newExcLog.hasError) return (newProgram, newExcLog)

    // Remove dead branches
    val deadBranchRemover = new DeadBranchRemover(newProgram)
    newProgram = deadBranchRemover.result
    newExcLog += deadBranchRemover.excLog
    if (newExcLog.hasError) return (newProgram, newExcLog)

    // Remove unused variables
    val unusedVariableRemover = new UnusedVariableRemover(newProgram)
    newProgram = unusedVariableRemover.result
    newExcLog += unusedVariableRemover.excLog
    if (newExcLog.hasError) return (newProgram, newExcLog)

    // Simplify
    newProgram = NU.SimplifyWalker.walk(newProgram)

    if (newExcLog.hasError || newProgram =~ program) {
      (newProgram, newExcLog)
    } else {
      deobfuscate(newProgram, newExcLog)
    }
  }

  def apply(
    pgm: Program,
    safeConfig: SafeConfig,
    config: DeobfuscateConfig
  ): Try[Program] = {
    // Run all of the deobfuscators until we reach a steady-state
    var (program, excLog) = deobfuscate(pgm, new ExcLog)

    // Rename variables
    val variableRenamer = new VariableRenamer(program)
    program = variableRenamer.result
    excLog += variableRenamer.excLog

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
        writer.close
        fw.close
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
