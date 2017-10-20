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
import kr.ac.kaist.safe.deobfuscator.{ StringDecoder }
import kr.ac.kaist.safe.nodes.ast.Program
import kr.ac.kaist.safe.util.{ NodeUtil => NU }
import kr.ac.kaist.safe.util._

// Deobfuscate phase
case object Deobfuscate extends PhaseObj[Program, DeobfuscateConfig, Program] {
  val name: String = "deobfuscator"
  val help: String =
    "Deobfuscates JavaScript malware (string decoder)"

  def apply(
    pgm: Program,
    safeConfig: SafeConfig,
    config: DeobfuscateConfig
  ): Try[Program] = {
    var program = pgm
    var excLog = new ExcLog

    // decode strings
    val stringDecoder = new StringDecoder(program)
    program = stringDecoder.result
    excLog += stringDecoder.excLog

    // Simplify
    program = NU.SimplifyWalker.walk(program)

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
