/**
 * *****************************************************************************
 * Copyright (c) 2016-2017, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.phase

import scala.util.{ Try, Failure }
import kr.ac.kaist.safe.{ LINE_SEP, SafeConfig }
import kr.ac.kaist.safe.json.ASTD3Protocol._
import kr.ac.kaist.safe.parser.Parser
import kr.ac.kaist.safe.nodes.ast.{ ASTNode, Program }
import kr.ac.kaist.safe.util._
import kr.ac.kaist.safe.errors.error.NoFileError

import spray.json._

// Parse phase
case object Parse extends PhaseObj[Unit, ParseConfig, Program] {
  val name = "parser"
  val help = "Parses files." + LINE_SEP +
    "If multiple files are given, they are concatenated in the given order before being parsed."

  def apply(
    unit: Unit,
    safeConfig: SafeConfig,
    config: ParseConfig
  ): Try[Program] = safeConfig.fileNames match {
    case Nil => Failure(NoFileError("parse"))
    case _ => Parser.fileToAST(safeConfig.fileNames).map {
      case (program, excLog) => {
        // Report errors.
        if (excLog.hasError) {
          println(program.relFileName + ":")
          println(excLog)
        }

        // Pretty print to file.
        config.outFile match {
          case Some(out) => {
            val (fw, writer) = Useful.fileNameToWriters(out)
            if (config.toD3Json) {
              // Print JSON
              val json = program.asInstanceOf[ASTNode].toJson
              writer.write(json.toString)
              println("Dumped parsed JavaScript code as JSON to " + out)
            } else {
              // Print JavaScript
              writer.write(program.toString(0))
              println("Dumped parsed JavaScript code to " + out)
            }
            writer.close; fw.close
          }
          case None =>
        }

        program
      }
    }
  }

  def defaultConfig: ParseConfig = ParseConfig()
  val options: List[PhaseOption[ParseConfig]] = List(
    ("out", StrOption((c, s) => c.outFile = Some(s)),
      "the parsed JavaScript code will be written to the outfile."),
    ("d3Json", BoolOption(c => c.toD3Json = true),
      "write the output as a D3 JSON file.")
  )
}

// Parse phase config
case class ParseConfig(
  var outFile: Option[String] = None,
  var toD3Json: Boolean = false
) extends Config
