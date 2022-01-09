/*
 * scala-swing (https://www.scala-lang.org)
 *
 * Copyright EPFL, Lightbend, Inc., contributors
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package main.scala

import sun.tools.jconsole.LabeledComponent

import java.awt.Color
import java.text.SimpleDateFormat
import java.util.Date
import javax.swing.{Icon, ImageIcon, JButton}
import scala.swing.*
import scala.swing.event.*
import lofi.*
import lofi.Observable.*
import lofi.Types.*

import scala.language.postfixOps

object ComboBoxes extends SimpleSwingApplication {

  import TabbedPane._

  var observablesR: scala.collection.mutable.Map[String, Observable[Real]] = scala.collection.mutable.Map.empty[String, Observable[Real]]
  var observablesT: scala.collection.mutable.Map[String, Observable[Time]] = scala.collection.mutable.Map("Time" -> now)
  var observablesB: scala.collection.mutable.Map[String, Observable[Boolean]] = scala.collection.mutable.Map.empty[String, Observable[Boolean]]

  lazy val ui: FlowPanel = new FlowPanel {

    contents += new Tree

    contents += new Label("Name:")
    val label = new TextField(5) {
      editable = true
    }
    contents += label

    val patterns = List("Const", "Lookup", "Lift1r", "Lift2r")

    val unOpR = List("Negate", "Exp", "Log", "Sin", "Cos")
    val binOpR = List("Add", "Sub", "Mul", "Div", "Max", "Pow", "Avg")
    val comp = List("Leq", "Geq", "Lt", "Gt", "Eq", "Neq")
    val unOpB = List("Not", "Ever", "Always")
    val binOpB = List("And", "Or")

    contents += new Label("Constructor:")
    val constructors = new ComboBox(patterns) {
      makeEditable()
    }
    contents += constructors

    val contentsCopy = contents.clone

    reactions += {
      case SelectionChanged(`constructors`) => reformat()
    }

    listenTo(constructors.selection)

    def reformat(): Unit = {
      try {
        import Dialog._
        val constructor = constructors.selection.item
        constructor match {
          case "Const" =>
            contents.clear
            contents ++= contentsCopy
            val input = addLabeledTextfield("Value", 10)
            val button = new Button(Action("Create") {
              //centerOnScreen()
              val result = realToObservable(input.text.toDouble)
              observablesR += (label.text -> result)
              contents += new Label(s"Result: ${toTex(result)("t")}") //
            })
            button.verticalTextPosition = Alignment.Bottom
            contents += button
          case "Lookup" =>
            contents.clear
            contents ++= contentsCopy
            val input = addLabeledTextfield("Id", 10)
            val button = new Button(Action("Create") {
              //centerOnScreen()
            })
            button.verticalTextPosition = Alignment.Bottom
            contents += button
          case "Lift1r" =>
            contents.clear
            contents ++= contentsCopy
            val input1= addLabeledComboBox("Operator", unOpR)
            val input2 = addLabeledTextfield("Operand", 10)
            val button = new Button(Action("Create") {
              //centerOnScreen()
            })
            button.verticalTextPosition = Alignment.Bottom
            contents += button
          case "Lift2r" =>
            contents.clear
            contents ++= contentsCopy
            val input1 = addLabeledComboBox("Operator", binOpR)
            val input2 = addLabeledTextfield("Left Operand", 10)
            val input3 = addLabeledTextfield("Right Operand", 10)
            val button = new Button(Action("Create") {
              //centerOnScreen()
              val operator = input1.selection.item
              val lhs = input2.text
              val rhs = input3.text
              //observablesR += (label.text -> lift2r)
            })
            contents += button
          case "Reduce" => ???
          case "Comp" => ???
          case "Lift1b" => ???
          case "Lift2b" => ???
          case "If" => ???
          case "Freeze" => ???
          case "Expectation" => ???
        }
      } catch {
        case e: IllegalArgumentException =>
          //field.foreground = Color.red
          //field.text = "Error: " + e.getMessage
      }
    }

    def addLabeledTextfield(label: String, length: Int): TextField =
      contents += new Label(s"$label:")
      val field = new TextField(length) {
        editable = true
      }
      contents += field
      field

    def addLabeledComboBox(label: String, choices: List[String]): ComboBox[String] =
      contents += new Label(s"$label:")
      val comboBox = new ComboBox(choices) {
        makeEditable()
      }
      contents += comboBox
      comboBox
  }

  def top: Frame = new MainFrame {
    title = "ComboBoxes Demo"
    contents = ui
  }
}

