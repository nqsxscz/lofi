package com.example

import javax.servlet.annotation.WebServlet
import vaadin.scala._
import vaadin.scala.server.ScaladinServlet

@WebServlet(urlPatterns = Array("/*"))
class Servlet extends ScaladinServlet(
  ui = classOf[HelloWorldUI]
)

class MyUI extends UI(theme = ValoTheme.ThemeName) {

  content = new VerticalLayout { layout =>
    margin = true
    spacing = true

    addComponent(new Label {
      value = "Hello World!"
      styleNames += ValoTheme.LabelH1
    })

    addComponent(Button("Click Me", { e =>
      layout.addComponent(Label("Thanks for clicking!"))
    }))
  }
}
