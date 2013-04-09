import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.{Pos, Orientation}
import scalafx.scene.control.{Label, TextArea, Button, SplitPane}
import scalafx.scene.layout.{Priority, BorderPane, AnchorPane}
import scalafx.scene.Scene

trait SimpleFxml {
  println("SimpleFxml!")


  val theButton = new Button {
    id = "theButton"
    mnemonicParsing = false
    text = "Button"
    style = "-fx-background-color: green"
  }

  val theTop = new Label {
    id = "theTop"
    text = "Top!"
    style = "-fx-background-color: blue"
    BorderPane.setAlignment(this, Pos.CENTER)
  }

  val theBorderStuff = new BorderPane {
    id = "theBorderStuff"
    prefHeight = 400
    prefWidth = 600
    style = "-fx-background-color: black"

    AnchorPane.setBottomAnchor(this, 0)
    AnchorPane.setLeftAnchor(this, 0)
    AnchorPane.setRightAnchor(this, 0)
    AnchorPane.setTopAnchor(this, 0)
    center = theButton
    top = theTop
  }

  val rootPane = new AnchorPane {
    id = "topPane"
    prefHeight = 400
    prefWidth = 600
    content = List(theBorderStuff)
  }

}

object Simple extends JFXApp with SimpleFxml{
  println("First!")

  stage = new PrimaryStage {
    scene = new Scene(800,800) {
      root = rootPane

    }
  }
}

