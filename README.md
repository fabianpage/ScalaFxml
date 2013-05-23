ScalaFxml
=========

Generate a Scala Trait from a Fxml file.

Gui.fxml:
```xml
  <AnchorPane fx:id="anchorPane1">
		<children>
			<Label text="Huhu"/>
		</children>
	</AnchorPane>
```

Gui.scala:
```scala
	trait Gui {
		val anchorPane1 = new AnchorPane() {
			children = List(
				label_2
			)
		}
		val labe_2 = new Label() {
			text = "Huhu"
		}
	}
```

Example at: https://github.com/Nuriaion/ScalaFxml-example

[![Build Status](https://travis-ci.org/Nuriaion/ScalaFxml.png?branch=master)](https://travis-ci.org/Nuriaion/ScalaFxml)

This is a very early prototype witch probably only works with the fxml file of the exmaple!
