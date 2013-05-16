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

[![Build Status](https://travis-ci.org/Nuriaion/ScalaFxml.png?branch=master)](https://travis-ci.org/Nuriaion/ScalaFxml)
