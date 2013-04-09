# FxmlToScalaFx Prototype

## Aims

- Try out if a generation of scalafx traits from an FXML file is possible

## Don't

- "Final stuff"

## Idea

- From an FXML file a scala trait should be genearatet.

Gui.fxml:

	<AnchorPane fx:id="anchorPane1">
		<children>
			<Label text="Huhu"/>
		</children>
	</AnchorPane>


Gui.scala:

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

## to do

## Done