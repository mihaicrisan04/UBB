package com.example.view.gui;

import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.VBox;
import javafx.geometry.Insets;
import javafx.stage.Stage;
import java.util.List;
import java.util.function.Consumer;

public class ProgramSelectionView {
    private Stage stage;
    private ListView<String> programList;
    private Button selectButton;

    public ProgramSelectionView() {
        stage = new Stage();
        initializeComponents();
    }

    private void initializeComponents() {
        VBox root = new VBox(10);
        root.setPadding(new Insets(10));
        
        programList = new ListView<>();
        selectButton = new Button("Select Program");
        
        root.getChildren().addAll(
            new Label("Select a program to execute:"),
            programList,
            selectButton
        );
        
        Scene scene = new Scene(root, 800, 500);
        stage.setTitle("Program Selection");
        stage.setScene(scene);
    }

    public void show() {
        stage.show();
    }

    public void setPrograms(List<String> programs) {
        programList.getItems().addAll(programs);
    }

    public void setOnProgramSelect(Consumer<String> handler) {
        selectButton.setOnAction(e -> {
            String selected = programList.getSelectionModel().getSelectedItem();
            if (selected != null) {
                stage.close();
                handler.accept(selected);
            }
        });
    }
}