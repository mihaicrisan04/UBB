package com.example;

import javafx.application.Application;
import javafx.stage.Stage;

import com.example.view.gui.ProgramSelectionView;
import com.example.view.gui.MainProgramView;
import com.example.controller.Programs;

public class App extends Application {
    private Programs programs = new Programs();

    @Override
    public void start(Stage primaryStage) {
        ProgramSelectionView selectionView = new ProgramSelectionView();
        selectionView.setPrograms(programs.getProgramStrings());
        selectionView.setOnProgramSelect(this::showMainProgramWindow);
        selectionView.show();
    }

    private void showMainProgramWindow(String selectedProgram) {
        MainProgramView mainView = new MainProgramView();
        mainView.showMainProgramWindow(selectedProgram);
    }

    public static void main(String[] args) {
        launch(args);
    }
}