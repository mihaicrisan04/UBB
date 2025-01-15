package com.example;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;
import javafx.scene.control.*;
import javafx.scene.layout.VBox;
import javafx.geometry.Insets;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;

import com.example.collections.dictionary.MyDictionary;
import com.example.collections.heap.MyIHeap;
import com.example.controller.Controller;
import com.example.repository.Repository;
import com.example.model.PrgState;
import com.example.controller.Programs;
import com.example.model.statements.IStmt;
import com.example.model.values.Value;
import com.example.model.exceptions.MyException;
import com.example.collections.list.MyIList;

import java.util.stream.Collectors;

public class App extends Application {
    private Controller controller;
    private PrgState prgState;
    private MyIList<PrgState> prgList;
    private Programs programs = new Programs();

    private TableView<HeapEntry> heapTable;
    private ListView<String> outputList;
    private ListView<String> fileTableList;
    private ListView<String> prgStateList;
    private TableView<SymTableEntry> symTable;
    private ListView<String> exeStackList;
    private TextField prgStatesCount;
    
    @Override
    public void start(Stage primaryStage) {
        // First window - Program selection
        Stage selectionStage = new Stage();
        VBox selectionRoot = new VBox(10);
        selectionRoot.setPadding(new Insets(10));
        
        ListView<String> programList = new ListView<>();
        programList.getItems().addAll(programs.getProgramStrings());
        
        Button selectButton = new Button("Select Program");
        selectButton.setOnAction(e -> {
            String selected = programList.getSelectionModel().getSelectedItem();
            if (selected != null) {
                selectionStage.close();
                showMainProgramWindow(selected);
            }
        });
        
        selectionRoot.getChildren().addAll(
            new Label("Select a program to execute:"),
            programList,
            selectButton
        );
        
        Scene selectionScene = new Scene(selectionRoot, 800, 500);
        selectionStage.setTitle("Program Selection");
        selectionStage.setScene(selectionScene);
        selectionStage.show();
    }
    
    private void showMainProgramWindow(String selectedProgram) {
        Stage mainStage = new Stage();
        BorderPane mainRoot = new BorderPane();
        
        IStmt prg = programs.getProgramFromString(selectedProgram);
        try {
            prg.typeCheck(new MyDictionary<>());
            prgState = new PrgState(prg);
            Repository repo = new Repository(programs.getProgramLogFile(selectedProgram));
            repo.addProgram(prgState);
            controller = new Controller(repo);
            controller.executor = java.util.concurrent.Executors.newFixedThreadPool(2);

            // Create all the required components
            createComponents();
            
            // Layout setup
            VBox leftPane = new VBox(10);
            leftPane.setPadding(new Insets(10));
            leftPane.getChildren().addAll(
                new Label("Number of PrgStates:"),
                prgStatesCount,
                new Label("HeapTable:"),
                heapTable,
                new Label("Output:"),
                outputList
            );
            
            VBox centerPane = new VBox(10);
            centerPane.setPadding(new Insets(10));
            centerPane.getChildren().addAll(
                new Label("FileTable:"),
                fileTableList,
                new Label("PrgState IDs:"),
                prgStateList
            );
            
            VBox rightPane = new VBox(10);
            rightPane.setPadding(new Insets(10));
            rightPane.getChildren().addAll(
                new Label("SymTable:"),
                symTable,
                new Label("ExeStack:"),
                exeStackList
            );
            
            Button runOneStepButton = new Button("Run one step");
            runOneStepButton.setOnAction(e -> runOneStep());
            
            mainRoot.setLeft(leftPane);
            mainRoot.setCenter(centerPane);
            mainRoot.setRight(rightPane);
            mainRoot.setBottom(runOneStepButton);
            
            Scene mainScene = new Scene(mainRoot, 1200, 800);
            mainStage.setTitle("Program Execution");
            mainStage.setScene(mainScene);

            mainStage.setOnCloseRequest(e -> {
                if (controller != null && controller.executor != null) {
                    controller.executor.shutdownNow();
                }
                System.exit(0);
            });

            // Only run first step if initialization was successful
            updateUIComponents(); // Show initial state
        } catch (MyException e) {
            Alert alert = new Alert(Alert.AlertType.ERROR);
            alert.setTitle("Type Check Error");
            alert.setContentText(e.getMessage());
            alert.showAndWait();
            mainStage.close();
            return;
        }
        
        mainStage.show();
    }
    
    @SuppressWarnings("unchecked")
    private void createComponents() {
        // Initialize all UI components
        prgStatesCount = new TextField();
        prgStatesCount.setEditable(false);
        
        heapTable = new TableView<>();
        TableColumn<HeapEntry, Integer> addressCol = new TableColumn<>("Address");
        TableColumn<HeapEntry, String> valueCol = new TableColumn<>("Value");
        addressCol.setCellValueFactory(cellData -> new SimpleObjectProperty<>(cellData.getValue().getAddress()));
        valueCol.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().getValue()));
        heapTable.getColumns().addAll(addressCol, valueCol);
        
        outputList = new ListView<>();
        fileTableList = new ListView<>();
        prgStateList = new ListView<>();
        
        symTable = new TableView<>();
        TableColumn<SymTableEntry, String> varNameCol = new TableColumn<>("Variable");
        TableColumn<SymTableEntry, String> varValueCol = new TableColumn<>("Value");
        varNameCol.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().getVarName()));
        varValueCol.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().getValue()));
        symTable.getColumns().addAll(varNameCol, varValueCol);
        
        exeStackList = new ListView<>();
        
        // Add listeners for PrgState selection
        prgStateList.getSelectionModel().selectedItemProperty().addListener((obs, oldVal, newVal) -> {
            if (newVal != null) {
                MyIList<PrgState> programStates = controller.getRepo().getProgramList();
                programStates.stream()
                    .filter(p -> String.valueOf(p.getId()).equals(newVal))
                    .findFirst()
                    .ifPresent(prg -> {
                        // Update SymTable for selected program
                        symTable.getItems().setAll(prg.getSymTable().entrySet().stream()
                            .map(entry -> new SymTableEntry(entry.getKey(), entry.getValue().toString()))
                            .collect(Collectors.toList()));

                        // Update ExeStack for selected program
                        exeStackList.getItems().setAll(
                            prg.getExeStack().stream()
                                .map(IStmt::toString)
                                .collect(Collectors.toList())
                        );
                    });
            }
        });
    }
    
    private void runOneStep() {
        if (controller.getRepo().getProgramList().size() == 0) {
            Alert alert = new Alert(Alert.AlertType.INFORMATION);
            alert.setTitle("Program completed");
            alert.setContentText("Program execution finished");
            alert.showAndWait();
            controller.executor.shutdownNow();
            return;
        }

        try {
            prgList = controller.removeCompletedPrg(controller.getRepo().getProgramList());
            synchronized (controller.getRepo()) {
                // Garbage collector
                MyIHeap<Integer, Value> heap = controller.getRepo().getProgramList().get(0).getHeap();
                heap = controller.conservativeGarbageCollector(controller.getRepo().getProgramList(), heap);
                for (PrgState prg: controller.getRepo().getProgramList()) { prg.setHeap(heap); }
            }
            controller.oneStepForAllPrg(prgList);
            prgList = controller.removeCompletedPrg(controller.getRepo().getProgramList());

            updateUIComponents();
        } catch (Exception e) {
            Alert alert = new Alert(Alert.AlertType.ERROR);
            alert.setTitle("Error");
            alert.setContentText(e.getMessage());
            alert.showAndWait();
        }
    }
    
    private void updateUIComponents() {
        // Update number of program states
        prgStatesCount.setText(String.valueOf(controller.getRepo().getProgramList().size()));
        
        // Get current program state
        MyIList<PrgState> programStates = controller.getRepo().getProgramList();
        if (programStates.size() == 0) return;
        PrgState currentPrg = programStates.get(0);

        // Update HeapTable
        heapTable.getItems().setAll(currentPrg.getHeap().entrySet().stream()
            .map(entry -> new HeapEntry(entry.getKey(), entry.getValue().toString()))
            .collect(Collectors.toList()));

        // Update Output
        outputList.getItems().setAll(currentPrg.getOut().stream()
            .map(Value::toString)
            .collect(Collectors.toList()));

        // Update FileTable
        fileTableList.getItems().setAll(currentPrg.getFileTable().entrySet().stream()
            .map(entry -> entry.getKey().toString() + " -> " + entry.getValue().toString())
            .collect(Collectors.toList()));

        // Store current selection
        String selectedPrgState = prgStateList.getSelectionModel().getSelectedItem();

        // Update PrgState IDs list
        prgStateList.getItems().setAll(programStates.stream()
            .map(prg -> String.valueOf(prg.getId()))
            .collect(Collectors.toList()));

        // Restore selection if it still exists
        if (selectedPrgState != null && prgStateList.getItems().contains(selectedPrgState)) {
            prgStateList.getSelectionModel().select(selectedPrgState);
        } else {
            // If selected program no longer exists, select the first one
            prgStateList.getSelectionModel().selectFirst();
        }

        // Update SymTable
        symTable.getItems().setAll(currentPrg.getSymTable().entrySet().stream()
            .map(entry -> new SymTableEntry(entry.getKey(), entry.getValue().toString()))
            .collect(Collectors.toList()));

        // Update ExeStack
        exeStackList.getItems().setAll(
            currentPrg.getExeStack().stream()
                .map(IStmt::toString)
                .collect(Collectors.toList())
        );
    }
    
    private void updateSymTableAndExeStack(String prgStateId) {
        MyIList<PrgState> programStates = controller.getRepo().getProgramList();
        programStates.stream()
            .filter(p -> String.valueOf(p.getId()).equals(prgStateId))
            .findFirst()
            .ifPresent(prg -> {
                // Update SymTable for selected program
                symTable.getItems().setAll(prg.getSymTable().entrySet().stream()
                    .map(entry -> new SymTableEntry(entry.getKey(), entry.getValue().toString()))
                    .collect(Collectors.toList()));

                // Update ExeStack for selected program
                exeStackList.getItems().setAll(
                    prg.getExeStack().stream()
                        .map(IStmt::toString)
                        .collect(Collectors.toList())
                );
            });
    }
    
    public static void main(String[] args) {
        launch(args);
    }
}

// Helper classes for TableViews
class HeapEntry {
    private Integer address;
    private String value;

    public HeapEntry(Integer address, String value) {
        this.address = address;
        this.value = value;
    }

    public Integer getAddress() { return address; }
    public String getValue() { return value; }
}

class SymTableEntry {
    private String varName;
    private String value;

    public SymTableEntry(String varName, String value) {
        this.varName = varName;
        this.value = value;
    }

    public String getVarName() { return varName; }
    public String getValue() { return value; }
}
