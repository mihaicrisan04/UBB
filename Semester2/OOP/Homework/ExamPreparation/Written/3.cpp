#include <iostream>
#include <vector>
#include <string>
#include <stdexcept>

#include <iostream>
#include <vector>
#include <string>

// Abstract Action class
class Action {
public:
    virtual ~Action() = default;
    virtual void execute() const = 0;
};

// CreateAction class
class CreateAction : public Action {
public:
    void execute() const override {
        std::cout << "Create file" << std::endl;
    }
};

// ExitAction class
class ExitAction : public Action {
public:
    void execute() const override {
        std::cout << "Exit application" << std::endl;
    }
};

class MenuItem {
private:
    std::string text;
    Action* action;

public:
    MenuItem(const std::string& text, Action* action = nullptr)
        : text(text), action(action) {}

    virtual ~MenuItem() = default;

    virtual void print() const {
        std::cout << text << std::endl;
    }

    virtual void clicked() const {
        std::cout << text << std::endl;
        if (action) {
            action->execute();
        }
    }
};

class Menu : public MenuItem {
private:
    std::vector<MenuItem*> items;

public:
    Menu(const std::string& text)
        : MenuItem(text) {}

    void addMenuItem(MenuItem* item) {
        items.push_back(item);
    }

    void print() const override {
        MenuItem::print();
        for (const auto& item : items) {
            item->print();
        }
    }

    void clicked() const override {
        MenuItem::clicked();
        // Optionally, you can implement the logic to handle submenu clicks
    }
};

class MenuBar {
private:
    std::vector<Menu*> menus;

public:
    ~MenuBar() {
        for (auto& menu : menus) {
            delete menu;
        }
    }

    void addMenu(Menu* menu) {
        menus.push_back(menu);
    }

    void print() const {
        for (const auto& menu : menus) {
            menu->print();
        }
    }

    void clicked() const {
        for (const auto& menu : menus) {
            menu->clicked();
        }
    }
};
int main() {
    // Create actions
    CreateAction* createAction = new CreateAction();
    ExitAction* exitAction = new ExitAction();

    // Create menu items
    MenuItem* textItem = new MenuItem("Text", new CreateAction());
    MenuItem* cppItem = new MenuItem("C++", new CreateAction());
    MenuItem* exitItem = new MenuItem("Exit", new ExitAction());

    // Create submenus
    Menu* newMenu = new Menu("New");
    newMenu->addMenuItem(textItem);
    newMenu->addMenuItem(cppItem);

    Menu* fileMenu = new Menu("File");
    fileMenu->addMenuItem(newMenu);
    fileMenu->addMenuItem(exitItem);

    Menu* aboutMenu = new Menu("About");

    // Create menu bar
    MenuBar menuBar;
    menuBar.addMenu(fileMenu);
    menuBar.addMenu(aboutMenu);

    // Simulate clicks: File -> New -> C++ and then Exit
    std::cout << "Simulating clicks:" << std::endl;
    fileMenu->clicked();
    newMenu->clicked();
    cppItem->clicked();
    exitItem->clicked();

    // Cleanup
    delete createAction;
    delete exitAction;
    delete textItem;
    delete cppItem;
    delete exitItem;
    delete newMenu;
    delete fileMenu;
    delete aboutMenu;

    return 0;
}
