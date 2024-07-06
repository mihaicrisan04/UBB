#pragma once

#include "model.hpp"

#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>


class Repository {
public:
    std::vector<Item> items;
    std::vector<User> users;

    Repository() {
        loadItems();
        loadUsers();
    }

    ~Repository() {
        saveItems();
        saveUsers();
    }

    void addItem(Item item) { items.push_back(item); }

    void addUser(User user) { users.push_back(user); }   

    std::vector<Item> getItems() { return items; }

    std::vector<User> getUsers() { return users; }

    void loadItems() {
        std::ifstream file("../items.txt");
        std::string line;   

        while (std::getline(file, line)) {
            std::stringstream ss(line);
            std::string name, category;
            std::string price_str;
            int price;
            std::string offers_str;
            std::vector<std::tuple<int, std::string, int>> offers;

            std::getline(ss, name, '|');
            std::getline(ss, category, '|');
            std::getline(ss, price_str, '|');
            price = std::stoi(price_str);
            std::getline(ss, offers_str, '|');  

            std::stringstream ss2(offers_str);  
            std::string userId_str, date, priceOffer_str;
            int userId, priceOffer;

            while (std::getline(ss2, userId_str, ',')) {
                std::getline(ss2, date, ',');
                std::getline(ss2, priceOffer_str, '"');
                userId = std::stoi(userId_str);
                priceOffer = std::stoi(priceOffer_str);
                offers.push_back({userId, date, priceOffer});
            }

            items.push_back(Item(name, category, price, offers)); 
        }

        file.close();
    }

    void loadUsers() {
        std::ifstream file("../users.txt");
        std::string line;
        
        while (std::getline(file, line)) {
            std::stringstream ss(line);
            std::string name, id_str, type;
            int id;

            std::getline(ss, name, '|');
            std::getline(ss, id_str, '|');
            std::getline(ss, type, '|');
            id = std::stoi(id_str);

            users.push_back(User(name, id, type)); 
        }

        file.close();
    }

    void saveItems() {
        std::ofstream file("../items.txt");

        for (auto item : items) {
            file << item.name << "|" << item.category << "|" << item.price << "|";
            for (auto offer : item.offers) {
                file << std::get<0>(offer) << "," << std::get<1>(offer) << "," << std::get<2>(offer) << "\"";
            }
            file << std::endl;
        }

        file.close();
    }

    void saveUsers() {
        std::ofstream file("../users.txt");

        for (auto user : users) {
            file << user.name << "|" << user.id << "|" << user.type << std::endl;
        }

        file.close();
    }   
};